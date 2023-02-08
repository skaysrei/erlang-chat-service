%%%-------------------------------------------------------------------
%% @doc 
%% translation layer worker;
%% receives the raw message from the client and parses it,
%% sends the appropriate reques to the chat_controller.
%% @end
%%%-------------------------------------------------------------------
-module(translation_layer).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% record is a shitty compiler hack why am i using it instead of a map??
-record(state, {
    user,
    login_time = null}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

%% if no user is passed try logging in
%% TODO: refactor case constructs w pattern matching functions (separate with ;)
%% TODO: check against state if user is logged in
handle_call({message, Msg}, _From, State) ->
    %% replace with BIF (built in function) 'element(1, _From)'
    {Pid, ReplyTag} = _From,
    %% extracts the [command, message] from raw data
    Parsed = parse_message(Msg),
    case Parsed of
        {Command, Content} ->
            select(Parsed, Pid, State#state.user),
            {reply, ok, State};
        protocol_error ->
            {reply, {error, "Err: Wrong protocol syntax OwO"}, State};
        unknown_error ->
            {reply, {error, "Err: Unknown error CwC"}, State};
        _ ->
            {reply, error, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% sets the Username on succesful login
handle_cast({login, Username}, State) ->
    {noreply, State#state{user = Username}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% TODO Set return values into state inside handle_call
parse_message(Msg) ->
    case lists:member($:, Msg) of
        false ->
            protocol_error;
        true -> 
            {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Msg),
            io:format("\n\nParsed command: C[~p] M[~p]", [Command, Content]),
            {Command, Content};
        _ ->
            unknown_error
    end.

quit(Command, Msg) ->
    ok.

%% TODO: change from pid to name, CHANGE TO gen_server:cast()
%% LOGIN MANAGEMENT---------------------------------------------------
select({"LOGIN", Data}, ServerPid, User) ->
    gen_server:call(chat_controller, {login, Data, ServerPid});
select({"LOGOUT", Data}, ServerPid, User) ->
    gen_server:call(chat_controller, {logout, Data, ServerPid});
%% ROOM MANAGEMENT----------------------------------------------------
select({"LISTROOM", _}, ServerPid, User) -> controller({listroom, ServerPid});
select({"NEWROOM", Data}, ServerPid, User) -> controller({newroom, Data, ServerPid, User});
select({"DELROOM", Data}, ServerPid, User) -> controller({delroom, Data, ServerPid, User});
select({"JOINROOM", Data}, ServerPid, User) -> controller({joinroom, Data, ServerPid, User});
select({"EXITROOM", Data}, ServerPid, User) -> controller({exitroom, Data, ServerPid, User}).

%% Private message
select({"SAY", Data}, ServerPid) ->
    ok.

controller(Message) -> gen_server:cast(chat_controller, Message).