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
    room,
    login_time = null}).


start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #state{ user="", room=""}}.

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
            select(Parsed, Pid, State),
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
handle_cast({set_user, Username}, State) ->
    {noreply, State#state{user = Username}};
handle_cast({set_room, Room}, State) ->
    {noreply, State#state{room = Room}};
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
%% TODO: Refactor to allow for dynamics args list if possible
%% LOGIN MANAGEMENT---------------------------------------------------
select({"LOGIN", Data}, ServerPid, State) ->
    gen_server:call(chat_controller, {login, Data, ServerPid});
select({"LOGOUT", Data}, ServerPid, State) ->
    gen_server:call(chat_controller, {logout, Data, ServerPid});
select({"WHOAMI", Data}, ServerPid, State) -> controller({who, ServerPid, State#state.user}); %% which user i'm logged in as
select({"WHEREAMI", Data}, ServerPid, State) -> controller({where, ServerPid, State#state.room}); %% what room i'm in
%% ROOM MANAGEMENT----------------------------------------------------
select({"LISTROOM", _}, ServerPid, State) -> controller({listroom, ServerPid});
select({"NEWROOM", Data}, ServerPid, State) -> controller({newroom, Data, ServerPid, State#state.user});
select({"DELROOM", Data}, ServerPid, State) -> controller({delroom, Data, ServerPid, State#state.user});
%% TODO: change user and room state update to be handled by tcp_server
select({"JOINROOM", Data}, ServerPid, State) -> controller({joinroom, Data, ServerPid, State#state.user});
select({"EXITROOM", Data}, ServerPid, State) -> controller({exitroom, Data, ServerPid, State#state.user});
select({"SETPRIVATE", Data}, ServerPid, State) -> controller({joinroom, Data, ServerPid, State#state.user});
select({"SETPUBLIC", Data}, ServerPid, State) -> controller({joinroom, Data, ServerPid, State#state.user}).
%% SEND MESSAGE-------------------------------------------------------

select({"SAY", Data}, ServerPid) ->
    ok.
%%--------------------------------------------------------------------
%% SELECTION CONTROLLER
controller(Message) -> gen_server:cast(chat_controller, Message).
