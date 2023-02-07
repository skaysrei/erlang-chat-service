%%%-------------------------------------------------------------------
%% @doc 
%% translation layer worker.
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
handle_call({message, Msg}, _From, State) ->
    {Pid, ReplyTag} = _From,
    %% extracts the [command, message] from raw data
    Parsed = parse_message(Msg),
    case Parsed of
        {Command, Content} ->
            io:format("\nTranslation layer (Parsed data):\n\nCommand[ ~s ]\nMessage[ ~s ]\n\n", [Command, Content]),
            %%  gen_server:cast(chat_controller, PID tcp worker, command/msg)
            command_select({Command, Content} = Parsed, Pid),
            {reply, ok, State};
        protocol_error ->
            {reply, {error, "Err: Wrong protocol syntax OwO"}, State};
        unknown_error ->
            {reply, {error, "Err: Unknown error CwC"}, State};
        _ ->
            {reply, error, State}
    end;



    % case Command of
    %     "LOGIN" ->
    %         try_login(Content);
    %     _ ->
    %         {reply, {error, "Err: Unknown command UwU"}, State}
    % end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
            io:format("\n\nCONTENT: ~p", [Content]),
            {Command, Content};
        _ ->
            unknown_error
    end.

quit(Command, Msg) ->
    ok.

%% TODO: change from pid to name
command_select({"LOGIN", User}, ServerPid) ->
    io:format("\n\nCOMMAND SELECTOR REACHED~n~n", []),
    gen_server:call(chat_controller, {login, User, ServerPid}),
    ok;
command_select(Command, Msg) ->
    ok;
command_select(Command, Msg) ->
    ok;
command_select(Command, Msg) ->
    ok.

try_login(User) ->
    gen_server:call({login, User}).