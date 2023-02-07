%%%-------------------------------------------------------------------
%% @doc 
%% the brains of the whole operation;
%% receives a set of commands from the translation_layers,
%% broadcasts the messages to the appropriate recipients.
%% @end
%%
%% PS:
%% https://erlangforums.com/t/what-happened-to-map-access-syntax/798
%% Apparently they cant implement 'get-map' syntax like wtf???
%% it's the records fault all over again this is so dumb xd
%%%-------------------------------------------------------------------
-module(chat_controller).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% state#loggedUsers -> Key(Username) => Val(tcp_server PID)
%% activeGroups# -> Key(GroupName) => Val([Owner, [ActiveMembers], [Visibility]])
-record(state, { %% this is only the record definition! needs to be created later
    loggedUsers,
    activeGroups
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init(_Args) ->
    io:format("Starting chat_controller...~n~n"),
    State = #state{
        loggedUsers = dict:new(),
        activeGroups = dict:new()
    },
    {ok, State}.


%% args:(message, module, loopdata)
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({message, Msg}, _From, State) ->
    io:format("\n\nCall receiver by chat_controller: MSG[ ~p ]~n~n", [Msg]),
    {reply, {}, State};
%% LOGIN %%
handle_call({login, User, ServerPid}, _From, State) ->
    io:format("\n\n[LOGIN] request received by chat_controller: USR[ ~p ] PID[ ~p ]~n~n", [User, ServerPid]),
    %% checks if the user pid is stored in the state (user logged in)
    NewState = user_action({login, ServerPid, element(1, _From), User}, State),
    {reply, logged_in, NewState};
%% LOGOUT %%
handle_call({logout, User, ServerPid}, _From, State) ->
    io:format("\n\n[LOGOUT] request received by chat_controller: PID[ ~p ]~n~n", [ServerPid]),
    %% checks if the user pid is stored in the state (user logged in)
    NewState = user_action({logout, ServerPid, User}, State),
    {reply, logged_in, NewState};
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

%% INTERNAL FUNCTIONS-------------------------------------------------------------------
%% PATTERN MATCHING GALORE! -> Refactoring all those nasty 'case' constructs

%% TODO: helper function to extract state REFACTORR
update_user_state() ->
    ok.

%% these check/update the user state
user_action({login, UserPid, TrsPid, User}, State) ->
    case dict:is_key(User, State#state.loggedUsers) of
        true ->
            %% already logged in
            gen_server:cast(UserPid, {message, "Already logged in as [" ++ User ++ "]."}),
            State;
        false ->
            %% TODO: Add multiple login check;
            %% one tcp client can currently login multiple users per session
            gen_server:cast(TrsPid, {login, User}),
            gen_server:cast(UserPid, login),
            NewState = #state{
                loggedUsers = dict:append(User, UserPid, State#state.loggedUsers),
                activeGroups = State#state.activeGroups
            };
        _ ->
            State
    end;
user_action({logout, UserPid, User}, State) ->
    case dict:is_key(User, State#state.loggedUsers) of
        true ->
            gen_server:cast(UserPid, logout),
            NewState = #state{
                loggedUsers = dict:erase(User, State#state.loggedUsers),
                activeGroups = State#state.activeGroups
            };
        false ->
            %% not logged in
            gen_server:cast(UserPid, {message, "You are not logged in."}),
            State;
        _ ->
            State
    end.

%% TODO: helper function to extract state REFACTORR
update_room() ->
    ok.

% these check/update the room state
room_action({add_room, Owner, Visibility}, State) ->
    ok;
room_action({rem_room, Owner, Visibility}, State) ->
    ok;
room_action({join, User}, State) ->
    ok;
room_action({exit, User}, State) ->
    ok;
room_action({invite_private, User}, State) ->
    ok.

%% MESSAGE ROUTING FUNCTIONS------------------------------------------------------------

broadcast(UsrList, Message, From) ->
    lists:foreach(idk, UsrList).

