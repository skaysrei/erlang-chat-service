%%%-------------------------------------------------------------------
%% @doc 
%% the brains of the whole operation.
%% receives a set of commands from the translatiob layers,
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

%% state#loggedUsers -> Key(tcp_server PID) => Val(Username)
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
handle_call({login, User, UserPid}, _From, State) ->
    io:format("\n\n[LOGIN] request received by chat_controller: USR[ ~p ] PID[ ~p ]~n~n", [User, UserPid]),
    %% checks if the user pid is stored in the state (user logged in)
    NewState = user_action({login, UserPid, User}, State),
    {reply, logged_in, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% handles initial client connection synchronously,
%% checks if the username is taken and updates the
%% Users state accordingly.
% handle_call() ->
%     Response = case dict:is_key(UserName, Users) of
%         true ->
%             NewUsers = Users,
%             username_taken;
%         false ->
%             NewUsers = dict:append(UserName, Socket, Users)
%     {}.

%% handle_call is asynchronous
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
update_user() ->
    ok.

%% these check/update the user state
user_action({login, UserPid, User}, State) ->
    case dict:is_key(UserPid, State#state.loggedUsers) of
        true ->
            State;
        false ->
            %% TODO: Add Username check
            gen_server:cast(UserPid, login),
            dict:append(UserPid, User, State#state.loggedUsers);
        _ ->
            State
    end;
user_action({logout, UserPid, User}, State) ->
    case dict:is_key(UserPid, State#state.loggedUsers) of
        true ->
            gen_server:cast(UserPid, logout),
            dict:erase(UserPid, State#state.loggedUsers);
        false ->
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

%% Sends message to user
dispatch(UsrPid, Message, From) ->
    gen_server:cast(UsrPid, Message).

broadcast(UsrList, Message, From) ->
    lists:foreach(idk, UsrList).

