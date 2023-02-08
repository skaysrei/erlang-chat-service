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
%% activeRooms# -> Key(GroupName) => Val({Owner, ActiveMembers, Visibility})
-record(state, { %% this is only the record definition! needs to be created later
    loggedUsers,
    activeRooms
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init(_Args) ->
    io:format("Starting chat_controller...~n~n"),
    State = #state{
        loggedUsers = dict:new(),
        activeRooms = dict:new()
    },
    {ok, State}.


%% args:(message, module, loopdata)
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

%% TODO: Change login calls to casts
%% HANDLERS-----------------------------------------------------------------------------
%% LOGIN %%
handle_call({login, User, ServerPid}, _From, State) ->
    io:format("\n\n[LOGIN] request received by chat_controller: USR[ ~p ] ServerPID[ ~p ]", [User, ServerPid]),
    {reply, logged_in, NewState = user_action({login, ServerPid, element(1, _From), User}, State)};
%% LOGOUT %%
handle_call({logout, User, ServerPid}, _From, State) ->
    io:format("\n\n[LOGOUT] request received by chat_controller: USR[ ~p ] ServerPID[ ~p ]", [User, ServerPid]),
    {reply, logged_in, NewState = user_action({logout, ServerPid, User}, State)};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% LIST ROOMS
handle_cast({listroom, ServerPid}, State) -> 
    room_action({list, ServerPid}, State), {noreply, State};
%% CREATE ROOM
handle_cast({newroom, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({new, RoomName, ServerPid, User, Public = true}, State)};
%% DELETE ROOM
handle_cast({delroom, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({del, RoomName, ServerPid, User}, State)};
%% JOIN ROOM
%% EXIT ROOM
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

%% LOGIN FUNCTIONS----------------------------------------------------------------------
user_action({login, ServerPid, TrsPid, User}, State) ->
    case dict:is_key(User, State#state.loggedUsers) of
        true ->
            gen_server:cast(ServerPid, {message, "Already logged in as [" ++ User ++ "]."}),
            State;
        false ->
            %% TODO: Add multiple login check;
            %% one tcp client can currently login multiple users per session
            gen_server:cast(TrsPid, {login, User}),
            gen_server:cast(ServerPid, login),
            NewState = #state{
                loggedUsers = dict:append(User, ServerPid, State#state.loggedUsers),
                activeRooms = State#state.activeRooms
            };
        _ ->
            State
    end;
user_action({logout, ServerPid, User}, State) ->
    case dict:is_key(User, State#state.loggedUsers) of
        true ->
            gen_server:cast(ServerPid, logout),
            NewState = #state{
        loggedUsers = dict:erase(User, State#state.loggedUsers),
                activeRooms = State#state.activeRooms
            };
        false ->
            %% not logged in
            gen_server:cast(ServerPid, {message, "You are not logged in."}),
            State;
        _ ->
            State
    end.
    
%% ROOM FUNCTIONS-----------------------------------------------------------------------
%% TODO: Improve the output
room_action({list, ServerPid}, State) ->
    RoomList = lists:flatten(io_lib:format("~p", [dict:to_list(State#state.activeRooms)])),
    gen_server:cast(ServerPid, {message, "Active rooms:\n\n" ++ RoomList});
room_action({new, RoomName, ServerPid, Owner, Visibility}, State) ->
    case dict:is_key(RoomName, State#state.activeRooms) of
        true ->
            gen_server:cast(ServerPid, {message, "Room name [" ++ RoomName ++ "]is taken."}),
            State;
        false ->
            gen_server:cast(ServerPid, {message, "New room [" ++ RoomName ++ "] added."}),
            NewState = #state{
                activeRooms = dict:append(RoomName, {Owner, [Owner], true}, State#state.activeRooms),
                loggedUsers = State#state.loggedUsers
            };
        _ ->
            State
    end;
room_action({del, RoomName, ServerPid, Owner}, State) ->
    case dict:is_key(RoomName, State#state.activeRooms) of
        false ->
            gen_server:cast(ServerPid, {message, "Room named [" ++ RoomName ++ "] was not found."}),
            State;
        true ->
            case element(1, lists:nth(1, dict:fetch(RoomName, State#state.activeRooms))) == Owner of
                false ->
                    gen_server:cast(ServerPid, {message, "You are not the owner of this room."}),
                    State;
                true ->
                    gen_server:cast(ServerPid, {message, "Room [" ++ RoomName ++ "] was deleted."}),
                    NewState = #state{
                        activeRooms = dict:append(RoomName, {Owner, [Owner], true}, State#state.activeRooms),
                        loggedUsers = State#state.loggedUsers
                    };
                _ ->
                    State
            end;
        _ ->
            State
    end;
room_action({join, User}, State) ->
    ok;
room_action({exit, User}, State) ->
    ok;
room_action({invite_private, User}, State) ->
    ok.

%% MESSAGE ROUTING FUNCTIONS------------------------------------------------------------

broadcast(UsrList, Message, From) ->
    lists:foreach(idk, UsrList).

