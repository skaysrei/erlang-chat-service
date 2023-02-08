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

%% loggedUsers[dict] -> Key(Username = "") => Val(tcp_server PID = pid())
%% activeRooms[dict] -> Key(GroupName = "") => Val({Owner = "", ActiveMembers = [[],...], Public = bool})
-record(state, {
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
    {reply, logged_in, NewState = user_action({login, ServerPid, User}, State)};
%% LOGOUT %%
handle_call({logout, User, ServerPid}, _From, State) ->
    io:format("\n\n[LOGOUT] request received by chat_controller: USR[ ~p ] ServerPID[ ~p ]", [User, ServerPid]),
    {reply, logged_in, NewState = user_action({logout, ServerPid, User}, State)};     
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% WHO AM I
handle_cast({who, ServerPid, User}, State) ->
    client_info(who, User, ServerPid), {noreply, State};
%% WHERE AM I
handle_cast({where, ServerPid, Room}, State) ->
    client_info(where, Room, ServerPid), {noreply, State};    

%% LIST ROOMS
handle_cast({listroom, ServerPid}, State) -> 
    room_action({list, ServerPid}, State), {noreply, State};
%% CREATE ROOM
handle_cast({newroom, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({new, RoomName, ServerPid, User, Public = true}, State)};
%% CREATE ROOM PRIVATE
handle_cast({newroom, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({new, RoomName, ServerPid, User, Public = false}, State)};
%% DELETE ROOM
handle_cast({delroom, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({del, RoomName, ServerPid, User}, State)};
%% JOIN ROOM
handle_cast({delroom, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({join, RoomName, ServerPid, User}, State)};
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
%% PATTERN MATCHING GALORE! ->

%% TODO: Implement state manipulation functions during refactorin stage
%% TODO: Refactoring all those nasty 'case' constructs
%% USER STATE MANIPULATION--------------------------------------------------------------
update_user(State, _Args) ->
    ok.

%% ROOM STATE MANIPULATION--------------------------------------------------------------
update_room(State, _Args) ->
    ok.

%% LOGIN FUNCTIONS----------------------------------------------------------------------
user_action({login, ServerPid, User}, State) ->
    case dict:is_key(User, State#state.loggedUsers) of
        true ->
            gen_server:cast(ServerPid, {message, "Already logged in as " ++ User ++ "."}),
            State;
        false ->
            %% TODO: Add multiple login check; REMOVE REDUNDANCY (set_user/login and logout too)
            %% one tcp client can currently login multiple users per session
            gen_server:cast(ServerPid, {set_user, User}), %% updating state on trs_layer
            gen_server:cast(ServerPid, login),
            NewState = #state{
                loggedUsers = dict:append(User, ServerPid, State#state.loggedUsers),
                activeRooms = State#state.activeRooms
            };
        _ ->
            State
    end;
%% TODO: Implement cleanup on logout (zero state in translation_layer and self for User and Room)
user_action({logout, ServerPid, User}, State) ->
    case dict:is_key(User, State#state.loggedUsers) of
        true ->
            gen_server:cast(ServerPid, {set_user, ""}), %% updating state on trs_layer
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

%% [META FUNCTIONS]
client_info(who, User, ServerPid) -> 
    case User of
        "" ->
            Message = "You are not currently logged in.";
        _ ->
            Message = "You are currently logged in as: " ++ User ++ "."
    end,
    send(ServerPid, {Message});
client_info(where, Room, ServerPid) -> 
    case Room of
        "" ->
            Message = "You are not currently in a room.";
        _ ->
            Message = "You are currently in the room: " ++ Room ++ "."
    end,
    send(ServerPid, {Message}).

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
            case Owner == "" of
                true ->
                    gen_server:cast(ServerPid, {message, "You need to be logged in first."}),
                    State;
                false ->
                    gen_server:cast(ServerPid, {message, "New room [" ++ RoomName ++ "] added."}),
                    NewState = #state{
                        activeRooms = dict:append(RoomName, {Owner, [], true}, State#state.activeRooms),
                        loggedUsers = State#state.loggedUsers
                    }
                end;
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
                    gen_server:cast(ServerPid, {set_room, RoomName}), %% updating state on trs_layer
                    NewState = #state{
                        activeRooms = dict:erase(RoomName, State#state.activeRooms),
                        loggedUsers = State#state.loggedUsers
                    };
                _ ->
                    State
            end;
        _ ->
            State
    end;
room_action({join, RoomName, ServerPid, User}, State) ->
    %% check if the room exists
    case dict:is_key(RoomName, State#state.activeRooms) of
        false ->
            gen_server:cast(ServerPid, {message, "Room named [" ++ RoomName ++ "] was not found."}),
            State;
        true ->
            %% check if public
            case element(3, dict:fetch(RoomName, State#state.activeRooms)) of
                false ->
                    gen_server:cast(ServerPid, {message, "Room [" ++ RoomName ++ "] private!"}),
                    State;
                true ->
                    %% sends room state update to the client's server worker
                    gen_server:cast(ServerPid, {set_room, RoomName}),
                    gen_server:cast(ServerPid, {message, "You joined [" ++ RoomName ++ "]!"}),
                    UpdatedRooms = element(2, dict:fetch(RoomName, State#state.activeRooms)),
                    NewState = #state{
                        activeRooms = dict:update(RoomName, fun (Old) -> erlang:insert_element(2, Old, lists:append(element(2, Old), [User])) end, State#state.activeRooms),
                        loggedUsers = State#state.loggedUsers
                    };
                _ ->
                    State
            end;
        _ ->
            State
        end;
room_action({exit, User}, State) ->
    ok;
room_action({invite_private, User}, State) ->
    ok.

%% MESSAGE ROUTING FUNCTIONS------------------------------------------------------------

%% send messages to the client
send(ServerPid, {Message}) ->
    gen_server:cast(ServerPid, {message, Message});
send(ServerPid, {Message, From}) ->
    FullMessage = "New message (USER[ " ++ From ++ " ] | PRIVATE: " ++ Message,
    gen_server:cast(ServerPid, {message, FullMessage});
send(ServerPid, {Message, From, Room}) ->
    FullMessage = "New message (USER[ " ++ From ++ " ] | ROOM[ " ++ Room ++ " ]: " ++ Message,
    gen_server:cast(ServerPid, {message, FullMessage}).

broadcast(UsrList, Message, From) ->
    lists:foreach(idk, UsrList).
