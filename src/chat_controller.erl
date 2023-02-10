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
%% BAD COMMAND!
handle_cast({badcomm, Command, ServerPid}, State) ->
    user_action({badcomm, Command, ServerPid}, State), {noreply, State};

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
handle_cast({joinroom, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({join, RoomName, ServerPid, User}, State)};
%% EXIT ROOM
handle_cast({exitroom, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({exit, RoomName, ServerPid, User}, State)};
%% SET PRIVATE
handle_cast({set_private, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({set_private, RoomName, ServerPid, User}, State)};
%% SET PUBLIC
handle_cast({set_public, RoomName, ServerPid, User}, State) -> 
    {noreply, NewState = room_action({set_public, RoomName, ServerPid, User}, State)};
%% INVITE USER
handle_cast({invite_user, RoomName, ServerPid, User, Target}, State) -> 
    {noreply, NewState = room_action({invite_user, RoomName, ServerPid, User, Target}, State)};
%% BROADCAST
handle_cast({broadcast, RoomName, Message, ServerPid, User}, State) -> 
    {noreply, NewState = routing_action({broadcast, RoomName, Message, ServerPid, User}, State)};
%% DIRECT
handle_cast({direct, Recipient, Message, ServerPid, User}, State) -> 
    {noreply, NewState = routing_action({direct, Recipient, Message, ServerPid, User}, State)};
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
                loggedUsers = dict:store(User, ServerPid, State#state.loggedUsers),
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
    end;
user_action({badcomm, Command, ServerPid}, State) -> send(ServerPid, {"Command '" ++ Command ++ "' not recognized!"}).

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
    RoomList = dict:to_list(State#state.activeRooms),
    io:format("ROOMLIST: ~p", [RoomList]),
    Message = case erlang:length(RoomList) < 1 of
        true ->
            "There are no active rooms.";
        false ->
            OutputList = lists:map(fun (R) ->
                    Visibility = case element(3, element(2, R)) of
                        true -> "PUBLIC"; 
                        false -> "PRIVATE"
                    end,
                    RoomName = element(1, R),
                    NMemb = erlang:length(element(2, element(2, R))),
            %% compose output message from extracted data
            io_lib:format("\n\n[~p] ~p: ~p Current Members", [Visibility, RoomName, NMemb]) end, RoomList)
    end,
    gen_server:cast(ServerPid, {message, "Active rooms:" ++ Message});
room_action({new, RoomName, ServerPid, Owner, Visibility}, State) ->
    case dict:is_key(RoomName, State#state.activeRooms) of
        true ->
            gen_server:cast(ServerPid, {message, "Room name [" ++ RoomName ++ "] is taken."}),
            State;
        false ->
            case Owner == "" of
                true ->
                    gen_server:cast(ServerPid, {message, "You need to be logged in first."}),
                    State;
                false ->
                    gen_server:cast(ServerPid, {message, "New room [" ++ RoomName ++ "] added."}),
                    NewState = #state{
                        activeRooms = dict:store(RoomName, {Owner, [], true}, State#state.activeRooms),
                        loggedUsers = State#state.loggedUsers
                    }
                end;
        _ ->
            State
    end;
room_action({del, RoomName, ServerPid, User}, State) ->
    case dict:is_key(RoomName, State#state.activeRooms) of
        false ->
            gen_server:cast(ServerPid, {message, "Room named [" ++ RoomName ++ "] was not found."}),
            State;
        true ->
            case element(1, dict:fetch(RoomName, State#state.activeRooms)) == User of
                false ->
                    gen_server:cast(ServerPid, {message, "You are not the owner of this room."}),
                    State;
                true ->
                    gen_server:cast(ServerPid, {message, "Room [" ++ RoomName ++ "] was deleted."}),
                    gen_server:cast(ServerPid, {set_room, RoomName}), %% updating state on trs_layer %% ADD CHECK TOO SEE IF I AM INSIDE ROOM
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
            %% check if public / TODO: check if inside
        IsOwner = (element(1, dict:fetch(RoomName, State#state.activeRooms)) == User),
            case {IsOwner, element(3, dict:fetch(RoomName, State#state.activeRooms))} of
                %% matches all public rooms and private/owner
                Clause when Clause =:= {true, true}; Clause =:= {false, true}; Clause =:= {true, false} ->
                    %% sends room state update to the client's server worker
                    gen_server:cast(ServerPid, {set_room, RoomName}),
                    gen_server:cast(ServerPid, {message, "You joined [" ++ RoomName ++ "]!"}),

                    %% atomized update operations
                    Current = dict:fetch(RoomName, State#state.activeRooms),
                    Partecipants = element(2, Current),
                    NewPart = lists:append(Partecipants, [User]),
                    ReplaceList = erlang:setelement(2, Current, NewPart),
                    UpdatedDict = dict:update(RoomName, fun (_) -> ReplaceList end, State#state.activeRooms),

                    NewState = #state{
                        activeRooms = UpdatedDict,
                        loggedUsers = State#state.loggedUsers
                    };
                {false, false} ->
                    gen_server:cast(ServerPid, {message, "Room [" ++ RoomName ++ "] private!"}),
                    State;
                _ ->
                    State
            end;
        _ ->
            State
    end;
room_action({exit, RoomName, ServerPid, User}, State) ->
    %% check if the room exists
    case dict:is_key(RoomName, State#state.activeRooms) of
        false ->
            gen_server:cast(ServerPid, {message, "Room named [" ++ RoomName ++ "] was not found."}),
            State;
        true ->
            %% check if user is inside
            case lists:search(fun (Elem) -> Elem == User end, element(2, dict:fetch(RoomName, State#state.activeRooms))) of %% remove nth
                false ->
                    gen_server:cast(ServerPid, {message, "Your are not in this room."}),
                    State;
                {value, User} ->
                    %% sends room state update to the client's server worker
                    gen_server:cast(ServerPid, {set_room, RoomName}),
                    gen_server:cast(ServerPid, {message, "You left [" ++ RoomName ++ "]!"}),

                    %% atomized update operations
                    Current = dict:fetch(RoomName, State#state.activeRooms),
                    Partecipants = element(2, Current),
                    NewPart = lists:delete(User, Partecipants),
                    ReplaceList = erlang:setelement(2, Current, NewPart),
                    UpdatedDict = dict:update(RoomName, fun (_) -> ReplaceList end, State#state.activeRooms),

                    NewState = #state{
                        activeRooms = UpdatedDict,
                        loggedUsers = State#state.loggedUsers
                    };
                _ ->
                    State
            end;
        _ ->
            State
    end;
%% TODO: these two are basically the same function, should refactor
%% LIKE HOLY SHIT SO MUCH DUPLICATE CODE, but i am too sleepy now
%% make it work today -> refactor tomorrow :3
room_action({set_private, RoomName, ServerPid, User}, State) ->
    %% check if the room exists
    case dict:is_key(RoomName, State#state.activeRooms) of
        false ->
            gen_server:cast(ServerPid, {message, "Room named [" ++ RoomName ++ "] was not found."}),
            State;
        true ->
            %% check if user is the owner
            case element(1, dict:fetch(RoomName, State#state.activeRooms)) == User of
                true ->
                    gen_server:cast(ServerPid, {message, "Room [" ++ RoomName ++ "] set to private."}),
                    UpdatedRoom = erlang:insert_element(3, dict:fetch(RoomName, State#state.activeRooms), false),
                    NewState = #state{
                        activeRooms = dict:store(RoomName, UpdatedRoom, State#state.activeRooms),
                        loggedUsers = State#state.loggedUsers
                    };
                false->
                    gen_server:cast(ServerPid, {message, "Only the owner can edit the room visibility."}),
                    State;
                _ ->
                    State
            end
    end;
room_action({set_public, RoomName, ServerPid, User}, State) ->
    %% check if the room exists
    case dict:is_key(RoomName, State#state.activeRooms) of
        false ->
            gen_server:cast(ServerPid, {message, "Room named [" ++ RoomName ++ "] was not found."}),
            State;
        true ->
            %% check if user is the owner
            case element(1, dict:fetch(RoomName, State#state.activeRooms)) == User of
                true ->
                    gen_server:cast(ServerPid, {message, "Room [" ++ RoomName ++ "] set to public."}),
                    UpdatedRoom = erlang:insert_element(3, dict:fetch(RoomName, State#state.activeRooms), true),
                    NewState = #state{
                        activeRooms = dict:store(RoomName, UpdatedRoom, State#state.activeRooms),
                        loggedUsers = State#state.loggedUsers
                    };
                false->
                    gen_server:cast(ServerPid, {message, "Only the owner can edit the room visibility."}),
                    State;
                _ ->
                    State
            end
    end;
room_action({invite_user, RoomName, ServerPid, User, Target}, State) ->
    case {dict:is_key(RoomName, State#state.activeRooms),
        dict:is_key(Target, State#state.loggedUsers)} of
        Tuple when Tuple =:= {false, true}; Tuple =:= {false, false} ->
            gen_server:cast(ServerPid, {message, "Room named [" ++ RoomName ++ "] was not found."}),
            State;
        {true, false} ->
            gen_server:cast(ServerPid, {message, "User " ++ Target ++ " does not exist."}),
            State;
        {true, true} ->
            case element(1, dict:fetch(RoomName, State#state.activeRooms)) == User of
                false ->
                    gen_server:cast(ServerPid, {message, "Only the owner can add new members."});
                true ->
                    TargetPid = dict:fetch(Target, State#state.loggedUsers),
                    gen_server:cast(TargetPid, {set_room, RoomName}),
                    gen_server:cast(TargetPid, {message, "You were added to the private room: " ++ RoomName ++ "."}),

                    %% atomized update operations
                    Current = dict:fetch(RoomName, State#state.activeRooms),
                    Partecipants = element(2, Current),
                    NewPart = lists:append(Partecipants, [Target]),
                    ReplaceList = erlang:setelement(2, Current, NewPart),
                    UpdatedDict = dict:update(RoomName, fun (_) -> ReplaceList end, State#state.activeRooms),

                    NewState = #state{
                        activeRooms = UpdatedDict,
                        loggedUsers = State#state.loggedUsers
                    }
            end
    end.

%% MESSAGE ROUTING FUNCTIONS------------------------------------------------------------
routing_action({broadcast, RoomName, Message, ServerPid, User}, State) ->
    case dict:is_key(RoomName, State#state.activeRooms) of
        false ->
            gen_server:cast(ServerPid, {message, "Room named [" ++ RoomName ++ "] was not found."}),
            State;
        true ->
            lists:foreach(
                fun (RoomMember) ->
                    send(
                        dict:fetch(RoomMember, State#state.loggedUsers),
                        {Message, User, RoomName}
                    )
                end, element(2, dict:fetch(RoomName, State#state.activeRooms))),
                State
    end;
routing_action({direct, Recipient, Message, ServerPid, User}, State) ->
    case dict:is_key(Recipient, State#state.loggedUsers) of
        false ->
            gen_server:cast(ServerPid, {message, "User " ++ Recipient ++ " does not exist."}),
            State;
        true ->
            send(
                dict:fetch(Recipient, State#state.loggedUsers),
                {Message, User}
            ), State
    end.
%% send messages to the client
send(ServerPid, {Message}) ->
    gen_server:cast(ServerPid, {message, Message});
send(ServerPid, {Message, From}) ->
    FullMessage = "direct@" ++ From ++ ": " ++ Message,
    gen_server:cast(ServerPid, {message, FullMessage});
send(ServerPid, {Message, From, Room}) ->
    FullMessage = "room:[" ++ Room ++ "]@" ++ From ++ ": " ++ Message,
    gen_server:cast(ServerPid, {message, FullMessage}).

broadcast(UsrList, Message, From) ->
    lists:foreach(idk, UsrList).
