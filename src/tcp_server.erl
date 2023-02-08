%%%-------------------------------------------------------------------
%% @doc 
%% gen_tcp server to handle client-server data exchange;
%% accepts for incoming connections and forwards data to
%% translation_layer, while sending back to the cliet the
%% messages received by the chat_controller.
%% @end
%%%-------------------------------------------------------------------
-module(tcp_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket, trs_pid}).

start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
	%% start accepting requests;
	%% we must cast this to the worker's process, as it blocks it.
	gen_server:cast(self(), accept),
	{ok, #state{socket=Socket}}.

%% STARTUP SEQUENCE---------------------------------------------------
handle_cast(accept, State = #state{socket=ListenSocket}) ->
	%% enters accepting state on the socket
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	%% boot a new listener to replace this one.
	networking_sup:start_server_worker(),

	%% starts translation_layer subprocess
	{ok, Pid} = translation_layer:start_link(),

	%% link(Pid),
	send(AcceptSocket, "Server ready.", []),
	{noreply, State#state{socket=AcceptSocket, trs_pid=Pid}};
%% INTERNAL REQUEST HANDLING------------------------------------------
%% handles chat_controller output
handle_cast(login, State) ->
	send(State#state.socket, "Logged in successfully OwO", []),
	{noreply, State};
handle_cast(logout, State) ->
	send(State#state.socket, "Logged out successfully UwU", []),
	{noreply, State};
handle_cast({message, Message}, State) ->
	send(State#state.socket, Message, []),
	{noreply, State};
%% updates the user and room of control layer
handle_cast({set_user, User}, State) ->
	gen_server:cast(State#state.trs_pid, {set_user, User}),{noreply, State};
handle_cast({set_room, Room}, State) ->
	gen_server:cast(State#state.trs_pid, {set_room, Room}),{noreply, State}.

%% NETWORK REQUEST HANDLING (gen_tcp)---------------------------------
%% TODO: CLEANUP USER STATE ON CLOSED CONNECTION
handle_info({tcp, Socket, "!exit"++_}, State) ->
	%% TODO: Change the translation_layer exit
	exit(State#state.trs_pid, kill),
	gen_tcp:close(Socket),
	{stop, normal, State};
handle_info({tcp, Socket, Msg}, State) ->
	%% TODO: change to gen_server:cast
	gen_server:call(State#state.trs_pid, {message, Msg}),
	{noreply, State};
%% TODO add logout for conn lost or error
handle_info({tcp_closed, _Socket}, State) -> exit(State#state.trs_pid, kill), {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> exit(State#state.trs_pid, kill), {stop, normal, State};
handle_info(E, State) ->
	io:fwrite("\n\nunexpected: ~p~n", [E]),
	{noreply, State}.

terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

%% INTERNAL FUNCTIONS-------------------------------------------------

%% send message back to the client
send(Socket, Str, Args) ->
	%% TODO: Overhaul this thing ??
	ok = gen_tcp:send(Socket, io_lib:format(Str, Args)),
	ok = inet:setopts(Socket, [{active, once}]),
	ok.

%% TODO: implement logout on conn_lost/error
initiate_logout() ->
	ok.