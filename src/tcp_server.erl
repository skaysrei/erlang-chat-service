-module(tcp_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket, trs_pid}).

start_link(Socket) ->
	{ok, Pid} = gen_server:start_link(?MODULE, Socket, []),
	io:format("TEST PID1 ~p\n", [Pid]),
	{ok, Pid}.


init(Socket) ->
	%% Start accepting requests
	%% We must cast this to the worker's process, as it blocks it.
	gen_server:cast(self(), accept),
	{ok, #state{socket=Socket}}.

%% enters accepting state on the socket
%% will handle async messages from gen_tcp
handle_cast(accept, State = #state{socket=ListenSocket}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	%% boot a new listener to replace this one.
	networking_sup:start_server_worker(),
	{ok, Pid} = translation_layer:start_link(),
	send(AcceptSocket, "Server ready :3", []),
	{noreply, State#state{socket=AcceptSocket, trs_pid=Pid}};
handle_cast(login, State) ->
	send(State#state.socket, "Logged in successfully OwO", []),
	{noreply, State};
handle_cast(logout, State) ->
	send(State#state.socket, "Logged out successfully UwU", []),
	{noreply, State};
handle_cast({message, Message}, State) ->
	send(State#state.socket, Message, []),
	{noreply, State}.

%% handles messages from the client
handle_info({tcp, Socket, "quit"++_}, State) ->
	gen_tcp:close(Socket),
	{stop, normal, State};
%% handles the message received on the socket
handle_info({tcp, Socket, Msg}, State) ->
	%% send received msg to chat_controller
	gen_server:call(State#state.trs_pid, {message, Msg}),
	send(Socket, "Server received: " ++ Msg, []),
	{noreply, State};
handle_info({tcp_closed, _Socket}, State) -> {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State};
handle_info(E, State) ->
	io:fwrite("unexpected: ~p~n", [E]),
	{noreply, State}.

handle_call({error, Msg}, _From, State) -> 
	send(State#state.socket, Msg, []),
	{noreply, State}.

terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

%% Send a message back to the client
send(Socket, Str, Args) ->
	%% TODO: Remove complexity
	ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
	ok = inet:setopts(Socket, [{active, once}]),
	ok.