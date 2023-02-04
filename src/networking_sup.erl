% this supervisor starts the TCP socket and a pool of 10 TCP listeners,
% each listener (worker) will generate a new one to replace itself as soon 
% as it receives a connection request, when done the worker will die.

-module(networking_sup).
-behaviour(supervisor).

-export([start_link/1, start_socket/0]).
-export([init/1]).

start_link(Port) ->
    io:format("Networking supervisor started!~n"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init(Port) ->
    io:format("OPENING TCP SOCKET ON PORT: ~p~n~n~n~n", Port),
    % fix the goddam list unpacking in the args!! wtf is this trash
	{ok, Socket} = gen_tcp:listen(1337, [{active, once}]),
	%% fire up another process to spawn the starting pool of listeners,
	%% doing so separately as this is a blocking process(?).
	spawn_link(fun empty_listeners/0),

    SupFlags = #{
        % restarting dynamically generated workers
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecs = [
        #{
            id => tcp_server,
            start => {tcp_server, start_link, [Socket]},
            restart => temporary, % permanent | transient | temporary
            shutdown => 2000, % shutdown time expressed in ms
            type => worker, % worker | supervisor
            modules => [tcp_server]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.


%% internal functions

start_socket() ->
	{ok, Pid} = supervisor:start_child(?MODULE, []),
    io:format("Spawning listener worker (PID: ~p)", [Pid]),
    {ok, Pid}.

%% creates 10 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
	[start_socket() || _ <- lists:seq(1,10)],
    io:format("TCP listener started!~n~n~n~n"),
	ok.