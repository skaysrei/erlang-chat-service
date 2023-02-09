%%%-------------------------------------------------------------------
%% @doc
%% this supervisor starts the TCP socket and a pool of 10 TCP listeners,
%% each listener (worker) will generate a new one to replace itself as soon 
%% as it receives a connection request, when done the worker will die.
%% @end
%%%-------------------------------------------------------------------
-module(networking_sup).
-behaviour(supervisor).

% API
-export([start_link/1, start_server_worker/0]).
-export([init/1]).

start_link(Port) ->
    io:format("Starting networking supervisor...~n"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
    io:format("Opening TCP socket on port ~p~n~n", [Port]),
	{ok, Socket} = gen_tcp:listen(Port, [{active, once}]),
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
            type => worker,
            modules => [tcp_server]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.


%% INTERNAL FUNCTIONS-------------------------------------------------
start_server_worker() ->
	{ok, Pid} = supervisor:start_child(?MODULE, []),
    io:format("~nSpawning server worker (PID: ~p)", [Pid]),
    {ok, Pid}.

%% creates 10 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
	[start_server_worker() || _ <- lists:seq(1,10)],
	ok.
