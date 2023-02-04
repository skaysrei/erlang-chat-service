%%%-------------------------------------------------------------------
%% @doc chat_service top level supervisor.
%%      This supervisor manages the networking supervisor and the
%%      interal messaging server, comprising the different groups.
%% @end
%%%-------------------------------------------------------------------

-module(chat_service_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    io:format("Root supervisor started!~n~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags =  #{
            % when one process crashes, restart just that process
            strategy => one_for_one,
            % how many times in the period a process can be restarted before self termination
            intensity => 1,
            % how many seconds is a period made up of
            period => 5},
    ChildSpecs = [
        #{
            % internal identifier used by supervisor
            id => networking_sup,
            % function to start up the worker process { module, fun, args }
            % starts tcp server and passes the port
            start => {networking_sup, start_link, [1337]},
            restart => permanent, % permanent | transient | temporary
            % time in milliseconds with no response before worker gets shutdown
            shutdown => 2000,
            type => supervisor, % worker | supervisor
            % LOOK INTO WHAT THE HELL THIS DOES
            modules => [networking_sup]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions