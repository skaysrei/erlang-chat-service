%%%-------------------------------------------------------------------
%% @doc 
%% chat_service top level supervisor.
%% this supervisor manages the networking supervisor and the
%% interal messaging server, comprising the different groups.
%% @end
%%%-------------------------------------------------------------------

-module(chat_service_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    io:format("Starting root supervisor...~n~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags =  #{
            strategy => one_for_one,
            % how many times in the period a process can be restarted
            intensity => 1,
            % how many seconds is a period made up of
            period => 5},
    ChildSpecs = [
        #{
            id => networking_sup,
            start => {networking_sup, start_link, [1337]},
            restart => permanent,
            shutdown => 2000, %% timeout expressed in ms
            type => supervisor, % worker | supervisor
            % LOOK INTO WHAT THE HELL THIS DOES
            modules => [networking_sup]
        },
        #{
            id => chat_controller,
            start => {chat_controller, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [chat_controller]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions