%%%-------------------------------------------------------------------
%% @doc chat_service public API
%% entrypoint for the app, starts the main supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_service_app).

-behaviour(application).

-export([start/2, stop/1]).

% Starts the root supervisor
start(_StartType, _StartArgs) ->
    chat_service_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
