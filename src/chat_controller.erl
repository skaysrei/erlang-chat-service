-module(chat_controller).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% gets called by gen_server on init
% returns a tuple {ok, start_state}
init(_Args) ->
    State = maps:new(), %% Create a Map to encapsulate loop State
    Users = maps:new(), %% Map store of all users
    State#{"Users" => Users},

    io:format("Starting chat_controller...~n~n"),
    {ok, State}.

%% pattern matches the received message
%% args:(message, module, loopdata)
handle_call(stop, _From, State) -> %% handle_call is synchronous
    {stop, normal, stopped, State};

handle_call({message, Msg}, _From, State) ->
    io:format("\n\nCall receiver by chat_controller: MSG[ ~p ]~n~n", [Msg]),
    {reply, {}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% handles initial client connection synchronously,
%% checks if the username is taken and updates the
%% Users state accordingly.
% handle_call() ->
%     Response = case dict:is_key(UserName, Users) of
%         true ->
%             NewUsers = Users,
%             username_taken;
%         false ->
%             NewUsers = dict:append(UserName, Socket, Users)
%     {}.

%% handle_call is asynchronous
handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

