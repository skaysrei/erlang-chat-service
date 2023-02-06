%%%-------------------------------------------------------------------
%% @doc 
%% translation layer worker.
%% receives the raw message from the client and parses it,
%% sends the appropriate reques to the chat_controller.
%% @end
%%%-------------------------------------------------------------------
-module(translation_layer).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% record is a shitty compiler hack why am i using it instead of a map??
-record(state, {
    user,
    login_time = null}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

handle_call({message, Msg}, _From, State) ->
    io:format("\n\nTranslation layer (Raw data):\n\nMessage[ ~s ]", [Msg]),
    %% extracts the [command, message] from raw data
    {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Msg),
    io:format("\nTranslation layer (Parsed data):\n\nCommand[ ~s ]\nMessage[ ~s ]\n\n", [Command, Content]),
    
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
