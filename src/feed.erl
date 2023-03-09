-module(feed).
-behaviour(gen_server).
-include("data.hrl").
-export([init/1, handle_call/3, start_link/0, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, account_feed}, ?MODULE, [], []).

init([]) -> {ok, []}.

handle_cast(Msg, State) ->
    lager:error("Received illegal cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call({events, From}, _, State) ->
    {reply, database:get_events_from(From), State};
handle_call(Msg, _, State) ->
    lager:error("Received illegal call: ~p~n", [Msg]),
    {reply, undefined, State}.

handle_info(Info, State) ->
    lager:error("Received illegal info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
