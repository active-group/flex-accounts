-module(server).

-behaviour(gen_server).

-export([init/1, start/1, handle_call/3, handle_cast/2]).

-type message() :: pid().
-type state() :: list(pid()).

start(PidList) ->
    gen_server:start(?MODULE, PidList, [{debug, [trace]}]).

-spec init(state()) -> {ok, state()}.
init(PidList) -> 
    register(account_service, self()),
    {ok, PidList}.

-spec handle_call(message(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(Pid, _From, List) -> {reply, ok, [Pid | List]}.
% handle_call(Pid, _From, [First | Rest]) -> {reply, ok, [Pid | [First | Rest]]}.

-spec handle_cast(message(), state()) -> {noreply, state()}.
handle_cast(_Pid, []) ->
     {noreply, []}.