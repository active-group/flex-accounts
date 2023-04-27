-module(server).

-behaviour(gen_server).
-include("data.hrl").
-export([init/1, start/1, handle_call/3, handle_cast/2, publish/3]).

-record(publish, {person :: #person{}, account :: #account{} }).

-type message() :: pid() | #publish{}.
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
handle_cast(Message, PidList) ->      
    publish(Message#publish.person, Message#publish.account, PidList),
    {noreply, PidList}.

-spec publish(#person{}, #account{}, state()) -> {noreply, state()}.
publish(_Person, _Account, []) -> {noreply, []};
publish(Person, Account, [First | Rest]) ->
    io:format("Casting to ~w: Person=~w, Account=~w ~n", [First, Person, Account]),
    gen_server:cast(First, Person),
    gen_server:cast(First, Account),
    publish(Person, Account, Rest).
