-module(server).

-behaviour(gen_server).
-include("events.hrl").
-export([init/1, start/1, handle_call/3, handle_cast/2, publish/2, handle_info/2]).

-type message() :: {pid(), no_events} | {pid(), #event{}}  | #event{}.
-type state() :: list(pid()).

start(PidList) ->
    gen_server:start(?MODULE, PidList, [{debug, [trace]}]).

-spec init(state()) -> {ok, state()}.
init(PidList) -> 
    register(account_service, self()),
    {ok, PidList}.

-spec handle_call(message(), gen_server:from(), state()) -> {reply, {ok, #event{}}, state()}.
handle_call({Pid, no_events}, _From, List) ->
    erlang:monitor(process, Pid), 
    {reply, {ok, database:get_all_events()}, lists:uniq([Pid | List])};
handle_call({Pid, Event}, _From, List) -> 
    erlang:monitor(process, Pid),
    {reply, {ok, database:get_events_from(Event#event.number)}, lists:uniq([Pid | List])}.

%handle_call(Message, _From, List) -> 
%    case Message of
%        {Pid, no_events} -> [Pid | List], {reply, ok, database:get_all_events()};
%        {Pid, Event} -> [Pid | List], {reply, ok, database:get_events_from(Event)}
%    end.

-spec handle_cast(message(), state()) -> {noreply, state()}.
handle_cast(Event, PidList) ->      
    publish(Event, PidList),
    {noreply, PidList}.

-spec publish(#event{}, state()) -> {noreply, state()}.
publish(_Event, []) -> {noreply, []};
publish(Event, [First | Rest]) ->
    io:format("Casting to ~w: Event=~w~n", [First, Event]),
    gen_server:cast(First, Event),
    publish(Event, Rest).

-spec handle_info(message(), state()) -> {noreply, state()}.
handle_info({'DOWN', _Ref, process, Pid2, Reason}, PidList) ->
    io:format("Subscriber ~w removed: ~w~n", [Pid2, Reason]),
    {noreply, lists:delete(Pid2, PidList)}.

