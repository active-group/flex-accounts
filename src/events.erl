-module(events).
-export([init_events/0, unique_event_number/0,put_event/2, get_all_events/0, get_events_from/1]).
-include("events.hrl").

% call after database: init_database/0

init_events() ->
    dets:close(event),
    file:delete("event.dets"),
    {ok, event} = dets:open_file(event, [{type, set}, {file, "event.dets"}]),
    dets:insert(table_id, {event, 0}).


-spec unique_event_number() -> non_neg_integer().
unique_event_number() -> dets:update_counter(table_id, event, 1).

-spec put_event(non_neg_integer(), term()) -> #eventDB{}.
put_event(Number, Payload) ->
    database:write(event, {Number, Payload}),
    logger:info("Created event with number ~p", [Number]),
    #eventDB{number = Number, payload = Payload}.

-spec deserialize_event({non_neg_integer(), term()}) -> #eventDB{}.
deserialize_event({Number, Payload}) ->
    #eventDB{number = Number, payload = Payload}.

-spec get_all_events() -> [#eventDB{}].
get_all_events() ->
    database:read_all(event, fun deserialize_event/1).

-spec get_events_from(non_neg_integer()) -> [#eventDB{}].
get_events_from(Number) ->
    Res = dets:select(event,
                        [{'$1',
                        [{'>=', {element, 1, '$1'}, Number}],
                        ['$_']}]),
    Events = lists:map(fun deserialize_event/1, Res),
    lists:sort(fun (#eventDB{number = Number1}, #eventDB{number = Number2}) -> Number1 =< Number2 end, Events).
