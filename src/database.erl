-module(database).
-include("data.hrl").
-export([init_database/0, destroy_tables/0, create_account/1,  next_person_id/0, next_account_number/0,
         get_all_events/0, get_all_events/1, get_events_from/1]).

destroy_tables() ->
    dets:close(event), % better safe than sorry
    file:delete("event.dets"),
    dets:close(table_id),
    file:delete("table_id.dets").

create_tables() ->
    {ok, event} = dets:open_file(event, [{keypos, #event.index}, {file, "event.dets"}]),
    {ok, table_id} = dets:open_file(table_id, [{file, "table_id.dets"}]),
    ok = dets:insert(table_id, {event, 0}),
    ok = dets:insert(table_id, {account, 0}),
    ok = dets:insert(table_id, {person, 0}),
    ok.

init_database() ->
    ok = create_tables(),
    ok.

-spec put_event(#event{}) -> ok.
put_event(Event) ->
    ok = dets:insert(event, Event).

-spec get_all_events() -> list(#event{}).
get_all_events() ->
    dets:select(event, [{'_',
                        [],
                        ['$_']}]).

-spec get_all_events(atom()) -> list(#event{}).
get_all_events(Type) ->
    dets:select(event, [{'$1',
                        [{'==', {element, #event.type, '$1'}, Type}],
                        ['$_']}]).

-spec get_events_from(non_neg_integer()) -> list(#event{}).
get_events_from(Index) ->
    dets:select(event, [{'$1',
                        [{'>=', {element, #event.index, '$1'}, Index}],
                        ['$_']}]).

-spec next_event_index() -> non_neg_integer().
next_event_index() -> dets:update_counter(table_id, event, 1).

-spec new_account_event(unique_id(), unique_id()) -> #event{}.
new_account_event(AccountNumber, PersonId) ->
    #event{index = next_event_index(),
           type = new_account_event,
           content = {AccountNumber, PersonId, 1000}}.

-spec new_person_event(binary(), binary(), unique_id()) -> #event{}.
new_person_event(Firstname, Surname, PersonId) ->
    #event{index = next_event_index(),
           type = new_person_event,
           content = {PersonId, Firstname, Surname}}.

-spec create_account(#account{}) -> ok.
create_account(Account) ->
    Person = Account#account.person,
    PersonEvent = new_person_event(Person#person.firstname, Person#person.lastname, Person#person.id),
    AccountEvent = new_account_event(Account#account.account_number, Person#person.id),
    put_event(PersonEvent),
    put_event(AccountEvent).

-spec next_account_number() -> unique_id().
next_account_number() -> dets:update_counter(table_id, account, 1).
-spec next_person_id() -> unique_id().
next_person_id() -> dets:update_counter(table_id, person, 1).
