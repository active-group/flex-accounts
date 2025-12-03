%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([open_account/2, get_account/1, get_person/1]).


%% Opens an account, that is creates a new account containing a new person 
%% Writes them into database.

-spec open_account(binary(), binary()) -> #account{}.
open_account(GivenName, Surname) ->
    make_account(
      make_person(
        GivenName, Surname)
     ).

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) -> database:get_account(AccountNumber).

-spec make_person(binary(), binary()) -> #person{}.
make_person(GivenName, Surname) ->
    PersonId = database:unique_person_id(),
    Person = #person{id = PersonId,
                   given_name = GivenName,
                   surname = Surname},
    database:put_person(Person),
    trigger_person_created_event(Person),
    Person.

-spec get_person(unique_id()) -> {ok, #person{} | {error, any()}}.
get_person(Id) -> database:get_person(Id).

-spec make_account(#person{}) -> #account{}.
make_account(Person) ->
    AccountNumber = database:unique_account_number(),
    Account = #account{account_number = AccountNumber,
                   person_id = Person#person.id,
                   amount = 1000},
    database:put_account(Account),
    trigger_account_created_event(Account),
    Account.

-spec trigger_account_created_event(#account{}) -> {}.
trigger_account_created_event(#account{account_number = AccountId, person_id = PersonId}) ->
  gen_server:cast(event_sender, #account_creation_event{account_number = AccountId, person_id = PersonId}).

-spec trigger_person_created_event(#person{}) -> {}.
trigger_person_created_event(#person{id = PersonId, given_name = Name, surname = Surname}) ->
  gen_server:cast(event_sender, #person_creation_event{id = PersonId, given_name = Name, surname = Surname}).
