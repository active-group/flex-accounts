%% This module represents the business logic layer

-module(business_logic).
-export([open_account/2, get_account/1, get_person/1]).

-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(person, {
    id :: unique_id(),
    given_name :: binary(),
    surname :: binary()
}).
-record(account, {
    account_number :: account_number(),
    person_id :: unique_id(),
    amount :: money()
}).

-record(account_dto, {
    account_number :: number(),
    person_id :: number(),
    given_name :: string(),
    surname :: string(),
    amount :: number()
}).

%% Opens an account, that is creates a new account containing a new person
%% Writes them into database.

-spec open_account(binary(), binary()) -> #account{}.
open_account(GivenName, Surname) ->
    Person = make_person(GivenName, Surname),
    Account = make_account(Person),
    AccountDto = #account_dto{
        account_number = Account#account.account_number,
        person_id = Account#account.person_id,
        given_name = Person#person.given_name,
        surname = Person#person.surname,
        amount = Account#account.amount
    },
    account_server:add_account(AccountDto),
    Account.

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) -> database:get_account(AccountNumber).

-spec make_person(binary(), binary()) -> #person{}.
make_person(GivenName, Surname) ->
    PersonId = database:unique_person_id(),
    Person = #person{
        id = PersonId,
        given_name = GivenName,
        surname = Surname
    },
    database:put_person(Person),
    Person.

-spec get_person(unique_id()) -> {ok, #person{} | {error, any()}}.
get_person(Id) -> database:get_person(Id).

-spec make_account(#person{}) -> #account{}.
make_account(Person) ->
    AccountNumber = database:unique_account_number(),
    Account = #account{
        account_number = AccountNumber,
        person_id = Person#person.id,
        amount = 1000
    },
    database:put_account(Account),
    Account.
