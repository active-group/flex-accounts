%% This module represents the frontend layer

-module(client).
-export([open_account/2, name_by_account_number/1]).

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
%% returns the name of the person associated to the account
%% given by account number.
-spec name_by_account_number(account_number()) -> string().
name_by_account_number(AccountNumber) ->
    {ok, Account} = business_logic:get_account(AccountNumber),
    {ok, Person} = business_logic:get_person(Account#account.person_id),
    binary_to_list(Person#person.given_name) ++ " " ++ binary_to_list(Person#person.surname).

%% opens an acocunt with a given name and surname.
%% prints the result and the account number to stdout.
-spec open_account(string(), string()) -> ok.
open_account(GivenName, Surname) ->
    Account = business_logic:open_account(
        list_to_binary(GivenName),
        list_to_binary(Surname)
    ),
    io:format("Account was successfully opened. Account number: ~p ~n", [
        Account#account.account_number
    ]).