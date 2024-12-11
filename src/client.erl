%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([open_account/2, statement/1]).

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

%% prints the header of a bank statement, namely the full name and the
%% current balance, associated with the account number to stdout.
print_head(AccountNumber) ->
    {ok, Account} = business_logic:get_account(AccountNumber),
    Name = name_by_account_number(AccountNumber),
    io:format("~nBank statement for: ~s~n", [Name]),
    io:format("---------------------------------------------------- ~n", []),
    io:format("Balance: ~p~n", [Account#account.amount]),
    io:format("---------------------------------------------------- ~n", []).
