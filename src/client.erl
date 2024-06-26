%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([open_account/2,get_person/1,get_account/1]).


%% opens an acocunt with a given name and surname.
%% prints the result and the account number to stdout.
-spec open_account(string(), string()) -> ok.
open_account(GivenName, Surname) ->
    Account = business_logic:open_account(list_to_binary(GivenName),
                                          list_to_binary(Surname)),
    io:format("Account was successfully opened. Account number: ~p ~n", [Account#account.account_number]).

-spec get_person(unique_id()) -> {ok, #person{} | {error, any()}}.
get_person(Id) -> business_logic:get_person(Id).


-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) -> business_logic:get_account(AccountNumber).

