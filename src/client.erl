%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([open_account/2, print_all_accounts/0]).

%% opens an acocunt with a given name and surname.
%% prints the result and the account number to stdout.
-spec open_account(string(), string()) -> ok.
open_account(GivenName, Surname) ->
    Account = business_logic:open_account(list_to_binary(GivenName),
                                          list_to_binary(Surname)),
    io:format("Account was successfully opened. Account number: ~p ~n", [Account#account.account_number]).

-spec print_all_accounts() -> ok.
print_all_accounts() ->
  Accounts = database:get_all_accounts(),
   lists:foreach(
     fun(#account{account_number = Account_number, person_id = Person_id, amount = Amount}) ->
       {ok, #person{given_name = Given_Name, surname = Surname}} = database:get_person(Person_id),
       io:format("~p - ~p - ~p ~p - ~p~n",
         [
           Account_number,
           Person_id,
           binary_to_list(Given_Name),
           binary_to_list(Surname),
           Amount]
       ) end,
     Accounts).
