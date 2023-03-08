-module(client).
-include("data.hrl").
-export([start/0, open_account/2]).

%% opens an acocunt with a given first and surname.
%% prints the result and the account number to stdout.
-spec open_account(string(), string()) -> ok.
open_account(Firstname, Surname) ->
    AccountNumber = business_logic:dispatch(
                      #command{type = open_account,
                               content = {list_to_binary(Firstname),
                                          list_to_binary(Surname)}}),
    io:format("Account was successfully opened. Account number: ~p ~n", [AccountNumber]).

start() ->
	ok.
