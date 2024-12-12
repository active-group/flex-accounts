% Demo-Modul
-module(account_server_subscribe_demo).
-export([run_demo/0, client_process/0]).

-record(account_dto, {
    account_number :: number(),
    person_id :: number(),
    given_name :: string(),
    surname :: string(),
    amount :: number()
}).

-define(ACCOUNT_1, #account_dto{
    account_number = 10,
    person_id = 1001,
    given_name = "Alice",
    surname = "Liddell",
    amount = 500
}).

-define(ACCOUNT_2, #account_dto{
    account_number = 11,
    person_id = 1002,
    given_name = "Bob",
    surname = "Marley",
    amount = 700
}).

-define(ACCOUNT_3, #account_dto{
    account_number = 12,
    person_id = 1003,
    given_name = "Carol",
    surname = "Danvers",
    amount = 1000
}).

-define(ACCOUNT_4, #account_dto{
    account_number = 99,
    person_id = 1004,
    given_name = "Peter",
    surname = "Tosh",
    amount = 1000
}).

-define(ACCOUNT_5, #account_dto{
    account_number = 100,
    person_id = 1005,
    given_name = "John",
    surname = "Doe",
    amount = 1000
}).

client_process() ->
    receive
        {welcome, Message, NewAccounts} ->
            io:format("receive: Welcome: ~p~n", [Message]),
            io:format("receive: Welcome : New Accounts (List): ~p~n", [NewAccounts]),
            client_process();
        {account_dtos, Account} ->
            io:format("receive: Broadcast-Results (List): ~p~n", [Account]),
            client_process()
    end.

run_demo() ->

    % Accounts hinzufügen (nun als Records)
    account_server:add_account(?ACCOUNT_1),
    account_server:add_account(?ACCOUNT_2),
    account_server:add_account(?ACCOUNT_3),

    % Client starten
    ClientPid = spawn(fun client_process/0),

    % Subscriben mit letzter bekannter Account-Nummer (hier: ACCOUNT_2)
    % Erwartet wird eine Liste neuer Accounts, die nach ACCOUNT_2 liegen:
    NewAccounts = account_server:subscribe(10, ClientPid),
    io:format("Subscription: New Accounts after Subscription (List): ~p~n", [NewAccounts]),

    % Broadcast
    % Erwartet wird eine Liste von Ergebnissen, etwa [{Pid1, ok}, {Pid2, ok}, ...]:
    %BroadcastResults = account_server:broadcast({account_dtos, ?ACCOUNT_4}),
    %io:format("Broadcast: Broadcast-DIRECT Results received (List): ~p~n", [BroadcastResults]).

    account_server:add_account(?ACCOUNT_4),
    account_server:add_account(?ACCOUNT_5).
