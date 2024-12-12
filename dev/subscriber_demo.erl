% Demo-Modul
-module(subscriber_demo).
-export([run_demo/0, client_process/0]).

client_process() ->
    receive
        {welcome, Message, NewAccounts} ->
            io:format("Willkommen: ~p~n", [Message]),
            io:format("Neue Accounts: ~p~n", [NewAccounts]);
        {broadcast, Message} ->
            io:format("Broadcast: ~p~n", [Message])
    end.

run_demo() ->
    % Server starten
    subscriber_server:start_link(),

    % Accounts hinzufügen
    subscriber_server:add_account("ACCOUNT_2023_12_01"),
    subscriber_server:add_account("ACCOUNT_2023_12_02"),
    subscriber_server:add_account("ACCOUNT_2024_01_01"),

    % Client starten
    ClientPid = spawn(fun client_process/0),

    % Subscriben mit letzter bekannter Account-Nummer
    NewAccounts = subscriber_server:subscribe("ACCOUNT_2023_12_02", ClientPid),
    io:format("Neue Accounts nach Subscription: ~p~n", [NewAccounts]),

    % Broadcast
    BroadcastResults = subscriber_server:broadcast({notification, "Test"}),
    io:format("Broadcast-Ergebnisse: ~p~n", [BroadcastResults]).
