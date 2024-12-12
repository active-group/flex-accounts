# Schnittstelle
Module: account_server
GenServer: account_server

Subscribe: handle_call({subscribe, LastAccountNumber:number(), ClientPid})
    Return: [ account_dto ] Liste of account_dto > LastAccountNumber


      gen_server:call(account_server, {subscribe, 42, ClientPid}).

-type unique_id() :: integer().
-record(account_dto, {
    account_number :: number(),
    person_id :: unique_id(),
    given_name :: binary(),
    surname :: binary(),
    amount :: number()
}).


client_process() ->
    receive       
        {account_dtos, Account_Dtos} ->
            io:format("Account_Dtos: ~p~n", [Account_Dtos])
    end.

## Ideen

### Annahme: 
Subscription kann z.B. alle 30s wiederholt werden. 

### Subscription handling
   -> mit parameter 'ich habe die Accounts bis Accountnumber X' + Subscriber PID
 1. Fall X kleiner Current-Accountnumber:
 Antwort: delta(X bis Current-Accountnumber).
 2. Fall X >= Current-Accountnumber:
 Antwort: keine. Weil Asyncron
 Struktur:  
 Antwort-Format: {}

### Aliveness-Problematik 
 Bei 1 u. 2
 Merken von Subscriber-PID in Liste.
 A. Wenn PID bereits bekannt, keine aufnahme ins Monitoring aber antworten wie bei 1. 
 B. Wenn PID nicht bekannt, aufnahme ins Monitoring und antworten wie bei 1. 
 Bei Monitor-Fail: entfernen aus PID-Liste.
### Mitbekommen von Account-Neuanlagen
 Bei Accountneuanlage: Antwort an alle in PID-Liste mit neuem Account.


# Erlbank account

Erlbank Legacy System

## Build

```
$ rebar3 compile
```

## Check

You can run the dialyzer via:

```
$ rebar3 dialyzer
```

## Test

You can run the tests in the `tests/` directory via:

```
$ rebar3 eunit
```

## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

You can set a short name via:

```
$ rebar3 shell --sname=accounts
```

The web-frontend is served at http://localhost:8000/
