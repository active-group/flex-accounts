# Schnittstelle
Module: account_server
GenServer: account_server

Subscribe: handle_call({subscribe, LastAccountNumber:number(), ClientPid})
    Return: [ account_dto ] Liste of account_dto

-record(account_dto, {
    account_number :: number(),
	person_id :: number(),
    given_name :: string(),
    surname :: string()
    amount :: number()
}).


client_process() ->
    receive       
        {account_dtos, Account_Dtos} ->
            io:format("Account_Dtos: ~p~n", [Account_Dtos])
    end.




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
