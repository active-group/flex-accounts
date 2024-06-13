# Erlbank Monolithic

Erlbank Legacy System

## Build

```
$ rebar3 compile
```


## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

The web-frontend is served at http://localhost:8000/


## dinge
net_adm:ping('transfers@PF2PMRC0'). 
client:open_account("Vor","nach").

## events

### account_created
{
person_id  : integer()
account_number: integer()
given_name: binary()
surname : binary()
amount: number()
}

### account_number

-record(ok, {sender :: binary(), account_number :: unique_id()}).