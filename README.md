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

ok{account_Number integer()}