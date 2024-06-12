-module(push_service).
-include("data.hrl").

-export([sendEvent/2]).


-record(account_created,
    {
     given_name :: binary(),
     surname :: binary(),
     account_number :: account_number(),
     person_id :: unique_id(),
     amount :: money()}
    ).



-spec sendEvent(#person{}, #account{}) -> ok.
sendEvent(Person, Account) -> 
    AccountCreated = #account_created{
            given_name = Person#person.given_name,
            surname = Person#person.surname,
            account_number = Account#account.account_number,
            person_id = Person#person.id,
            amount = Account#account.amount
        },
        transfers ! AccountCreated,
        statements ! AccountCreated,
        ok.
