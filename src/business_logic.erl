-module(business_logic).
-include("data.hrl").
-export([dispatch/1]).


%% Opens an account, that is creates a new account containing a new person
%% Writes them into database.

-spec open_account(binary(), binary()) -> account_number().
open_account(Firstname, Lastname) ->
    Person = #person{id = database:next_person_id(),
                    firstname = Firstname,
                    lastname = Lastname},
    Account = #account{account_number = database:next_account_number(),
                        person = Person,
                        initial_balance = 1000},
    database:create_account(Account),
    Account#account.account_number.


-spec dispatch({command, open_account, {binary(), binary()}}) -> term().
dispatch({command, open_account, {Firstname, Surname}}) ->
    open_account(Firstname, Surname).
