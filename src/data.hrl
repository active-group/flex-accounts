-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(person, {
    id :: unique_id(),
    given_name :: binary(),
    surname :: binary()
}).
-record(account, {
    account_number :: account_number(),
    person_id :: unique_id(),
    amount :: money()
}).
