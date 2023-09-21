-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().
-type eventType() :: account_created. % | account_deleted.


-record(get_account_events_since, {
    since :: unique_id(),
    receiver_pid :: pid()}).
-record(person,
    {id :: unique_id(),
     given_name :: binary(),
     surname :: binary()}).
-record(account,
    {account_number :: account_number(),
     person_id :: unique_id()}).
-record(event,
    {id :: unique_id(),
     eventType:: eventType(),
     account_number:: account_number(),
     givenName:: string(),
     surname:: string()}).

