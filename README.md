
## Event definitions

We send the following events:

```erlang
-type unique_id() :: integer().
-type account_number() :: integer().
-type eventType() :: account_created.

-record(event,
    {id :: unique_id(),
     eventType :: eventType(),
     account_number :: account_number(),
     givenName :: string(),
     surname :: string()}).
```
