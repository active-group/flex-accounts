
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

## 

How to get missing events:

* use service name "accounts"
* request missing events since eventNumber
  -> cast with following type "get_account_events_since"

```erlang
-record(get_account_events_since, {
      since :: unique_id(),
      receiver_pid :: pid()
}).
```
