-type money() :: number().
-type unique_id() :: integer().
-type account_number() :: integer().

-record(event, 
  {index :: non_neg_integer(),
   type :: atom(),
   content :: term()}).

-record(command,
  {type, content}).

-record(person,
    {id :: unique_id(),
     firstname :: binary(), lastname :: binary()}).

-record(account,
    {account_number :: account_number(),
     person :: #person{},
     initial_balance :: money()}).
