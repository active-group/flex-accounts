-module(feed_test).
-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").


setup() ->
    database:destroy_tables(),
    database:init_database(),
    business_logic:dispatch({command, open_account, {"Ben", "Affleck"}}),
    business_logic:dispatch({command, open_account, {"Matt", "Damon"}}).

cleanup(_) ->
    database:destroy_tables().


main_test_() ->
    {inorder,
      {foreach,
      fun setup/0,
      fun cleanup/1,
      [
       fun consume_acc_from_beginning/1,
       fun consume_acc_from_offset/1,
       fun consume_no_acc/1
      ]}}.


consume_acc_from_beginning(_) ->
    fun() ->
            {reply, Events, _} = feed:handle_call({events, 1}, bla, []),
            ?assertEqual(length(Events), 4)
    end.


consume_acc_from_offset(_) ->
    fun() ->
            {reply, Events, _} = feed:handle_call({events, 3}, bla, []),
            ?assertEqual(length(Events), 2)
    end.


consume_no_acc(_) ->
    fun() ->
            {reply, Events, _} = feed:handle_call({events, 5}, bla, []),
            ?assertEqual(length(Events), 0)
    end.
