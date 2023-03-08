-module(business_logic_test).
-include_lib("eunit/include/eunit.hrl").


setup() ->
    database:destroy_tables(),
    database:init_database(),
    timer:sleep(500).


cleanup(_) ->
    database:destroy_tables().


main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun create_account/1]
     }}.


create_account(_) ->
    fun() ->
            business_logic:dispatch({command, open_account, {"Ben", "Affleck"}}),
            business_logic:dispatch({command, open_account, {"Matt", "Damon"}}),
            PersonEvents = database:get_all_events(new_person_event),
            AccountEvents = database:get_all_events(new_account_event),
            ?assertEqual(length(PersonEvents), 2),
            ?assertEqual(length(AccountEvents), 2)
    end.
