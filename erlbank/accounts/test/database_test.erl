-module(database_test).
-include_lib("eunit/include/eunit.hrl").
-include("data.hrl").

setup() ->
    database:init_database().

cleanup(_) -> ok.

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun put_person/1, fun put_account/1]
     }}.

put_person(_) ->
    fun() ->
            Person = #person{id = 15, given_name = <<"Mike">>, surname = <<"Sperber">>},
            database:put_person(Person),
            ?assertEqual(database:get_person(15), {ok, Person}),
            ?assertEqual(database:get_all_persons(), [Person])
    end.

put_account(_) ->
    fun() ->
            Account = #account{account_number = 42, person_id = 17, amount = 100 },
            database:put_account(Account),
            ?assertEqual(database:get_account(42), {ok, Account}),
            ?assertEqual(database:get_all_accounts(), [Account])
    end.





