
-module(web_frontend).
-include("data.hrl").
-export([init/2]).



account_opened_success() ->
    << "
      <p> Account with account number ~p was opened successfully </p> ~n
       <a href=\"/\"> Back </a>
    " >>.

account_deleted_success() ->
  << "
      <p> Account with account number ~p was deleted successfully </p> ~n
       <a href=\"/\"> Back </a>
    " >>.

account_open_form() ->
            << "
<h3> Open Account </h3>
               <form method=\"post\" action=\"/accounts/open\">
  <label for=\"accounts_givenName\"> Given Name </label>
  <input type=\"text\" id=\"accounts_givenName\" name=\"accounts_givenName\" />

  <label for=\"accounts_surname\"> Surname </label>
  <input type=\"text\" id=\"accounts_surname\" name=\"accounts_surname\" />

  <input type=\"submit\" value=\"Open account\" />
</form>" >>.

account_delete_form() ->
  << "
<h3> Delete Account </h3>
<form method=\"post\" action=\"/accounts/delete\">
  <label for=\"account_number\"> Account Number </label>
  <input type=\"text\" id=\"account_number\" name=\"account_number\" />

  <input type=\"submit\" value=\"Delete account\" />
</form>" >>.

index() ->
  io_lib:format("~s~s~s",
    [account_open_form(), account_delete_form(), account_list()]).

%% /accounts/open
init(Request, open_account) ->

    logger:info("Creating new account ~p", [Request]),

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Request),

    KeyValues = maps:from_list(KeyValuesL),
    GivenName = maps:get(<<"accounts_givenName">>, KeyValues),
    Surname = maps:get(<<"accounts_surname">>, KeyValues),

    Account = business_logic:open_account(GivenName, Surname),
    Body = io_lib:format(account_opened_success(), [Account#account.account_number]),

    Reply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Request),

    logger:info("Created account with account number ~p", [Account#account.account_number]),

    {ok, Reply, []};

%% /accounts/delete
init(Request, delete_account) ->

  logger:info("Deleting account ~p", [Request]),

  {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Request),

  KeyValues = maps:from_list(KeyValuesL),
  AccountNumber = binary_to_integer(maps:get(<<"account_number">>, KeyValues)),

  business_logic:delete_account(AccountNumber),
  Body = io_lib:format(account_deleted_success(), [AccountNumber]),

  Reply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Request),

  logger:info("Deleted account with account number ~p", [AccountNumber]),

  {ok, Reply, []};

%% /index
init(Request, index) ->
    Reply = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           index(),
                           Request),
    {ok, Reply, []}.


account_list_template() -> "<h3> Accounts: </h3>
<table>
    <tr>
      <th>Account-ID</th>
      <th>Person-ID</th>
      <th>GivenName</th>
      <th>Surname</th>
    </tr>
    ~s
    </table>".
render_account_list(Accounts) ->
  io_lib:format(account_list_template(),
    [lists:foldl(fun(Account, Acc) ->
      #account{person_id = Person_id} = Account,
      {ok, Person} = business_logic:get_person(Person_id),
      Acc ++ render_account(Account, Person) end, "", Accounts)]).

account_template() -> "
    <tr>
      <td>~p</td>
      <td>~p</td>
      <td>~p</td>
      <td>~p</td>
    </tr> ".
render_account(#account{account_number = Account_number, person_id = Person_id},
    #person{given_name = Given_Name, surname = Surname}) ->
  io_lib:format(account_template(), [Account_number, Person_id,  binary_to_list(Given_Name), binary_to_list(Surname)]).

account_list() ->
  Accounts = business_logic:get_all_accounts(),
  render_account_list(Accounts).
