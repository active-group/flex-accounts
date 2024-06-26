
-module(web_frontend).
-include("data.hrl").
-export([init/2]).





account_opened_success() ->
    << "
      <p> Account with account number ~p was opened successfully </p> ~n
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


index() ->
    io_lib:format("~s",
                  [account_open_form()]
                   ).

%% /accounts/open
init(Request, open_account) ->

    logger:info("Creating new account"),

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Request),

    KeyValues = maps:from_list(KeyValuesL),
    GivenName = maps:get(<<"accounts_givenName">>, KeyValues),
    Surname = maps:get(<<"accounts_surname">>, KeyValues),

    Account = business_logic:open_account(GivenName, Surname),
    Body = io_lib:format(account_opened_success(), [Account#account.account_number]),

    Reply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Request),

    logger:info("Created account with account number ~p", [Account#account.account_number]),

    {ok, Reply, []};

%% /index
init(Request, index) ->
    Reply = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           index(),
                           Request),
    {ok, Reply, []}.
