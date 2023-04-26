
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


-spec bin_to_int(binary()) -> integer().
bin_to_int(B) ->
    erlang:list_to_integer(binary:bin_to_list(B)).


-spec amount_to_string(money(), string(), number_formatter:locale()) -> string().
amount_to_string(Amount, Currency, Format) ->
    {ok, AmountExchanged} = exchange_service:exchange(Currency, Amount),
    AmountFormatted = number_formatter:format(Format, AmountExchanged),
    AmountFormatted ++ " " ++ Currency.


%% returns the name of the person associated to the account nr
%% given by account number.
-spec name_by_account_number(unique_id()) -> string().
name_by_account_number(AccountNumber) ->
    {ok, Account} = business_logic:get_account(AccountNumber),
    name_by_account(Account).

%% returns the name of the person associated to the account
%% given by account.
-spec name_by_account(#account{}) -> string().
name_by_account(Account) ->
    {ok, Person}  = business_logic:get_person(Account#account.person_id),
    io_lib:format("~s ~s", [Person#person.given_name, Person#person.surname]).
 
head_template() ->
    "<p> Name: ~s </p>
     <p> Balance: ~s </p>
     <table>
      <tr>
        <th>ID</th>
        <th>Date</th>
        <th>Amount</th>
        <th>Sender</th>
        <th>Receiver</th>
      </tr> ".

back_button() ->
    "<a href=\"/\">Back </a>".

footer_template() ->
    "</table>" ++ back_button().


-spec head(#account{}, string(), number_formatter:locale()) -> string().
head(Account, Currency, Format) ->
    Amount = amount_to_string(Account#account.amount, Currency, Format),
    Name =  name_by_account(Account),
    io_lib:format(head_template(), [Name, Amount]).


index() ->
    io_lib:format("~s",
                  [account_open_form()]).

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
