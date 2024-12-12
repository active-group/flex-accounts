%%%-------------------------------------------------------------------
%% @doc erlbank_account public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_account_app).

-behaviour(application).

-export([start/2, stop/1]).

start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", web_frontend, index},
            {"/accounts/open", web_frontend, open_account}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, 8000}],
        #{env => #{dispatch => Dispatch}}
    ).

start(_StartType, _StartArgs) ->
    database:init_database(),
    {ok, Pid} = account_server:start_link(),
    start_cowboy(),
    register(account_server, Pid),
    erlbank_account_sup:start_link().

stop(_State) ->
    ok.
