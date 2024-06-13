-module(message_server).
-behaviour(gen_server). %implements "interface" - behaviour
-include("data.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    sendAck/1,
    message_server_start/0,
    message_server_start_link/0,
    storeEvent/2
]).





-record(ok, {account_number :: unique_id()}).


-record(state, {messagesToStatements :: list(), messagesToTranfer :: list}).

-record(account_created,
    {
     given_name :: binary(),
     surname :: binary(),
     account_number :: account_number(),
     person_id :: unique_id(),
     amount :: money()}
    ).


-spec init(#state{}) -> {ok, #state{}}.

init(_) ->
    State = #state{},
    {ok, State}.

%call: RPC
%cast: "auswerfen"  "fire-and-forget"

-spec handle_call(binary(), pid(), #state{}) -> 
    {reply, {error_not_implemented}, #state{}}.

handle_call(_R, _Pid, State) ->
    {reply, {error_not_implemented}, State}.

-spec handle_cast(#ok{}|#account_created{}, #state{}) ->
    {noreply, #state{}}.


handle_cast(#ok{account_number = _AccountNumber}, State) ->
    {noreply, State};
handle_cast(#account_created{} = Event, State) ->

    {noreply, State#state{
        messagesToStatements = [ Event| messagesToStatements], 
        messagesToTranfer = [ Event| messagesToTranfer]
    }}.

-spec sendAck(unique_id()) -> ok.
sendAck(AccountNumber) -> 
    gen_server:cast(accounts,#ok{account_number = AccountNumber}).

-spec storeEvent(#person{}, #account{}) -> ok.
storeEvent(Person, Account) ->
    AccountCreated = #account_created{
            given_name = Person#person.given_name,
            surname = Person#person.surname,
            account_number = Account#account.account_number,
            person_id = Person#person.id,
            amount = Account#account.amount
        },
    logger:info("storeEvent "),
    
    gen_server:cast(accounts,AccountCreated)
    .


message_server_start() ->
    gen_server:start(
        ?MODULE, 
        [], %geht an init()
        [{debug, [trace]}]
    ).

message_server_start_link() ->
    gen_server:start_link(
        ?MODULE, 
        [], %geht an init()
        [{debug, [trace]}]
    ).

