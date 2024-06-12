-module(message_server).
-behaviour(gen_server). %implements "interface" - behaviour
-include("data.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    sendAck/2,
    ackReceiver_start/1,
    ackReceiver_start_link/1
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

init(State) ->
    {ok, State}.

%call: RPC
%cast: "auswerfen"  "fire-and-forget"

-spec handle_call(binary(), pid(), #state{}) -> 
    {reply, {error_not_implemented}, #state{}}.

handle_call(_R, _Pid, State) ->
    {reply, {error_not_implemented}, State}.

-spec handle_cast(#ok{}, #state{}) ->
    {noreply, #state{}}.

handle_cast(#ok{account_number = _AccountNumber}, State) ->
    {noreply, State}.

-spec handle_cast(#account_created{}, #state{}) ->
    {noreply, #state{}}.

handle_cast(#account_created{}, State) ->
    {noreply, State}.





-spec sendAck(pid(), unique_id()) -> ok.
sendAck(Pid, AccountNumber) -> 
    gen_server:cast(Pid,#ok{account_number = AccountNumber}).

-spec storeEvent(#person{}, #account{}) -> ok.
storeEvent(Person, Account) ->
    AccountCreated = #account_created{
            given_name = Person#person.given_name,
            surname = Person#person.surname,
            account_number = Account#account.account_number,
            person_id = Person#person.id,
            amount = Account#account.amount
        };
    gen_server::cast(AccountCreated)
    .


ackReceiver_start(State) ->
    gen_server:start(
        ?MODULE, 
        State, %geht an init()
        [{debug, [trace]}]
    ).

ackReceiver_start_link(State) ->
    gen_server:start_link(
        ?MODULE, 
        State, %geht an init()
        []
    ).

