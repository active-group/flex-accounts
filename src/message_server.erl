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
    storeEvent/2,
    sendEvents/3,
    handle_info/2
]).





-record(ok, {sender :: atom(), account_number :: unique_id()}).
-record(send, {}).


-record(state, {messagesToStatements :: list(), messagesToTransfer :: list}).

-record(account_created,
    {
     account_number :: account_number(),
     given_name :: binary(),
     surname :: binary(),
     amount :: money(),
     person_id :: unique_id()
     }
    ).


-spec init(#state{}) -> {ok, #state{}}.

init(_) ->
    State = #state{
        messagesToStatements = [],
        messagesToTransfer = []
    },
    Interval = 5000,
    timer:send_interval(Interval,interval),
    {ok, State}.


handle_info(interval, #state{
        messagesToStatements = MessagesToStatements,
        messagesToTransfer = MessagesToTransfer
    } = State) ->
    sendEvents(transfers, "TRANSFERS_HOST", MessagesToTransfer),
    sendEvents(statements,"STATEMENT_HOST",MessagesToStatements),
    {noreply, State}
.

%call: RPC
%cast: "auswerfen"  "fire-and-forget"

-spec handle_call(binary(), pid(), #state{}) -> 
    {reply, {error_not_implemented}, #state{}}.

handle_call(_R, _Pid, State) ->
    {reply, {error_not_implemented}, State}.

-spec handle_cast(#ok{}|#account_created{}|#send{}, #state{}) ->
    {noreply, #state{}}.


handle_cast(#ok{sender = transfers, account_number = AccountNumber}, State) ->
    Pred = fun(#account_created{account_number = InternalAccountNumber}) -> AccountNumber /= InternalAccountNumber end,
    NewMessagesToTransfer = lists:filter(Pred, State#state.messagesToTransfer),
    NewState = State#state{messagesToTransfer = NewMessagesToTransfer},
    {noreply, NewState};

handle_cast(#ok{sender = statements, account_number = AccountNumber}, State) ->
    Pred = fun(#account_created{account_number = InternalAccountNumber}) -> AccountNumber /= InternalAccountNumber end,
    NewMessagesToStatements = lists:filter(Pred, State#state.messagesToStatements),
    NewState = State#state{messagesToStatements = NewMessagesToStatements},
    {noreply, NewState};


handle_cast(#send{}, State) ->
    {noreply, State};




handle_cast(#account_created{} = Event, State) ->

    {noreply, State#state{
        messagesToStatements = [ Event| State#state.messagesToStatements], 
        messagesToTransfer = [ Event| State#state.messagesToTransfer]
    }}.

-spec sendAck(unique_id()) -> ok.
sendAck(AccountNumber) -> 
    gen_server:cast(accounts,#ok{account_number = AccountNumber}).

-spec storeEvent(#person{}, #account{}) -> ok.
storeEvent(Person, Account) ->
    AccountCreated = #account_created{
            account_number = Account#account.account_number,
            given_name = Person#person.given_name,
            surname = Person#person.surname,
            amount = Account#account.amount,
            person_id = Person#person.id
        },
    logger:info("storeEvent "),
    
    gen_server:cast(accounts,AccountCreated)
    .

- spec sendEvents(string(),string(),list()) -> ok.

sendEvents(_,_,[]) -> 
    ok;
sendEvents(Target, ENV_HOSTNAME, [First|Rest]) -> 
    gen_server:cast({Target,node_util:node_from_env(Target, ENV_HOSTNAME)}, First),
    sendEvents(Target,ENV_HOSTNAME,Rest)
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

