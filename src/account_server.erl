-module(account_server).
-behavior(gen_server).

-export([
    start_link/0,
    subscribe/2,
    broadcast/1,
    add_account/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(account_dto, {
    account_number :: number(),
    person_id :: unique_id(),
    given_name :: binary(),
    surname :: binary(),
    amount :: number()
}).

-record(person, {
    id :: unique_id(),
    given_name :: binary(),
    surname :: binary()
}).
-record(account, {
    account_number :: account_number(),
    person_id :: unique_id(),
    amount :: money()
}).

-record(state, {
    % Map von Pid zu Subscription-Infos
    subscribers = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%{debug, [trace]}

subscribe(LastAccount, ClientPid) ->
    gen_server:call(?MODULE, {subscribe, LastAccount, ClientPid}).

broadcast(Account) ->
    gen_server:call(?MODULE, {account_dtos, Account}).

add_account(NewAccount) ->
    gen_server:call(?MODULE, {add_account, NewAccount}).

init([]) ->
    {ok, #state{}}.

create_account_dtos(LastNumber) ->
    Accounts = database:get_accounts_larger_than(LastNumber),
    lists:map(fun build_account_dto/1, Accounts).

build_account_dto(Account) ->
    {ok, Person} = database:get_person(Account#account.person_id),
    #account_dto{
        account_number = Account#account.account_number,
        person_id = Account#account.person_id,
        given_name = Person#person.given_name,
        surname = Person#person.surname,
        amount = Account#account.amount
    }.

handle_call({subscribe, LastNumber, ClientPid}, _From, State) ->
    % Finde alle neuen Accounts, deren Nummer größer ist als LastNumber
    NewAccounts = create_account_dtos(LastNumber),

    % Sende Willkommensnachricht und neue Accounts
    InitialMessage = {welcome, "Subscription erfolgreich", NewAccounts},
    ClientPid ! InitialMessage,

    % Monitor für den Client erstellen, um bei Abbruch informiert zu werden
    MonitorRef = erlang:monitor(process, ClientPid),

    % Subscriber speichern
    UpdatedState = State#state{
        subscribers = maps:put(ClientPid, #{monitor => MonitorRef}, State#state.subscribers)
    },

    {reply, NewAccounts, UpdatedState};
handle_call({account_dtos, Account}, _From, State) ->
    Responses = broadcast_internal(State#state.subscribers, Account),
    {reply, Responses, State};
handle_call({add_account, NewAccount}, _From, State) ->
    % Verhindere Duplikate durch Prüfung, ob der Account bereits in all_accounts vorhanden ist
    %UpdatedAccounts =
    %    case lists:member(NewAccount, State#state.all_accounts) of
    %        true -> State#state.all_accounts;
    %        false -> [NewAccount | State#state.all_accounts]
    %    end,

    %UpdatedState = State#state{all_accounts = UpdatedAccounts},
    broadcast_internal(State#state.subscribers, NewAccount),
    {reply, {ok, NewAccount}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    % Entferne disconnected Client aus subscribers
    UpdatedState = State#state{
        subscribers = maps:remove(Pid, State#state.subscribers)
    },
    {noreply, UpdatedState};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

broadcast_internal(Subscribers, Account) ->
    maps:fold(
        fun(Pid, _Info, Acc) ->
            try
                Pid ! {account_dtos, Account},
                [lists:flatten(io_lib:format("Nachricht an ~p gesendet", [Pid])) | Acc]
            catch
                _:_ ->
                    [lists:flatten(io_lib:format("Fehler beim Senden an ~p", [Pid])) | Acc]
            end
        end,
        [],
        Subscribers
    ).
