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

-record(account_dto, {
    account_number :: number(),
    person_id :: number(),
    given_name :: string(),
    surname :: string(),
    amount :: number()
}).

-record(state, {
    % Map von Pid zu Subscription-Infos
    subscribers = #{},
    % Gesamtliste aller Accounts (Liste von #account_dto{})
    all_accounts = []
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%{debug, [trace]}

subscribe(LastAccount, ClientPid) ->
    gen_server:call(?MODULE, {subscribe, LastAccount, ClientPid}).

broadcast(Message) ->
    gen_server:call(?MODULE, {account_dtos, Message}).

add_account(NewAccount) ->
    gen_server:call(?MODULE, {add_account, NewAccount}).

init([]) ->
    {ok, #state{}}.

handle_call({subscribe, LastNumber, ClientPid}, _From, State) ->
    % Finde alle neuen Accounts, deren Nummer größer ist als LastNumber
    NewAccounts = [A || A <- State#state.all_accounts, A#account_dto.account_number > LastNumber],

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
    % Sende an alle Subscriber
    Responses = maps:fold(
        fun(Pid, _Info, Acc) ->
            try
                Pid ! {account_dtos, Account}
                %[lists:flatten(io_lib:format("Nachricht an ~p gesendet", [Pid])) | Acc]
            catch
                _:_ ->
                    [lists:flatten(io_lib:format("Fehler beim Senden an ~p", [Pid])) | Acc]
            end
        end,
        [],
        State#state.subscribers
    ),

    {reply, Responses, State};
handle_call({add_account, NewAccount}, _From, State) ->
    % Verhindere Duplikate durch Prüfung, ob der Account bereits in all_accounts vorhanden ist
    UpdatedAccounts =
        case lists:member(NewAccount, State#state.all_accounts) of
            true -> State#state.all_accounts;
            false -> [NewAccount | State#state.all_accounts]
        end,

    UpdatedState = State#state{all_accounts = UpdatedAccounts},
    {reply, ok, UpdatedState};
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
