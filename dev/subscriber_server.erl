-module(subscriber_server).
-behavior(gen_server).

-export([
    start_link/0,
    subscribe/2,
    broadcast/2,
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

-record(state, {
    % Map von Pid zu Subscription-Infos
    subscribers = #{},
    % Gesamtliste aller Accounts
    all_accounts = []
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe(LastAccountNumber, ClientPid) ->
    gen_server:call(?MODULE, {subscribe, LastAccountNumber, ClientPid}).

broadcast(Message, _Pid) ->
    gen_server:call(?MODULE, {broadcast, Message}).

add_account(NewAccount) ->
    gen_server:call(?MODULE, {add_account, NewAccount}).

init([]) ->
    {ok, #state{}}.

handle_call({subscribe, LastAccountNumber, ClientPid}, _From, State) ->
    % Monitor für den Client erstellen
    MonitorRef = erlang:monitor(process, ClientPid),

    % Finde Accounts nach der LastAccountNumber
    NewAccountsSinceLastNumber =
        lists:dropwhile(
            fun(Account) -> Account =/= LastAccountNumber end, State#state.all_accounts
        ),
    NewAccounts = tl(NewAccountsSinceLastNumber),

    % Sende Willkommensnachricht
    InitialMessage = {welcome, "Subscription erfolgreich", NewAccounts},
    ClientPid ! InitialMessage,

    % Aktualisiere Subscribers
    UpdatedState = State#state{
        subscribers = maps:put(ClientPid, #{monitor => MonitorRef}, State#state.subscribers)
    },

    {reply, NewAccounts, UpdatedState};
handle_call({broadcast, Message}, _From, State) ->
    % Sende an alle Subscriber
    Responses = maps:fold(
        fun(Pid, _, Acc) ->
            try
                Pid ! {broadcast, Message},
                [lists:flatten(io_lib:format("Nachricht an ~p gesendet", [Pid])) | Acc]
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
    % Verhindere Duplikate
    UpdatedAccounts =
        case lists:member(NewAccount, State#state.all_accounts) of
            true -> State#state.all_accounts;
            false -> [NewAccount | State#state.all_accounts]
        end,

    UpdatedState = State#state{all_accounts = UpdatedAccounts},
    {reply, ok, UpdatedState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) ->
    % Entferne disconnecteten Client
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
