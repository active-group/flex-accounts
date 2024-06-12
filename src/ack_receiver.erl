-module(ack_receiver).
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



init(State) ->
    {ok, State}.

%call: RPC
%cast: "auswerfen"  "fire-and-forget"

-spec handle_call(binary(), pid(), list()) -> 
    {reply, {error_not_implemented}, state}.

handle_call(_R, _Pid, State) ->
    {reply, {error_not_implemented}, State}.

-spec handle_cast(#ok{}, list()) ->
    {noreply, state}.

handle_cast(#ok{account_number = _AccountNumber}, State) ->
    {noreply, State}.



-spec sendAck(pid(), unique_id()) -> ok.
sendAck(Pid, AccountNumber) -> 
    gen_server:cast(Pid,#ok{account_number = AccountNumber}).


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

