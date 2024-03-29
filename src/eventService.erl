%%%-------------------------------------------------------------------
%%% @author ferenc.noack
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2023 16:01
%%%-------------------------------------------------------------------
-module(eventService).
-author("ferenc.noack").
-include("data.hrl").
-include("events.hrl").

%% API
-export([start/0, handle_cast/2, handle_call/3, init/1, trigger/2]).

-behaviour(gen_server).

-spec start() -> pid().
start() ->
  {ok, Pid} = gen_server:start(
    eventService,
    egal, % -> init
    [{debug, [trace]}]),
  register(accounts, Pid),
  logger:info("My PID: ~p", [Pid]),
  Pid.

% läuft im Server-Prozeß, cf. self()
-spec init(atom()) -> {ok, node()}.
init(Node) -> {ok, Node}.

-spec handle_cast(#get_account_events_since{}, node()) -> {noreply, node()}.
handle_cast(#get_account_events_since{} = Request, Node) ->
  logger:info("Received cast: ~p", [Request]),
  {noreply, process_request_message(Node, Request)};
handle_cast(Request, Node) ->
  logger:info("Received unnknow cast: ~p", [Request]),
  {noreply, Node}.

-spec handle_call(#get_account_events_since{}, term(), node()) -> {reply, [], node()}.
handle_call(Request, _Pid, Node) ->
  logger:info("Received call: ~p", [Request]),
  {reply, [], Node}.

-spec process_request_message(atom(), #get_account_events_since{}) -> node().
process_request_message(Node, #get_account_events_since{since = Since, receiver_pid = Receiver_Pid}) ->
  EventList = events:get_events_from(Since),
  logger:info("Will send ~p events.", [length(EventList)]),
  PayloadList = lists:map(
    fun(#eventDB{payload = Payload}) ->
      Payload end,
    EventList),
  lists:foreach(
    fun(Payload) ->
      logger:info("Send event to ~p with payload ~p", [Receiver_Pid, Payload]),
      gen_server:cast(Receiver_Pid, Payload) end,
    PayloadList),
  Node.

trigger(Pid, Since) ->
  gen_server:cast(Pid, #get_account_events_since{since = Since, receiver_pid = self()}).
