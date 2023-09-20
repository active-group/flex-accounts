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

%% API
-export([send_event/2]).

-type eventReceiver() :: transfers | statements.

-spec send_event(server_ref(), event()) -> ok.
send_event(Server, Event) ->
  gen_server:cast(Server, Event).

multicast_event(Node, Event) ->
  send_event({'transfers', Node}, Event),
  send_event({'statements', Node}, Event).

send_event_list(Server, EventList) ->
  lists:foreach( fun(Event) -> send_event(Server, Event) end, EventList).
