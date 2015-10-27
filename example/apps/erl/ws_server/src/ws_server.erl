%% Simple echo server
-module(ws_server).

-behaviour(cowboy_websocket_handler).

%% public API
-export([
  start/0,
  start/1
]).

%% handler
-export([
  init/3,
  websocket_init/3,
  websocket_handle/3,
  websocket_info/3,
  websocket_terminate/3]).

% to be started once only!
start([]) ->
  start().

start() ->
  application:ensure_all_started(cowboy),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/echo", ?MODULE, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8888}],
    [{env, [{dispatch, Dispatch}]}]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, no_state}.

websocket_handle({binary, JobId}, Req, State) ->
  {reply, {binary, JobId}, Req, State};
websocket_handle(_Data, Req, State) ->
  io:format("Handling unknown data: ~p", [_Data]),
  {ok, Req, State}.

websocket_info(_Info, Req, State) ->
  io:format("Handling unknown info: ~p", [_Info]),
  {ok, Req, State}.

websocket_terminate({error, closed}, Req, _State) ->
  {Host, _Req} = cowboy_req:host(Req),
  io:format("Terminating websocket to ~s\n", [Host]),
  ok;
websocket_terminate(Reason, Req, _State) ->
  io:format("Terminating websocket due to: ~p\nReq: ~p\n", [Reason, Req]),
  ok.
