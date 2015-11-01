%% HTTP/WS server.
-module(echo_server).

%% public API
-export([
  start/0,
  start/1
]).

% for command line
start([]) ->
  start().

start() ->
  application:ensure_all_started(cowboy),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/echo-ws", echo_ws_handler, []},
      {"/echo-http", echo_http_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8888}],
    [{env, [{dispatch, Dispatch}]}]).
