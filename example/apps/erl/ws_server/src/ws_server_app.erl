-module(ws_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
  ws_server:start().

stop(_State) ->
  ok.
