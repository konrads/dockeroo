-module(ws_client_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
  % {ok, _} = crypto:start(),
  % {ok, _} = ssl:start(),
  ws_client:start_link().

stop(_State) ->
  ok.
