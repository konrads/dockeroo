-module(echo_http_handler).

-behaviour(cowboy_http_handler).

-export([
  init/3,
  handle/2,
  terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [], <<"ok">>, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
