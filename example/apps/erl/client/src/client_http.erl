-module(client_http).

-export([
  start/0,
  start/1]).

start() ->
  start(["http://localhost:8888/echo-http"]).

start([Url]) ->
  ok = application:start(inets),
  {ok, Hostname} = inet:gethostname(),
  Delay = list_to_integer(os:getenv("MESSAGE_DELAY", "10")),
  io:format("status,client,id,ts,delta\n"),
  loop(Url, Hostname, Delay, 0).

loop(Url, Hostname, Delay, Id) ->
  spawn(
    fun() ->
      StartTs = now_ts(),
      Resp = httpc:request(Url),
      Delta = now_ts() - StartTs,
      case Resp of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} ->
          io:format("success,~s,~B,~B,~B\n", [Hostname, Id, StartTs, Delta]);
        {ok, {{_Version, _InvalidHttpCode, _ReasonPhrase}, _Headers, _Body}} ->
          io:format("invalid_http_code,~s,~B,~B,~B\n", [Hostname, Id, StartTs, Delta]);
        {error, Reason} ->
          io:format("~p,~s,~B,~B,~B\n", [Reason, Hostname, Id, StartTs, Delta])
        end
    end),
  timer:sleep(Delay),
  loop(Url, Hostname, Delay, Id+1).

%% internals
now_ts() ->
  {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
  (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.
