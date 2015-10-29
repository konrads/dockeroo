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
  io:format("status,client,req_id,ts\n"),
  loop(Url, Hostname, Delay, 0).

loop(Url, Hostname, Delay, Id) ->
  spawn(
    fun() ->
      StartTs = now_ts(),
      io:format("start,~s,~b,~b\n", [Hostname, Id, StartTs]),
      Resp = httpc:request(Url),
      EndTs = now_ts(),
      case Resp of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} ->
          io:format("end,~s,~b,~b\n", [Hostname, Id, EndTs]);
        {ok, {{_Version, _InvalidHttpCode, _ReasonPhrase}, _Headers, _Body}} ->
          io:format("invalid_http_code,~s,~b,~b\n", [Hostname, Id, EndTs]);
        {error, Reason} ->
          io:format("~p,~s,~b,~b\n", [Reason, Hostname, Id, EndTs])
        end
    end),
  timer:sleep(Delay),
  loop(Url, Hostname, Delay, Id+1).

%% internals
now_ts() ->
  {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
  (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.
