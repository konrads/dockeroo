%% WS client, auto-reconnecting, pushing a binary job counter on periodic basis.
-module(client_ws).

-behaviour(websocket_client).

-export([
  start/0,
  start/1,
  init/1,
  ondisconnect/2,
  onconnect/2,
  websocket_handle/3,
  websocket_info/3,
  websocket_terminate/3
]).

%% length of the encoded msg/reply
-define(MSG_LENGTH, 1000).

-record(ws_client_ctx, {
  hostname,
  job_cnt=0,
  delay=10
}).

start() ->
  start(["ws://localhost:8888/echo-ws"]).

start([Url]) ->
  websocket_client:start_link(Url, ?MODULE, []).

init([]) ->
  {ok, Hostname} = inet:gethostname(),
  Delay = list_to_integer(os:getenv("MESSAGE_DELAY", "10")),
  Ctx = #ws_client_ctx{hostname=Hostname, delay=Delay},
  io:format("status,client,req_id,ts\n"),
  self() ! trigger,
  {reconnect, Ctx}.

onconnect(_ConnState, Ctx) ->
  {ok, Ctx}.

ondisconnect(_ConnState, Ctx) ->
  {reconnect, Ctx}.

websocket_handle({binary, <<ReplyJobId:?MSG_LENGTH>>}, _ConnState, #ws_client_ctx{hostname=Hostname}=Ctx) ->
  io:format("end,~s,~b,~b\n", [Hostname, ReplyJobId, now_ts()]),
  {ok, Ctx};
websocket_handle(_, _ConnState, Ctx) ->
  {ok, Ctx}.

websocket_info(trigger, _ConnState, #ws_client_ctx{job_cnt=JobCnt, hostname=Hostname, delay=Delay}=Ctx) ->
  erlang:send_after(Delay, self(), trigger),
  io:format("start,~s,~b,~b\n", [Hostname, JobCnt, now_ts()]),
  {reply, {binary, <<JobCnt:?MSG_LENGTH>>}, Ctx#ws_client_ctx{job_cnt=JobCnt+1}};
websocket_info(_, _ConnState, Ctx) ->
  {ok, Ctx}.

websocket_terminate(_Reason, _ConnState, _Ctx) ->
  ok.

%% internals
now_ts() ->
  {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
  (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.
