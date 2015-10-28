-module(client_ws).

-include("client.hrl").

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

-define(ID_LENGTH, 1000).

-record(ws_client_ctx, {
  hostname,
  job_cnt=0,
  jobs=#{},
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
  io:format("status,client,id,ts,delta\n"),
  {reconnect, Ctx}.

onconnect(_ConnState, Ctx) ->
  self() ! trigger,
  % websocket_client:cast(self(), trigger),
  {ok, Ctx}.

ondisconnect(_ConnState, Ctx) ->
  {reconnect, Ctx}.

websocket_handle({binary, <<ReplyId:?ID_LENGTH>>}, _ConnState, #ws_client_ctx{jobs=Jobs, hostname=Hostname}=Ctx) ->
  EndTs = now_ts(),
  Ctx2 = case Jobs of
    #{ReplyId := StartTs} ->
      io:format("success,~s,~B,~b,~b\n", [Hostname, ReplyId, EndTs, EndTs-StartTs]),
      Jobs2 = maps:remove(ReplyId, Jobs),
      Ctx#ws_client_ctx{jobs=Jobs2};
    _ ->
      io:format("invalid_id,~s,~B,~b,-1\n", [Hostname, ReplyId, EndTs]),
      Ctx
  end,
  {ok, Ctx2};
websocket_handle(_, _ConnState, Ctx) ->
  {ok, Ctx}.

websocket_info(trigger, _ConnState, #ws_client_ctx{jobs=Jobs, job_cnt=JobCnt, delay=Delay}=Ctx) ->
  erlang:send_after(Delay, self(), trigger),
  Jobs2 = Jobs#{JobCnt => now_ts()},
  {reply, {binary, <<JobCnt:?ID_LENGTH>>}, Ctx#ws_client_ctx{jobs=Jobs2, job_cnt=JobCnt+1}};
websocket_info(_, _ConnState, Ctx) ->
  {ok, Ctx}.

websocket_terminate(_Reason, _ConnState, _Ctx) ->
  ok.

%% internals
now_ts() ->
  {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
  (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.
