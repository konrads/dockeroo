#!/usr/bin/env/ escript

%% ensure code path is setup, eg: -pa <path_to_folsom> -pa <path_to_bear>

main([Cookie, TargetNode, MetricType, MetricId]) ->
  Cookie2 = list_to_atom(Cookie),
  TargetNode2 = list_to_atom(TargetNode),
  MetricType2 = list_to_atom(MetricType),
  MetricId2 = list_to_binary(MetricId),
  [_Target, Host] = string:tokens(TargetNode, "@"),
  ThisNode = list_to_atom("folsom_query@" ++ Host),

  % establish connection
  {ok, _} = net_kernel:start([ThisNode, longnames]),
  erlang:set_cookie(node(), Cookie2),
  case net_adm:ping(TargetNode2) of
  	pong -> ok;
  	pang ->
  	  io:format("Failed to connect to ~p", [TargetNoe2]),
  	  halt(1)
  end,

  % request folsom value
  io:format("~B", [get_metric(TargetNode2, MetricType, MetricId)]).


get_metric(Node, counter, MetricId) ->
  rpc:call(Node, folsom_metric_counter, get_value, [MetricId]).