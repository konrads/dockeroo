all: erl-client erl-echo-server scala-echo-server

erl-client:
	cd erl/client && rebar get-deps compile

erl-echo-server:
	cd erl/echo_server && rebar get-deps compile

scala-echo-server:
	cd scala/echo_server && sbt compile
