dockeroo
========

Docker based tests. Organized in:
* bin - holds runnable framework scripts
* example - holds docker based tests, as an example - WS client/server
  * ws_server is started via: ERL_LIBS=deps erl -pa ebin -s ws_server
  * ws_client is started via: ERL_LIBS=deps erl -pa ebin -run ws_client start_link ws://localhost:8888/echo
