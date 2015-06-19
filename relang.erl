-module(relang).

-author(kureikain).
-email("kurei@axcoto.com").

%%-export([connect/1]).
-compile(export_all). %% replace with -export() later, for God's sake!

connect(RethinkDBHost) ->
  {ok, Sock} = gen_tcp:connect(RethinkDBHost, 28015,
                               [binary, {packet, 0}]),
  ok = gen_tcp:send(Sock, "Some Data"),
  Sock.

close(Sock) ->
  gen_tcp:close(Sock).

run() ->
  RethinkDBHost = "localhost", % to make it runnable on one machine
  RethinkSock   = connect(RethinkDBHost),
  close(RethinkSock).
