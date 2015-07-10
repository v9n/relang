-module(relang).

-author(kureikain).
-email("kurei@axcoto.com").

%%-export([connect/1]).
-compile(export_all). %% replace with -export() later, for God's sake!

%% From ql2.proto
-define(RETHINKDB_VERSION, 32#723081e1).

-include("ql2_pb.hrl").

start() ->
  application:start(relang),
  ok.

stop() ->
  application:stop(relang),
  ok.

%% http://erlang.org/pipermail/erlang-questions/2004-December/013734.html
connect(RethinkDBHost) ->
  {ok, Sock} = gen_tcp:connect(RethinkDBHost, 28015,
                               [binary, {packet, 0}, {active, false}]),
  handshake(Sock, <<"">>),
  Sock.

close(Sock) ->
  gen_tcp:close(Sock).

handshake(Sock, AuthKey) ->
  KeyLength = iolist_size(AuthKey),
  ok = gen_tcp:send(Sock, binary:encode_unsigned(16#400c2d20, little)),
  %%ok = gen_tcp:send(Sock, [<<KeyLength:32/little-unsigned>>, AuthKey]),
  ok = gen_tcp:send(Sock, [<<0:32/little-unsigned>>]),
  % Using JSON Protocol
  ok = gen_tcp:send(Sock, [<<16#7e6970c7:32/little-unsigned>>]),

  {ok, Response} = read_until_null(Sock),
  case Response == <<"SUCCESS",0>> of
    true -> ok;
    false ->
      io:fwrite("Error: ~s~n", [Response]),
      {error, Response}
  end.

query(Socket, RawQuery) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  Token = random:uniform(3709551616),
  io:format("QueryToken = ~p~n", [Token]),
  Query = relang_ast:make(RawQuery),
  io:format("Query = ~p ~n", [Query]),
  Iolist  = ["[1,["] ++ Query ++ ["],{}]"], % list db 
  Length = iolist_size(Iolist),
  io:format("Query= ~p~n", [Iolist]),
  io:format("Length: ~p ~n", [Length]),

  case gen_tcp:send(Socket, [<<Token:64/little-unsigned>>, <<Length:32/little-unsigned>>, Iolist]) of
    ok -> ok;
    {error, Reason} ->
      io:fwrite("Got Error when sedning query: ~s ~n", [Reason]),
      {error, Reason}
  end,

  case recv(Socket) of
    {ok, Response} ->
      io:format("Ok "),
      io:format(Response),
      Response
      ;
    {error, ErrReason} ->
      io:fwrite("Got Error when receving: ~s ~n", [ErrReason]),
      ErrReason
  end
  .


%% Receive data from Socket
%%Once the query is sent, you can read the response object back from the server. The response object takes the following form:
%%
%% * The 8-byte unique query token
%% * The length of the response, as a 4-byte little-endian integer
%% * The JSON-encoded response
recv(Socket) ->
  case gen_tcp:recv(Socket, 8) of
    {ok, Token} ->
      io:format("Get back token ~p ~n", [Token]);
    {error, Reason} ->
      io:format("Fail to parse token")
  end,

  {RecvResultCode, ResponseLength} = gen_tcp:recv(Socket, 4),

  io:fwrite("ResponseLengh ~p ~n", [ResponseLength]),

  {ResultCode, Response} = gen_tcp:recv(Socket, binary:decode_unsigned(ResponseLength, little)),
  case ResultCode of
    ok -> {ok, Response};
    error ->
      io:fwrite("Got Error ~s ~n", [Response]),
      {error, Response}
  end.

run() ->
  RethinkDBHost = "127.0.0.1", % to make it runnable on one machine
  RethinkSock   = connect(RethinkDBHost),

  Qlist = [{db_list}],
  Qtlist = [{db, [<<"test">>]}, {table_list}],
  Qtcreate = [{db, [<<"test">>]}, {table_create, [<<"kids2">>]}],
  Qtinsert = [{db, [<<"test">>]}, {table, <<"kids">>}, {insert, [<<"{\"name\":\"item87vinhtestinerlang\"}">>]} ],

  io:format("LIST DB ~n======~n"),
  query(RethinkSock, Qlist),

  io:format("LIST Table ~n======~n"),
  query(RethinkSock, Qtlist),
  io:format("Create  ~n======~n"),
  query(RethinkSock, Qtcreate),
  io:format("Insert ~n======~n"),
  query(RethinkSock, Qtinsert),

  close(RethinkSock).

read_until_null(Socket) ->
  read_until_null(Socket, []).

read_until_null(Socket, Acc) ->
  %%{ok, Response} = gen_tcp:recv(Socket, 0),
  case gen_tcp:recv(Socket, 0) of
    {error, OtherSendError} ->
      io:format("Some other error on socket (~p), closing", [OtherSendError]),
      %%Client ! {self(),{error_sending, OtherSendError}},
      gen_tcp:close(Socket);
    {ok, Response} ->
      Result = [Acc, Response],
      case is_null_terminated(Response) of
        true -> {ok, iolist_to_binary(Result)};
        false -> read_until_null(Socket, Result)
      end
  end.

is_null_terminated(B) ->
  binary:at(B, iolist_size(B) - 1) == 0.
