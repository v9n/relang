-module(relang).

-author(kureikain).
-email("kurei@axcoto.com").

%%-export([connect/1]).
-compile(export_all). %% replace with -export() later, for God's sake!

%% From ql2.proto
-define(RETHINKDB_VERSION, 32#723081e1).

-include("ql2_pb.hrl").
-include("term.hrl").

start() ->
  application:start(relang),
  ok.

stop() ->
  application:stop(relang),
  ok.

%% http://erlang.org/pipermail/erlang-questions/2004-December/013734.html
connect() ->
  connect("127.0.0.1").

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


%%% RethinkDB API
r(Q) -> query(Q).
r(Socket, RawQuery) ->
  query(Socket, RawQuery).

%%% Fetch next batch
next({Socket, Token}) ->
  Iolist = ["[2]"],
  Length = iolist_size(Iolist),
  io:format("Block socket <<< waiting for more data from stream~n"),

  ok = gen_tcp:send(Socket, [<<Token:64/little-unsigned>>, <<Length:32/little-unsigned>>, Iolist]),
  {ok, R} = recv(Socket),
  Rterm = jsx:decode(R),
  proplists:get_value(<<"r">>, Rterm).

%%% Build AST from raw query
query(RawQuery) ->
  relang_ast:make(RawQuery).

%%% Build and Run query when passing Socket
query(Socket, RawQuery) ->
  query(Socket, RawQuery, [{}]).

query(Socket, RawQuery, Option) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  Token = random:uniform(3709551616),
  %io:format("QueryToken = ~p~n", [Token]),

  Query = relang_ast:make(RawQuery),

  io:format("Query = ~p ~n", [Query]),
  Iolist  = jsx:encode([?QUERYTYPE_START, Query, Option]), % ["[1,"] ++ [Query] ++ [",{}]"], % list db 
  Length = iolist_size(Iolist),
  %io:format("Query= ~p~n", [Iolist]),
  %io:format("Length: ~p ~n", [Length]),

  case gen_tcp:send(Socket, [<<Token:64/little-unsigned>>, <<Length:32/little-unsigned>>, Iolist]) of
    ok -> ok;
    {error, Reason} ->
      io:fwrite("Got Error when sending query: ~s ~n", [Reason]),
      {error, Reason}
  end,

  case recv(Socket) of
    {ok, R} ->
      io:format("Ok "),
      io:format(R),
      Rterm = jsx:decode(R),
      %proplists:get_value(<<"r">>, Rterm),
      case proplists:get_value(<<"t">>, Rterm) of
        ?RUNTIME_ERROR ->
          io:format("Error"),
          {error, proplists:get_value(<<"r">>, Rterm)};
        ?SUCCESS_ATOM ->
          io:format("response: a single atom"),
          {ok, proplists:get_value(<<"r">>, Rterm)};
        ?SUCCESS_SEQUENCE ->
          io:format("response: a sequence"),
          {ok, proplists:get_value(<<"r">>, Rterm)};
        ?SUCCESS_PARTIAL ->
          % So we get back a stream, let continous pull query
          io:format("response: partial. Can use next here"),

          Recv = spawn(?MODULE, stream_recv, [Socket, Token]),
          Pid = spawn(?MODULE, stream_poll, [{Socket, Token}, Recv]),

          {ok, {pid, Pid}, proplists:get_value(<<"r">>, Rterm)}
      end
      ;
    {error, ErrReason} ->
      io:fwrite("Got Error when receving: ~s ~n", [ErrReason]),
      {error, ErrReason}
  end
  .
%%%

%%% When the response_type is SUCCESS_PARTIAL=3, we can call next to send more data
next(Query) ->
  continue.

stream_stop(Socket, Token) ->
  Iolist = ["[3]"],
  Length = iolist_size(Iolist),
  ok = gen_tcp:send(Socket, [<<Token:64/little-unsigned>>, <<Length:32/little-unsigned>>, Iolist])
  .

%% receive data from stream, then pass to other process
stream_recv(Socket, Token) ->
  receive
    R ->
      io:fwrite("Changefeed receive item: ~p ~n",[R])
  end,
  stream_recv(Socket, Token)
  .

%Continues getting data from stream
stream_poll({Socket, Token}, PidCallback) ->
  Iolist = ["[2]"],
  Length = iolist_size(Iolist),
  io:format("Block socket <<< waiting for more data from stream~n"),

  ok = gen_tcp:send(Socket, [<<Token:64/little-unsigned>>, <<Length:32/little-unsigned>>, Iolist]),
  {ok, R} = recv(Socket),
  Rterm = jsx:decode(R),
  spawn(fun() -> PidCallback ! proplists:get_value(<<"r">>, Rterm) end),
  stream_poll({Socket, Token}, PidCallback)
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
      <<K:64/little-unsigned>> = Token,
      io:format("Get back token ~p ~n", [K]),
      io:format("Get back token ~p ~n", [Token]);
    {error, Reason} ->
      io:format("Fail to parse token")
  end,

  {RecvResultCode, ResponseLength} = gen_tcp:recv(Socket, 4),
  <<Rs:32/little-unsigned>> = ResponseLength,
  io:format("ResponseLengh ~p ~n", [Rs]),
  io:format("ResponseLengh ~p ~n", [ResponseLength]),

  {ResultCode, Response} = gen_tcp:recv(Socket, binary:decode_unsigned(ResponseLength, little)),
  case ResultCode of
    ok ->
      {ok, Response};
    error ->
      io:fwrite("Got Error ~s ~n", [Response]),
      {error, Response}
  end.

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
