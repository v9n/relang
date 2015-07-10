-module(relang_ast).

-author(kureikain).
-email("kurei@axcoto.com").

-compile(export_all). %% replace with -export() later, for God's sake!

%build([]) ->
%  [];
build(Query) when is_tuple(Query) ->
  case Query of
    {Func} ->
      io:format("Sngle Tuple hello ~p ~n", [Func]),
      T = ["[", apply(?MODULE, Func, []), ",[]]"]
      ;
    {Func, Arguments} ->
      io:format("Sngle Tuple ~p ~ p lol~n", [Func, Arguments]),
      T = ["[", apply(?MODULE, Func, Arguments), ",[]]"]
  end,
  io:format("Single Return ~p ~n", [T]),
  T
  ;
build([Query , Qs]) ->
  io:format("Q = ~p ~n", [Query]),
  io:format("Qs = ~p ~n", [Qs]),

  case Query of
    {Func} ->
      io:format("F = ~p ~n", [Func]),
      T = ["[", apply(?MODULE, Func), ",[]]"]
      ;
    {Func, Arguments} ->
      io:format("F = ~p~p ~n", [Func, Arguments]),
      T = ["[", apply(?MODULE, Func, Arguments), ",[]]"]
  end,
  io:format("T = ~p ~n", [T]),
  ["[", build(Qs), T]
  %"[59,[]]".
  .

db(DbName) ->
  [
   "[",
   14,
   "[", DbName, "]",
   "]"
  ].

db_list() ->
  ["[", 59 , ",[]]"].

table_list() ->
  [
   "[", 62, "]"
  ].


