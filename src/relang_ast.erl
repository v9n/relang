-module(relang_ast).

-author(kureikain).
-email("kurei@axcoto.com").

-compile(export_all). %% replace with -export() later, for God's sake!

make(Query) when is_tuple(Query)->
  Q = build(Query);

make([Query | Qs]) ->
  Parent = build(Query),
  Q = build(Qs, Parent),
  io:fwrite("Q= ~p", [Q]),
  io:fwrite("Q2= ~p", [Q]),
  Q
  .

%build([]) ->
%  "";

build(Query) when is_tuple(Query) ->
  case Query of
    {Func} ->
      io:format("Sngle Tuple hello ~p ~n", [Func]),
      T = apply(?MODULE, Func, [])
      ;
    {Func, Arguments} when is_list(Arguments)->
      io:format("Sngle Tuple ~p ~p lol~n", [Func, Arguments]),
      T = apply(?MODULE, Func, Arguments);
    {Func, Arguments} when not is_list(Arguments)->
      io:format("Sngle Tuple ~p ~p lol~n", [Func, [Arguments]]),
      T = apply(?MODULE, Func, [Arguments])
  end,
  io:format("Single Return ~p ~n", [T]),
  T
.

build([], Parent) ->
  Parent
  ;
build([Query | Qs], Parent) when is_tuple(Query)->
  case Query of
    {Func} ->
      io:format("Sngle Tuple hello ~p ~n", [Func]),
      T = apply(?MODULE, Func, [Parent])
      ;
    {Func, Arguments} when is_list(Arguments)->
      io:format("Sngle Tuple ~p ~p lol~n", [Func, Arguments]),
      T = apply(?MODULE, Func, [Parent] ++ Arguments);
    {Func, Arguments} when not is_list(Arguments)->
      io:format("Sngle Tuple ~p ~p lol~n", [Func, [Arguments]]),
      T = apply(?MODULE, Func, [Parent] ++ [Arguments])
  end,
  io:format("Single Return ~p ~n", [T]),
  build(Qs, T)
  .

build(Ignore_Now, [Query | Qs], Parent) ->
  {Tc, Ta, To} = build(Query),
  Node = case Ta of
    [] ->
      %[Tc, ",[[" ] ++ [Parent] ++ ["]", Ta, To, "]"];
      [Tc, [Parent]];
    _ ->
      %[Tc, ",[[" ] ++ [Parent] ++ ["],", Ta, To, "]", To]
      [Tc, Parent ++ Ta, To]
  end,
  build(Qs, Node)
  .

%%Detail implementation of API
db_create(Name) ->
  [
   57,
   [Name],
   {}
  ]
.

db(DbName) ->
  [
   14,
   [DbName],
   [{}]
  ].

db_list() ->
  [
    59,
    [],
    [{}]
  ].

table_list(Db) ->
  [
   62,
   [Db],
   [{}]
  ].

table_list(Db,Option) ->
  [
   62,
   [Db],
   Option
  ].

table(Db, Name) ->
  [
   15,
   [Db, Name]
  ].

table_create(Db, Name) ->
  [
   60,
   [Db, Name]
  ].

insert(Table, Item) ->
  [
   56,
   [Table, Item]
  ].

changes(Table, Function) ->
  [
   152,
   [Table],
   [{}]
  ]
  %Function(F)
  .

filter(Sequence, F) when is_tuple(F) ->
  filter(Sequence, [F]);

filter(Sequence, F) when is_list(F) ->
  io:fwrite("F= ~p ~n",[F]),
  io:fwrite("F= ~p ~n", [jsx:encode(F)]),
  [
    39,
    [],
    [F]
  ];
filter(Sequence, F) when is_function(F) ->
  [
    39,
    [Sequence,F(1)]
  ].

eq(Field, Value) ->
  [
   17,
   [[170, [[10, [20]], Field]], Value]
   %[{}]
  ]
  .

gt({Field, Value}) ->
  [
   21,
   [[170, [[10, [20]], Field]], Value]
   %[]
  ]
  .

match({Field, Value}) ->
  [
   97,
   [[170, [[10, [20]], Field]], Value]
  ]
  .

f_and([]) -> [];
f_and([F|R])  ->
  make([F]) ++ f_and(R).

now() ->
  [
   103,
   [],
   [{}]
  ]
  .

