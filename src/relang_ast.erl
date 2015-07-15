-module(relang_ast).

-author(kureikain).
-email("kurei@axcoto.com").

-compile(export_all). %% replace with -export() later, for God's sake!

%% Term definition
-define(NOW, 103).
-define(MATCH, 97).
-define(CHANGE, 152).
-define(INSERT, 56).
-define(BRACKET, 170).
-define(GT, 21).
-define(EQ, 17).
-define(FILTER, 39).
-define(DB, 14).
-define(DB_CREATE, 57).
-define(db_list, 59).
-define(TABLE, 15).
-define(table_list, 62).
-define(TABLE_CREATE, 60).

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
   ?DB_CREATE,
   [Name],
   {}
  ]
.

db(DbName) ->
  [
   ?DB,
   [DbName],
   [{}]
  ].

db_list() ->
  [
    ?db_list,
    [],
    [{}]
  ].

table_list(Db) ->
  [
   ?table_list,
   [Db],
   [{}]
  ].

table_list(Db,Option) ->
  [
   ?table_list,
   [Db],
   Option
  ].

table(Db, Name) ->
  [
   ?TABLE,
   [Db, Name]
  ].

table_create(Db, Name) ->
  [
   ?TABLE_CREATE,
   [Db, Name]
  ].

insert(Table, Item) ->
  [
   ?INSERT,
   [Table, Item]
  ].

changes(Table, Function) ->
  [
   ?CHANGE,
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
    ?FILTER,
    [],
    [F]
  ];
filter(Sequence, F) when is_function(F) ->
  [
    ?FILTER,
    [Sequence,F(1)]
  ].

eq(Field, Value) ->
  [
   ?EQ,
   [[170, [[10, [20]], Field]], Value]
   %[{}]
  ]
  .

gt({Field, Value}) ->
  [
   ?GT,
   [[?BRACKET, [[10, [20]], Field]], Value]
   %[]
  ]
  .

match({Field, Value}) ->
  [
   ?MATCH,
   [[?BRACKET, [[10, [20]], Field]], Value]
  ]
  .

f_and([]) -> [];
f_and([F|R])  ->
  make([F]) ++ f_and(R).

now() ->
  [
   ?NOW,
   [],
   [{}]
  ]
  .

