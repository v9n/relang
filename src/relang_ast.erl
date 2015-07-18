-module(relang_ast).

-author(kureikain).
-email("kurei@axcoto.com").
-include("term.hrl").

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

% Argument can be other ReQL

build_argument(A) when is_tuple(A)->
  A
  ;
build_argument(A) when is_list(A)->
  A
  .

build(Query) when is_tuple(Query) ->
  case Query of
    {Func} ->
      T = apply(?MODULE, Func, []);
    {Func, Arguments} when is_list(Arguments)->
      T = apply(?MODULE, Func, Arguments);
    {Func, Arguments} when not is_list(Arguments)->
      T = apply(?MODULE, Func, [Arguments])
  end,
  T
.

% We have some function name we
func_name(F) ->
  case F of
    f_and ->
      list_to_atom("r_" ++ atom_to_list(F));
    _ ->
      F
  end
  .

build([], Parent) ->
  Parent;
build([Query | Qs], Parent) when is_tuple(Query)->
  T = case Query of
    {Func} ->
      apply(?MODULE, Func, [Parent]);
    {Func, Arguments} when is_list(Arguments)->
      apply(?MODULE, Func, [Parent] ++ Arguments);
    {Func, Arguments} when not is_list(Arguments)->
      apply(?MODULE, Func, [Parent] ++ [Arguments])
  end,
  build(Qs, T)
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
   [[170, [[?VAR, [20]], Field]], Value]
   %[{}]
  ]
  .

gt({Field, Value}) ->
  [
   ?GT,
   [[?BRACKET, [[?VAR, [20]], Field]], Value]
   %[]
  ]
  .

match({Field, Value}) ->
  [
   ?MATCH,
   [[?BRACKET, [[?VAR, [20]], Field]], Value]
  ]
  .

r_and([]) -> [];
r_and([F|R])  ->
  make([F]) ++ r_and(R).

r_or() -> [].

now() ->
  [
   ?NOW,
   [],
   [{}]
  ]
  .

expr([Op | Rest]) ->
  Ex = expr(Op)
  ;
expr([]) -> [];
expr(Op) when is_tuple(Op) ->
  expr([Op])
  .

add(X, Y) ->
  [?ADD,
   [X, Y]
  ].

sub(X, Y) ->
  [?SUB,
   [X, Y]
  ].

mul(X, Y) ->
  [?MUL,
   [X, Y]
  ].

r_div(X, Y) ->
  [?DIV, [X, Y]]
  .

mod(X, Y) ->
  [?MOD, [X, Y]]
  .
