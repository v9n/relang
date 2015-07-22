-module(relang_ast).

-author(kureikain).
-email("kurei@axcoto.com").
-include("term.hrl").

-compile(export_all). %% replace with -export() later, for God's sake!

make(Query) when is_tuple(Query)->
  build(Query);

make([Query | Qs]) ->
  Parent = build(Query),
  Q = build(Qs, Parent),
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
  Params = case Query of
    {F} -> [];
    {F, A} when is_list(A)-> A;
    {F, A} when not is_list(A)-> [A];
    {F, A, O} when is_list(A)-> [A, O];
    {F, A, O} when not is_list(A)-> [A, O]
  end,
  case F of
    'or' -> apply(?MODULE, F, [Params]) ;
    'and' -> apply(?MODULE, F, [Params]) ;
    _ -> apply(?MODULE, F, Params)
  end;
build(N) when is_number(N) ->
  apply(?MODULE, var, [N])
  .

build([], Parent) ->
  Parent;
build([Query | Qs], Parent) when is_tuple(Query)->
  T = case Query of
    {Func} -> apply(?MODULE, Func, [Parent]);
    {Func, Arguments} when is_list(Arguments)-> apply(?MODULE, Func, [Parent] ++ Arguments);
    {Func, Arguments} when not is_list(Arguments)-> apply(?MODULE, Func, [Parent] ++ [Arguments]);
    {Func, Arguments, Options} when not is_list(Arguments)-> apply(?MODULE, Func, [Parent] ++ [Arguments] ++ [Options]);
    {Func, Arguments, Options} when is_list(Arguments)-> apply(?MODULE, Func, [Parent] ++ Arguments ++ [Options])
  end,
  build(Qs, T)
  .

% We have some function name we
func_name(F) ->
  case F of
    'and' ->
      list_to_atom("r_" ++ atom_to_list(F));
    _ ->
      F
  end
  .

var(N) ->
  [?VAR, [N]]
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


table(Name) ->
  [
   ?TABLE,
   [Name]
  ].

table(Db, Name) ->
  [
   ?TABLE,
   [Db, Name]
  ].

get(Table, Key) ->
  [
   ?TERMTYPE_GET,
   [Table, Key]
  ].

table_create(Db, Name) ->
  [
   ?TABLE_CREATE,
   [Db, Name]
  ].

insert(Table, Item) ->
  [
   ?TERMTYPE_INSERT,
   [Table, Item]
  ].

update(Table, Item) ->
  [
   ?TERMTYPE_UPDATE,
   [Table, Item]
  ].
update(Table, Item, Option) ->
  [
   ?TERMTYPE_UPDATE,
   [Table, Item],
   Option
  ].

changes(Table, Function) ->
  [
   ?CHANGE,
   [Table],
   [{}]
  ]
  %Function(F)
  .

%%% For simple filter, we do exactly match only
%%% For complex filter, using anonymous function
filter(Sequence, F) when is_tuple(F) ->
  filter(Sequence, [F]);
filter(Sequence, F) when is_list(F) ->
  [
    ?FILTER,
    [Sequence,
    F]
  ];
filter(Sequence, F) when is_function(F) ->
  [
    ?FILTER,
    [Sequence, wrap_fun(make(F(gen_var(1))), gen_var(1))]
  ].

eq(Field, Value) ->
  [
   ?EQ,
   [make(Field), Value]
   %[{}]
  ]
  .

gt(Field, Value) ->
  [
   ?GT,
   [make(Field), Value]
   %[]
  ]
  .

lt(Field, Value) ->
  [
   ?LT,
   [make(Field), Value]
   %[]
  ]
  .

le(Field, Value) ->
  [
   ?LE,
   [make(Field), Value]
   %[]
  ]
  .

match(Field, Value) ->
  [
   ?MATCH,
   [make(Field), Value]
  ]
  .

%%% Note: not a part of REQL
%%% @TODO: Refactor and move it out
%%% has_field has no function body. no option
has_field(F) ->
  [?BRACKET, [[?VAR, [20]], F]]
  .

%%% Note: not a part of REQL
%%% @TODO: improve
field(P, F) ->
  [?BRACKET, [make(P), F]].

nth(Sequence, N) ->
  [?TERMTYPE_NTH, [Sequence, N]].

%%% when we pass argument to 'and', because of our recursion
%%% we don't know if an argument is compiled or not.
%%% We therefore use a {c, L} mean that it is compilted. 
%%% Otherwise it's not.
%%%
%%% We don't have to do for R, because R is never pre-compile
'and'([L,R]) ->
  L_ = case L of
    {c, L__} -> L__;
    _ -> make(L)
  end,
  [?TERMTYPE_AND, [L_, make(R)]]
  ;
'and'(C) ->
  [L,R, H|T] = C,
  'and'([{c, 'and'([L,R])}] ++
        [H] ++ T)
  .

'or'([L,R]) ->
  L_ = case L of
    {c, L__} -> L__;
    _ -> make(L)
  end,
  [?TERMTYPE_OR, [L_, make(R)]]
  ;
'or'(C) ->
  [L,R, H|T] = C,
  'or'([
    {c, 'or'([L,R])},
    [H] ++ T
        ])
  .

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

'div'(X, Y) ->
  [?DIV, [X, Y]]
  .

mod(X, Y) ->
  [?MOD, [X, Y]]
  .

during(X, Y) ->
  [?DURING, [X, Y]]
  .

%Working with filter
row(Q) ->
  [?FUNC, [
    [?MAKE_ARRAY, gen_var(1)],
    relang_ast:make(Q)
  ]].


row(Var, Q) ->
 %   [69, [
 %       [2, [17]],
 %       [67, [
 %           [17, [
 %               [170, [
 %                   [10, [17]], "age"
 %               ]],age 9999
 %           ]],
 %           [17, [
 %               [170, [
 %                   [170, [
 %                       [10, [17]], "name"
 %                   ]], "last"
 %               ]], "Adama"
 %           ]]
 %       ]]
 %   ]]

  [?FUNC, [
    [?MAKE_ARRAY, gen_var(1)],
    relang_ast:make(Q)
  ]]
  .

gen_var(L) ->
  [20]
  .

count(S) ->
  [
   ?TERMTYPE_COUNT,
   [S]
  ].

%%% [relang:connect(), [
%%% ]
inner_join(Table, F) ->
  10
  .

%%% @TODO: to make test work, detect environment and always return var number 20
wrap_fun(Q, Var) ->
  [?FUNC, [
    [?MAKE_ARRAY, Var],
    Q
  ]].

zip(Sequence) ->
  [?TERMTYPE_ZIP, [Sequence]]
  .

%%% [50,[[15,[[14,["foodb"]],"compounds_foods"]],"compound_id",[15,[[14,["foodb"]],"compounds_"]]]]
eq_join(Sequence, LeftField, RightTableQuery, Option) when is_function(LeftField) ->
  [
   ?TERMTYPE_EQ_JOIN,
   [Sequence, wrap_fun(make(LeftField(gen_var(1))), gen_var(1)), make(RightTableQuery)],
   Option
  ];
eq_join(Sequence, LeftField, RightTableQuery, Option) ->
  [
   ?TERMTYPE_EQ_JOIN,
   [Sequence, LeftField, make(RightTableQuery)],
   Option
  ].

eq_join(Sequence, LeftField, RightTableQuery) when is_function(LeftField) ->
  [
   ?TERMTYPE_EQ_JOIN,
   [Sequence,  wrap_fun(make(LeftField(gen_var(1))), gen_var(1)), make(RightTableQuery)]
  ];
eq_join(Sequence, LeftField, RightTableQuery)->
  [
   ?TERMTYPE_EQ_JOIN,
   [Sequence, LeftField, make(RightTableQuery)]
  ]
  .
