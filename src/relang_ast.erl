-module(relang_ast).

-author(kureikain).
-email("kurei@axcoto.com").

-compile(export_all). %% replace with -export() later, for God's sake!

make([Query]) when is_tuple(Query)->
  {Tc, Ta, To} = build(Query),
  [Tc,  ",[", Ta, "]"];

make([Query | Qs]) ->
  {Tc, Ta, To} = build(Query),
  Parent = [Tc, ",[", Ta, "]"],
  build(Qs, Parent)
  .

build([]) ->
  "";

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
  %["[", Tc, ",[" ] ++ [""] ++ ["],", Ta, "]"]
  ;
build(Query, Parent) when is_tuple(Query)->
  {Tc, Ta, To} = build(Query),
  case Ta of
    [""] ->
      [Tc, ",[[" ] ++ Parent ++ ["]", Ta, To, "]"];
    _ ->
      [Tc, ",[[" ] ++ Parent ++ ["],", Ta, To, "]"]
  end
  ;
build([Query | Qs], Parent) ->
  {Tc, Ta, To} = build(Query),
  Node = case Ta of
    [""] ->
      [Tc, ",[[" ] ++ [Parent] ++ ["]", Ta, To, "]"];
    _ ->
      [Tc, ",[[" ] ++ [Parent] ++ ["],", Ta, To, "]", To]
  end,

  build(Qs, Node)
  .

%%Detail implementation of API
db_create(Name) ->
  {
   "57",
   ["\"", Name, "\""],
   []
  }
.

db(DbName) ->
  {
   "14",
   ["\"", DbName, "\""],
   []
  }.

db_list() ->
  {
    "59",
    [""],
    []
  }.

table_list() ->
  {
   "62",
   [""],
    []
  }.

table(Name) ->
  {
   "15",
   ["\"", Name, "\""],
    []
  }.

table_create(Name) ->
  {
   "60",
   ["\"", Name, "\""],
    []
  }.

insert(Item) ->
  {
   "56",
   Item,
    []
  }.

changes(Function) ->
  {
   "152",
   [""],
    []
  }

  %Function(F)
  .

filter(F) when is_tuple(F) ->
  filter([F]);

filter(F) when is_list(F) ->
  io:fwrite("F= ~p ~n",[F]),
  io:fwrite("F= ~p ~n", [jsx:encode(F)]),
  {
    "39",
    [""],
    [",", jsx:encode(F)]
  };
filter(F) when is_function(F) ->
  {
  }.
