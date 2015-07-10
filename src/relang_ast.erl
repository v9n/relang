-module(relang_ast).

-author(kureikain).
-email("kurei@axcoto.com").

-compile(export_all). %% replace with -export() later, for God's sake!

make([Query]) when is_tuple(Query)->
  {Tc, Ta} = build(Query),
  [Tc,  ",[", Ta, "]"];

make([Query | Qs]) ->
  {Tc, Ta} = build(Query),
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
  {Tc, Ta} = build(Query),
  case Ta of
    [""] ->
      [Tc, ",[[" ] ++ Parent ++ ["]", Ta, "]"];
    _ ->
      [Tc, ",[[" ] ++ Parent ++ ["],", Ta, "]"]
  end
  ;
build([Query | Qs], Parent) ->
  io:format("Q = ~p ~n", [Query]),
  io:format("Qs = ~p ~n", [Qs]),

  {Tc, Ta} = build(Query),
  Node = case Ta of
    [""] ->
      [Tc, ",[[" ] ++ [Parent] ++ ["]", Ta, "", "]" ];
    _ ->
      [Tc, ",[[" ] ++ [Parent] ++ ["],", Ta, "", "]" ]
  end,

  io:format("Node = ~p ~n", [Node]),
  build(Qs, Node)
  .

db_create(Name) ->
  {
   "57",
   ["\"", Name, "\""]
  }
.

db(DbName) ->
  {
   "14",
   ["\"", DbName, "\""]
  }.

db_list() ->
  {
    "59",
    [""]
  }.

table_list() ->
  {
   "62",
   [""]
  }.

table(Name) ->
  {
   "15",
   ["\"", Name, "\""]
  }.

table_create(Name) ->
  {
   "60",
   ["\"", Name, "\""]
  }.

insert(Item) ->
  {
   "56",
   Item
  }.
