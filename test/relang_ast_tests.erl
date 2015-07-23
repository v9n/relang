-module(relang_ast_tests).
-include_lib("eunit/include/eunit.hrl").

-define(test, ?assertMatch(R, relang_ast:make(Q))).
-define(showResult, io:format("R = ~p~n", [relang_ast:make(Q)])).

db_test() ->
  ?assertMatch([14,[<<"test">>]], relang_ast:make([{db, <<"test">>}])).

db_list_test() ->
  ?assertMatch([59,[], [{}]], relang_ast:make([{db_list}])).

table_test() ->
  Q = [{db, <<"test">>}, {table, <<"tv_shows">>}],
  R = [15,[[14,[<<"test">>]], <<"tv_shows">>]],
  ?test
  .
table_no_db_test() ->
  Q = [{table, <<"tv_shows">>}],
  R = [15,[<<"tv_shows">>]],
  ?test
  .
table_list_test() ->
  ?assertMatch([62,[[14,[<<"test">>]]],[{}]], relang_ast:make([
                                                                    {db, <<"test">>},
                                                                    {table_list}
                                                                   ])).

table_create_no_db_test() ->
  Q = [{table_create, lol}],
  R = [60,[lol]],
  ?test.

table_drop_no_db_test() ->
  Q = [{table_drop, lol}],
  R = [61,[lol]],
  ?test.

table_create_test() ->
  Q = [{db, test}, {table_create, tv_shows}],
  R = [60,[[14,[test]],tv_shows]],
  ?test.

table_drop_test() ->
  Q = [{db, test}, {table_drop, tv_shows}],
  R = [61,[[14,[test]], tv_shows]],
  ?test
  .

get_test() ->
  Q = [{db, [<<"test">>]},  {table, <<"tv_shows">>},  {get, <<"key">>}],
  R = [16, [[15,[[14,[<<"test">>]],<<"tv_shows">>]], <<"key">>]],
  ?assertMatch(R, relang_ast:make(Q))
  .

insert_test() ->
  Q = [{db, [<<"test">>]},  {table, <<"tv_shows">>}, {insert, [[{<<"name">>, <<"kurei">>}, {<<"age">>, <<28>>}]]}],
  R = [56,[[15,[[14,[<<"test">>]], <<"tv_shows">>]], [{<<"name">>,<<"kurei">>}, {<<"age">>, <<28>>}]]],
  ?assertMatch(R, relang_ast:make(Q))
  .

update_all_test() ->
  Q = [ {db, [<<"test">>]},
        {table, [<<"tv_shows">>]},
        {update, [[{<<"vin_touch">>, <<12>>}]
                 ]}
      ],
  R = [53,[[15,[[14,[<<"test">>]], <<"tv_shows">>]], [{<<"vin_touch">>, <<"\f">>}]]],
  ?assertMatch(R, relang_ast:make(Q))
  .

update_single_test() ->
  Q = [ {db, [<<"test">>]},
        {table, [<<"tv_shows">>]},
        {get, [<<"1a98d636">>]},
        {update, [[{<<"vin_touch">>, 12}]
                 ]}
      ],
  R = [53,[[16,[[15,[[14,[<<"test">>]], <<"tv_shows">>]], <<"1a98d636">>]], [{<<"vin_touch">>, 12}]]],
  ?assertMatch(R, relang_ast:make(Q))
  .

update_nested_field_test() ->
  Q = [
        {table, users},
        {get, 10001},
        {update, [
                  [
                   {contact, [{phone, [{cell, <<"408-555-4242">>}]}]}
                  ]
                 ]}
      ],
  R = [53,[
           [16,[[15,[users]],10001]],
           [{contact,
             [{phone,
               [{cell, <<"408-555-4242">>}]
              }]
            }]
          ]],
  ?assertMatch(R, relang_ast:make(Q))
  .

update_with_option() ->
  Q = [ {db, [<<"test">>]},
        {table, [<<"tv_shows">>]},
        {get, [<<"1a98d636">>]},
        {update,
         [[{<<"vin_touch">>, 12}]],
         [{durability, soft}, {return_changes, false}]
        }
      ],
  R = [53,
       [[16,[[15,[[14,[<<"test">>]], <<"tv_shows">>]], <<"1a98d636">>]],
        [{<<"vin_touch">>, 12}],
        [{<<"durability">>, <<"soft">>},{<<"return_changes">>,false}] 
       ]],
  ?test
  .

update_using_function_test() ->
  Q =
  [
    {table, posts},
    {update,
      fun(Post) ->
        [{
          views,
            relang:r([
                {field, [Post, views]},
                {add, 100},
                {default, 20}
              ])
        }]
      end
    }
  ],
  ?showResult,
  R = [53,[[15,[posts]], [69,[[2,[20]], [{views,[92,[[24,[[170,[[10,[20]],views]],100]],20]]}] ]]]],
  ?test.

filter_exact_test() ->
  Q = [{db, [<<"test">>]},
        {table, [<<"tv_shows">>]},
        {filter, [
          [{<<"age">>, 30}]
         ]}
      ],
  R = [39,[[15,[[14,[<<"test">>]],<<"tv_shows">>]],[{<<"age">>, 30}]]],
  ?test.

filter_exact_match_multi_field_test() ->
  Q =[ {db, [<<"test">>]},
        {table, [<<"tv_shows">>]},
        {filter, [
          [{<<"age">>, 30},
          {<<"name">>, <<"kurei">>},
          {<<"show">>, 1}]
        ]}
      ],
  R = [39,[[15,[[14,[<<"test">>]], <<"tv_shows">>]], [{<<"age">>, 30}, {<<"name">>, <<"kurei">>}, {<<"show">>,1}]]],
  ?test.

filter_function_simple_test() ->
  R = [39,[[15,[[14,[<<"test">>]], <<"tv_shows">>]],[69,[[2,[20]],[17,[[170,[[10,[20]],<<"name">>]], <<"lol">>]]]]]],
  Q = [{db, [<<"test">>]},
          {table, [<<"tv_shows">>]}, 
          {filter, fun(X) ->
                       [
                           {eq, [{field, [X, <<"name">>]},  <<"lol">>]}
                         ]
                   end}],
  ?test.

filter_function_test() ->
  R = [39,[
           [15,[[14,[<<"test">>]], <<"tv_shows">>]],
           [69,
            [[2,[20]],
              [67,
                [[67,
                  [[21,[[170,[[10,[20]], <<"age">>]],22]],[19,[[170,[[10,[20]],<<"age">>]],25]]]],
                 [97,[[170,[[10,[20]],<<"name">>]], <<"^k">>]]]
              ]
            ]
          ]
          ]],
  Q = [{db, [<<"test">>]}, {table, [<<"tv_shows">>]}, 
                  {filter, fun(X) ->
                               [
                                  {'and', [
                                           {gt, [{field, [X, <<"age">>]}, 22]},
                                           {lt, [{field, [X, <<"age">>]}, 25]},
                                           {match, [{field, [X, <<"name">>]},  <<"^k">>]}
                                          ]}
                                 ]
                           end}],
  ?test.

filter_function_has_field_test()->
  R = [39,[[15,[[14,[<<"test">>]], <<"tv_shows">>]],[69,[[2,[20]],[170,[[10,[20]],<<"show">>]]]]]],
  Q = [{db, [<<"test">>]}, {table,
        [<<"tv_shows">>]}, {filter, fun(X) ->
        [
          {field, [X, <<"show">>]}
        ]
      end}],
  ?test.

count_test() ->
  Q = [{db, [<<"test">>]}, {table, [<<"tv_shows">>]}, {count}],
  R = [43,[[15,[[14,[<<"test">>]], <<"tv_shows">>]]]],
  ?test
  .

eq_join_test() ->
  Q =   [{db, [<<"foodb">>]},
         {table, <<"compounds_foods">>},
         {eq_join,
          [<<"compound_id">>,
           [{db, [<<"foodb">>]}, {table, <<"compounds">>}]
          ]
         }
        ],
  R = [50,[[15,[[14,[<<"foodb">>]], <<"compounds_foods">>]], <<"compound_id">>, [15, [[14,[<<"foodb">>]], <<"compounds">>]]]],
  ?test.

eq_join_with_index_test() ->
  Q =   [{db, [<<"foodb">>]},
         {table, <<"compounds_foods">>},
         {eq_join,
          [<<"compound_id">>,
           [{db, [<<"foodb">>]}, {table, <<"compounds">>}],
           [{<<"index">>, <<"lol">>}]
          ]
         }
        ],
  R = [50,[[15,[[14,[<<"foodb">>]],<<"compounds_foods">>]],<<"compound_id">>,[15,[[14,[<<"foodb">>]],<<"compounds">>]]], [{<<"index">>, <<"lol">>}]],
  ?test.

eq_join_with_function_test() ->
  Q = [{db, [<<"foodb">>]},
       {table, <<"compounds_foods">>},
       {eq_join,
        [
         fun (X) ->
             [
              {field, {field, [X, <<"Parent">>]}, <<"Sub">>}
             ]
         end,
         [{table, <<"compounds_">>}]
        ]
        ,
        [{<<"index">>, <<"different_index">>}]
       }
      ],
  R = [50,[[15,[[14,[<<"foodb">>]], <<"compounds_foods">>]],[69,[[2,[20]],[170,[[170,[[10,[20]],<<"Parent">>]],<<"Sub">>]]]],[15,[<<"compounds_">>]]],[{<<"index">>, <<"different_index">>}]],
  ?test.

eq_join_with_function_one_level_test() ->
  Q = [{db, [<<"foodb">>]},
       {table, <<"compounds_foods">>},
       {eq_join,
        [
         fun (X) ->
             [
              {field, [X, <<"Parent">>]}
             ]
         end,
         [{table, <<"compounds_">>}]
        ]
        ,
        [{<<"index">>, <<"different_index">>}]
       }
      ],
  R = [50,[[15,[[14,[<<"foodb">>]], <<"compounds_foods">>]],[69,[[2,[20]],[170,[[10,[20]],<<"Parent">>]]]],[15,[<<"compounds_">>]]],[{<<"index">>, <<"different_index">>}]],
  ?test.

eq_join_with_function_complex_espression_test() ->
  Q = [{db, [<<"foodb">>]},
       {table, <<"compounds_foods">>},
       {eq_join,
        [
         fun (X) ->
             [
              {field, {field, [X, <<"Parent">>]}, <<"Sub">>},
              {nth, 20}
             ]
         end,
         [{table, <<"compounds_">>}]
        ]
        ,
        [{<<"index">>, <<"different_index">>}]
       }
      ],

  R = [50,[[15,[[14,[<<"foodb">>]], <<"compounds_foods">>]],[69,[[2,[20]],[45,[[170,[[170,[[10,[20]], <<"Parent">>]], <<"Sub">>]],20]]]],[15,[<<"compounds_">>]]],[{<<"index">>, <<"different_index">>}]],
  ?test.
%%%

nth_test() ->
  Q = [ {db, [<<"test">>]},
        {table, <<"tv_shows">>},
        {nth, 120}
      ],
  R = [45,[[15,[[14,[<<"test">>]], <<"tv_shows">>]],120]],
  ?test.

zip_test() ->
  Q =   [{db, [<<"foodb">>]},
         {table, <<"compounds_foods">>},
         {eq_join,
          [<<"compound_id">>,
           [{db, [<<"foodb">>]}, {table, <<"compounds">>}]
          ],
          [{<<"index">>, <<"lol">>}]
         },
         {zip}
        ],
  R = [72,[[50,[[15,[[14,[<<"foodb">>]],<<"compounds_foods">>]],<<"compound_id">>,[15,[[14,[<<"foodb">>]],<<"compounds">>]]],[{<<"index">>, <<"lol">>}]]]],
  ?test.

circle_test() ->
  Q = [{circle, [{-122.423246,37.779388}, 1000]}],
  R = [165,[[2,[ -122.423246, 37.779388]],1000]],
  ?test.

distance_test() ->
  Q =
    [
      {distance,
        [
          relang:r([{point, [-122.423246,37.779388]}]),
          relang:r([{point, [-117.220406,32.719464]}])
        ],
        [{unit, km}]
      }
    ],
  R = [162,[[159,[-122.423246,37.779388]],[159,[-117.220406,32.719464]]], [{unit, km}]],
  ?test.

point_test()->
  Q = [{point, [10, 20]}],
  R = [159,[10,20]],
  ?test.

polygon_test()->
  R = [161,[[2,[-122.423246,37.779388]],[2,[-122.423246,37.329898]],[2,[-121.88642,37.329898]],[2,[-121.88642,37.779388]]]],
  Q = [{polygon,
        [
          [-122.423246,37.779388],
          [-122.423246,37.329898],
          [-121.886420,37.329898],
          [-121.886420,37.779388]
        ]
       }
      ],
  ?test.

polygon_in_other_expression_test()->
  Q =
    [
      {table, geo},
      {insert, [[
        {id, 101},
        {rectangle, relang:r([{polygon,
            [
              [-122.423246,37.779388],
              [-122.423246,37.329898],
              [-121.886420,37.329898],
              [-121.886420,37.779388]
            ]
           }
          ])
        }
      ]]}
    ],
  R =
    [56, [
        [15, [geo]], [{
            id, 101
        }, {
            rectangle, [161, [
                [2, [-122.423246, 37.779388]],
                [2, [-122.423246, 37.329898]],
                [2, [-121.88642, 37.329898]],
                [2, [-121.88642, 37.779388]]
            ]]
        }]
    ]],

  ?test.

line_test()->
  Q =
  [
   {line,
    [
     [-122.423246,37.779388],
     [-121.886420,37.329898]
    ]
   }
  ]
  ,
  R = [160,[[2,[-122.423246,37.779388]],[2,[-121.88642,37.329898]]]],
  ?test.

line_using_in_other_expression_test()->
  Q =
  [
    {table, geo},
    {insert, [[
      {id, 101},
      {route,
        relang:r([
          {line, [
            [-122.423246,37.779388], [-121.886420,37.329898]
          ]}
        ])
      }
    ]]}
  ]
  ,
  R =
    [56, [
        [15, [geo]], [
            {id, 101},
            {route, [160, [
                [2, [-122.423246, 37.779388]],
                [2, [-121.88642, 37.329898]]
            ]]}
        ]
    ]]
  ,
  ?test.

fill_using_in_other_expression_test() ->
  Q =
  [
    {table, geo},
    {get, 201},
    {update,
      fun(Doc) ->
        [{
          rectangle,
            relang:r([
                {field, [Doc, rectangle]},
                {fill}
              ])
        }]
      end,
      [{non_atomic, true}]
    }
  ],
  R = [53,[[16,[[15,[geo]],201]],[69,[[2,[20]], [{rectangle, [167,[[170,[[10,[20]],rectangle]]]]}] ]]], [{non_atomic, true}]],
  ?test.

default_test() ->
  R = relang_ast:default(conmeo, a),
  Q = [92, [conmeo, a]],
  ?assertMatch(R, Q).

geojson_test()->
  T = [{type,'Point'},
       {coordinates, [ -122.423246, 37.779388 ]
       }
      ]
      ,
  Q = [{geojson, T}],
  R = [157,[ [{type,'Point'},{coordinates,[2,[-122.423246,37.779388]]}] ]],
  ?test.

geojson_using_in_expression_test()->
  T = [{type,'Point'},
       {coordinates, [ -122.423246, 37.779388 ]
       }
      ]
      ,
  Q = [
       {table, geo},
       {insert, [[
        {id, sfo},
        {name, <<"San Francisco">>},
        {location, relang:r([{geojson, T}])}
                ]]}
      ],
  R =
    [56, [
        [15, [geo]], [
            {id, sfo},
            {name, <<"San Francisco">>},
            {location, [157, [
                [
                  {type, 'Point'},
                  {coordinates, [2, [-122.423246, 37.779388]]}
                ]
            ]]}
        ]
    ]],

  ?test.
