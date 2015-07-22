-module(relang_ast_tests).
-include_lib("eunit/include/eunit.hrl").

-define(test, ?assertMatch(R, relang_ast:make(Q))).

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
