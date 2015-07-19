-module(relang_ast_tests).
-include_lib("eunit/include/eunit.hrl").

db_test() ->
  ?assertMatch([14,[<<"test">>],[{}]], relang_ast:make([{db, <<"test">>}])).

db_list_test() ->
  ?assertMatch([59,[],[{}]], relang_ast:make([{db_list}])).

table_list_test() ->
  ?assertMatch([62,[[14,[<<"test">>],[{}]]],[{}]], relang_ast:make([
    {db, <<"test">>},
    {table_list}
  ])).

get_test() ->
  Q = [{db, [<<"test">>]},  {table, <<"tv_shows">>},  {get, <<"key">>}],
  R = [16, [[15,[[14,[<<"test">>],[{}]],<<"tv_shows">>]], <<"key">>]],
  ?assertMatch(R, relang_ast:make(Q))
  .

insert_test() ->
  Q = [{db, [<<"test">>]},  {table, <<"tv_shows">>}, {insert, [[{<<"name">>, <<"kurei">>}, {<<"age">>, <<28>>}]]}],
  R = [56,[[15,[[14,[<<"test">>], [{}]], <<"tv_shows">>]], [{<<"name">>,<<"kurei">>}, {<<"age">>, <<28>>}]]],
  ?assertMatch(R, relang_ast:make(Q))
  .

update_test() ->
  Q = [ {db, [<<"test">>]},
        {table, [<<"tv_shows">>]},
        {update, [[{<<"vin_touch">>, <<12>>}]
                  ]}
      ],
  R = [53,[[15,[[14,[<<"test">>], [{}]], <<"tv_shows">>]], [{<<"vin_touch">>, <<"\f">>}]]],
  ?assertMatch(R, relang_ast:make(Q))
  .



filter_test() ->
  ?assertMatch([39,
                [[15,[[14,[<<"test">>],[{}]],<<"tv_shows">>]],
                 [69,[[2,[20]],[67,[[67,[[21,[[170,[[10,[20]], <<"age">>]],22]],[19,[[170,[[10,[20]],<<"age">>]],25]]]],[97,[[170,[[10,[20]],<<"name">>]],<<"^k">>]]]]]]
                ]],
               relang_ast:make(
                 [{db, [<<"test">>]}, {table, [<<"tv_shows">>]}, 
                  {filter, fun(X) ->
                               X([
                                  {'and', [
                                           {gt, [<<"age">>, 22]},
                                           {lt, [<<"age">>, 25]},
                                           {match, [<<"name">>,  <<"^k">>]}
                                          ]}
                                 ])
                           end}]
                )).
