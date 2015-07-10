-module(relang_ast_tests).
-include_lib("eunit/include/eunit.hrl").

make_test() ->
  ?assertMatch(["59",",[",[[]],"]"], relang_ast:make([{db_list}])).
