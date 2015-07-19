-module(log).

-export([debug/2,
        warning/2,
        info/2
        ]).

debug(Label, L) ->
  io:fwrite("~n###########################~n~s : ~p. ~n###########################~n", [Label, [L]])
  .

warning(Label, L) ->
  io:fwrite("~n###########################~n~s : ~p. ~n###########################~n", [Label, [L]])
  .

info(Label, L) ->
  io:fwrite("~n###########################~n~s : ~p. ~n###########################~n", [Label, [L]])
  .
