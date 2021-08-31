%% vim:ts=2:sw=2:et
-module(defarg_test).

-compile({parse_transform, defarg}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

defarg_test() ->
  ?assertEqual(3,     a()),
  ?assertEqual(9,     a(7)),
  ?assertEqual(10,    a(6,4)),
  ?assertEqual(7,     c()),
  ?assertEqual(9,     d()),
  ?assertEqual(5,     d(abc, [12])),
  ok.

a(A / 1, B / 2) ->
  A+B.

c(A / (10*2-15), B / (64 / 32)) ->
  A + B.

d(A / undefined, B / []) ->
  length(atom_to_list(A)) + length(B).

-endif.
