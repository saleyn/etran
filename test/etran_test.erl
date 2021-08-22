-module(etran_test).

-compile({parse_transform, etran}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

etran_test() ->
  ?assertEqual({[1,2,3], 6}, [{I,S+I} || S = 0, I <- [1,2,3]]),
  ?assertEqual(6,            [S+I     || S = 0, I <- [1,2,3]]),
	?assertEqual(3,            abc / atom_to_list / length),
  ?assertEqual(error,        iif(1 == length([1,2]), ok, error)).


-endif.
