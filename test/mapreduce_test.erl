-module(mapreduce_test).

-compile({parse_transform, mapreduce}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

mapreduce_test() ->
  ?assertEqual({[1,2,3], 6}, [{I,S+I} || S = 0, I <- [1,2,3]]),
  ?assertEqual({[1,2,3], 6}, [{I,S+I} || S = 0, {I,_} <- [{1,a},{2,b},{3,c}]]),
  ?assertEqual({[1,3],   4}, [{I,S+I} || S = 0, I <- [1,2,3], I /= 2]),
  ?assertEqual({[{1,3},
                 {1,4}], 9}, [{{I,J},S+I+J} || S = 0, I <- [1,2], J <- [3,4], I /= 2]).

fold_test() ->
  ?assertEqual(6, [S+I   || S = 0, I <- [1,2,3]]),
  ?assertEqual(6, [S+I   || S = 0, {I,_} <- [{1,a},{2,b},{3,c}]]),
  ?assertEqual(4, [S+I   || S = 0, I <- [1,2,3], I /= 2]),
  ?assertEqual(9, [S+I+J || S = 0, I <- [1,2], J <- [3,4], I /= 2]).


-endif.
