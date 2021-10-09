%% vim:ts=2:sw=2:et
-module(erlpipe_test).

-compile({parse_transform, erlpipe}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

erlpipe_test() ->
  ?assertEqual("1\n",   test1(1)),
  ?assertEqual("ab2",   test2(10)),
  ?assertEqual(b,       test3(3, [{1,a},{10,b}])),
  ?assertEqual(5.0,     test4(25, 5)),
  ?assertEqual(8,       test5()),
  ?assertEqual(28,      test6()),
  ?assertEqual(10,      test_tap()),
  ?assertEqual(2,       [2] / (fun t/1)),
  ?assertEqual(2,       [[1,2]] / (fun erlang:length/1)),
  ?assertEqual(2,       [2] / t),
  ?assertEqual(2,       [2] /  fun(I) -> I end),
  ?assertEqual(2,       [2] / (fun(I) -> I end)(_)),
  ?assertEqual(6,       [I || I <- [1,2,3]] / lists:sum),
  %?assertEqual(11,      [1, 2, 3] / fun1 / fun2 / fun3),
  ?assertEqual(1.0,     10 / min(2,3) / 5.0),
  ?assertEqual(2.0,     10 / 5),
  ?assertEqual(1,       [[1]] / hd),
  ?assertEqual(3,       abc / atom_to_list / length),
  ?assertEqual(3,       "abc" / length),
  ?assertEqual("abc",   <<"abc">> / binary_to_list),
  ?assertEqual("c",     [2] / lists:nthtail(_, "abc")),
  ?assertEqual("1,2,3", {$1,$2,$3} / tuple_to_list / [[I] || I <- _] / string:join(_, ",")),
  ?assertEqual("abc\n", "abc" / (_ ++ "\n")),
  ?assertEqual(8,       "abc" / (fun(A) -> A - length(_) end)(10) / (_ + 1) ).

test1(A) ->
  [A] / integer_to_list
      / list_to_integer()
      / element(_, {1,2})
      / io_lib:format("~w\n", [_])
      / lists:flatten
      / ttt(get(env)).

test2(A) ->
  [A+10]
  / integer_to_list
  / lists:append(["ab", _])
  / lists:split(3, _)
  / element(1, _).

test3(A, B) ->
  [B ++ [{5,c}], max(A,10)]
  / lists:keyfind(_2, 1, _1)
  / element(2, _).

test4(A, B) ->
  % To make sure the parse transform doesn't touch `A / B' expressions,
  % where `A' is a function call, an integer, or a float.
  C = begin
        max(A, 20) / min(B, 20)
      end,
  D = 5.0 / C,
  E = 5 / trunc(C),
  erlang:max(A, 1) / max(C, 5) * D * E.

test5() ->
  %% I.e.: g(length([max(1,2)]), f(5, h(t(2)))).
  [1] / max(2)
      / ([2] ++ [_])
      / length([_])
      / ([t(2)] / f(5, [_] / h) / g).

test6() ->
  %% I.e.: 20 + length(atom_to_list(abc)) + length("ee" ++ "efg")
  abc / atom_to_list
      / length
      / (20 + _ + ("ee" / (_ ++ "efg") / length)).

test_tap() ->
  [10] / max(2)
       / tap(fun(A) -> A+1 end)
       / tap(t1)
       / tap(fun t1/1).

t(A)    -> A.
t1(A)   -> A+1.
h(I)    -> I.
f(I, J) -> I+J.
g(I, J) -> I+J.

ttt(A, _B) ->
  A.

%fun1(A, B, C) -> [A+B, B+C].
%fun2(A, B)    -> [1, 2, A+B].
%fun3(A, B, C) -> A+B+C.

-endif.
