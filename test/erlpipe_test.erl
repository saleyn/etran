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
  ?assertEqual(3,       abc / atom_to_list / length),
  ?assertEqual(3,       "abc" / length),
  ?assertEqual("abc",   <<"abc">> / binary_to_list),
  ?assertEqual("1,2,3", {$1,$2,$3} / tuple_to_list / [[I] || I <- _] / string:join(_, ",")),
  ?assertEqual("abc\n", "abc" / (_ ++ "\n")),
  ?assertEqual(8,       "abc" / (fun(A) -> A - length(_) end)(10) / (_ + 1) ).

test1(A) ->
  [A] / integer_to_list
      / list_to_integer()
      / element(_, {1,2})
      / io_lib:format("~w\n", [_])
      / lists:flatten.

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

-endif.
