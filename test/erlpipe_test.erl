-module(erlpipe_test).

-export([erlpipe_test/0, m/2, t/2]).

-compile({parse_transform, erlpipe}).

erlpipe_test() ->
  test1(1).

t(A, B) ->
  [A,B] / lists:keyfind(_2, 1, _1)
        / max(10, _)
        / min(_, 20).

m(A, B) ->
  C = begin
        min(A, 10) / min(B, 20)
      end,
  max(A, 1) / max(C, 5).

test1(A) ->
  [A] / integer_to_list
      / list_to_integer()
      / element(_, {1,2})
      / io_lib:format("~w\n", [_]).
