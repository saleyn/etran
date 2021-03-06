%% vim:ts=2:sw=2:et
-module(iif_test).

-compile({parse_transform, iif}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

iif_test() ->
  A = is_tuple(erlang:timestamp()), B = not A,
  ?assertEqual(ok,        iif(A, ok)),
  ?assertEqual(undefined, iif(B, ok)),
  ?assertEqual(ok,        iif(A, ok, error)),
  ?assertEqual(error,     iif(B, ok, error)),
  ?assertEqual(ok,        iif(is_tuple(erlang:timestamp()), ok, error)),
  ?assertEqual(ok,        iif(1, 1, ok, error)),
  ?assertEqual(error,     iif(1, 2, ok, error)).

nvl_test() ->
  ?assertEqual(ok,        nvl(false, ok)),
  ?assertEqual(true,      nvl(true,  ok)),
  ?assertEqual(1,         nvl(1,     ok)),
  ?assertEqual(error,     nvl(true,  ok, error)),
  ?assertEqual(ok,        nvl(false, ok, error)),
  ?assertEqual(error,     nvl(1,     ok, error)).

-endif.
