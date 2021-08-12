-module(erlpipe).
-export([parse_transform/2]).

% parse_transform entry point
parse_transform(AST, Options) ->
  ShowAST       = lists:member({d,pipe_verbose}, Options),
  ShowAST andalso io:format("Before: ~p~n", [AST]),
  Transformed   = transform(fun replace/1, AST),
  ShowAST andalso io:format("After: ~p~n", [Transformed]),
  Transformed.

replace({op, _Loc, '/', Arg, Exp}) ->
  case apply_args(Arg, Exp, false) of
    {true, Res} ->
      Res;
    false ->
      continue
  end;
replace(_Exp) ->
  continue.

apply_args({op, _Loc, '/', A, E}, Exp, Found) ->
  case apply_args(A, E, Found) of
    {true, Args} ->
      {true, do_apply(Exp, [Args])};
    false ->
      Found
  end;
apply_args({cons, _Loc, _, _} = List, Exp, _Found) ->
  Args = [hd(transform(fun replace/1, [F])) || F <- cons_to_list(List)],
  [E]  = transform(fun replace/1, [Exp]),
  {true, do_apply(E, Args)};
apply_args(AArgs, Exp, _Found) when is_list(AArgs) ->
  Args = [hd(transform(fun replace/1, [F])) || F <- AArgs],
  [E]  = transform(fun replace/1, [Exp]),
  {true, do_apply(E, Args)};
apply_args(_Other, _Exp, Found) ->
  Found.

do_apply({atom, Line, _V} = Function, Arguments) ->
  {call, Line, Function, Arguments};

do_apply({remote, Line, _M, _F} = Function, Arguments) ->
  {call, Line, Function, Arguments};

do_apply({call, Line, Fun, []}, Arguments) ->
  {call, Line, Fun, Arguments};

do_apply({call, Line, Fun, ArgPattern}, Arguments) ->
  Substituted = transform(fun(Forms) -> substitute(Arguments, Forms) end, ArgPattern),
  {call, Line, Fun, Substituted};

do_apply({var, _, '_'}, [Arg]) ->
  Arg;
do_apply(Exp, _A) ->
  io:format("-- skip: ~p\n", [Exp]),
  Exp.

cons_to_list({cons, _, A, B}) ->
  [A | cons_to_list(B)];
cons_to_list({nil, _}) ->
  [];
cons_to_list([A]) ->
  [A].

%% Substitute '_', '_1', '_2', ... '_N' with the corresponding argument
substitute(Args, {var, _, V}) when is_atom(V) ->
  case atom_to_list(V) of
    "_"    -> hd(Args);
    [$_|T] ->
      try
        M = list_to_integer(T),
        lists:nth(M, Args)
      catch _:_ ->
        continue
      end
  end;
substitute(_Args, _) ->
  continue.

transform(Fun, Forms) when is_function(Fun, 1), is_list(Forms) ->
  transform2(Fun, Forms).

transform2(_, []) ->
  [];
transform2(Fun, [F|Fs]) when is_atom(element(1,F)) ->
  case Fun(F) of
    NewF when is_tuple(NewF) ->
      [NewF | transform2(Fun, Fs)];
    continue ->
      [list_to_tuple(transform2(Fun, tuple_to_list(F))) | transform2(Fun, Fs)]
  end;
transform2(Fun, [L|Fs]) when is_list(L) ->
  [transform2(Fun, L) | transform2(Fun, Fs)];
transform2(Fun, [F|Fs]) ->
  [F | transform2(Fun, Fs)];
transform2(_, F) ->
  F.
