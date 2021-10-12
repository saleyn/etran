%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc Erlang pipeline parse transform
%%%
%%% This transform implements a parser syntax extension that enables application
%%% of cascading function calls using the `/' operator.
%%%
%%% In the `LHS / RHS / ... Last.' notation, the result of evaluation of the LHS
%%% expression is passed as an argument to the RHS expression. This process
%%% continues until the `Last' expression is evaluated.  The head element of the
%%% pipeline must be either a term to which the arithmetic division `/` operator
%%% cannot apply (i.e. not integers, floats, functions), or if you need to pass
%%% integer(s) or float(s), wrap them in a list brackets.
%%%
%%% This transfor is inspired by the similar functionality in Linux (i.e. `|'
%%% pipe) and Elixir (`|>' pipe).
%%%
%%% When using this as a parse transform, include the `{parse_transform,erlpipe}'
%%% compiler option.
%%%
%%% The following examples illustrate the work of the transform, in which:
%%% ```
%%% test1(A)   -> [A]   / fun1 / mod:fun2 / fun3.
%%% test2(A,B) -> [A,B] / fun4 / fun5() / io:format("~p\n", [_]).
%%% '''
%%% will be transformed to:
%%% ```
%%% test1(A)   -> fun3(mod:fun2(fun1(A))).
%%% test2(A,B) -> io:format("~p\n", [fun5(fun4(A,B))]).
%%% '''
%%%
%%% Similarly to Elixir, a special `tap/2' function is implemented, which
%%% passes the given argument to an anonymous function, returning the argument
%%% itself. The following:
%%% ```
%%% f(A) -> A+1.
%%% ...
%%% test_tap() ->
%%%   [10] / tap(f)
%%%        / tap(fun f/1)
%%%        / tap(fun(I) -> I+1 end).
%%% '''
%%% is equivalent to:
%%% ```
%%% ...
%%% test_tap() ->
%%%   begin
%%%     f(10),
%%%     begin
%%%       f(10),
%%%       begin
%%%         (fun(I) -> I end)(10)
%%%         10
%%%       end
%%%     end
%%%   end.
%%% '''
%%%
%%% For debugging the AST of the resulting transform, pass the following
%%% options to the `erlc' compiler:
%%% <dl>
%%% <li>`-Derlpipe_orig' - print the original AST before the transform</li>
%%% <li>`-Derlpipe_ast'  - print the transformed AST</li>
%%% <li>`-Derlpipe_src'  - print the resulting source code after the transform</li>
%%% </dl>
%%%
%%% @author Serge Aleynikov <saleyn(at)gmail(dot)com>
%%% @end
%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2021 Serge Aleynikov
%%%
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without restriction,
%%% including without limitation the rights to use, copy, modify, merge,
%%% publish, distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do
%%% so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%-----------------------------------------------------------------------------
-module(erlpipe).

-export([parse_transform/2]).
-import(etran_util, [transform/2]).

-define(OP, '/').

%% @doc parse_transform entry point
parse_transform(AST, Options) ->
  etran_util:apply_transform(?MODULE, fun replace/1, AST, Options).

replace({op, _Loc, ?OP, Arg, Rhs}) ->
  apply_args(Arg, Rhs);
replace(_Exp) ->
  continue.

apply_args({op, _Loc, ?OP, A, E}, Rhs) ->
  case apply_args(A, E) of
    continue -> continue;
    [Args]   -> apply_args(Rhs, [Args]);
    Args     -> apply_args(Rhs, [Args])
  end;
apply_args({cons, _Loc, _, _} = List, Rhs) ->
  Args = [hd(transform(fun replace/1, [F])) || F <- cons_to_list(List)],
  [E]  = transform(fun replace/1, [Rhs]),
  do_apply(E, Args);
apply_args({Op, _Loc, _} = Arg, RHS) when Op==atom; Op==bin; Op==tuple; Op==string ->
  if is_tuple(RHS) ->
    do_apply(RHS, [Arg]);
  true ->
    do_apply(Arg, RHS)
  end;
%% List comprehension
apply_args({lc,_,_,_}=Lhs, Rhs) ->
  do_apply(Lhs, Rhs);
apply_args(AArgs, Rhs) when is_list(AArgs), is_list(Rhs) ->
  Args = [hd(transform(fun replace/1, [F])) || F <- AArgs],
  [E]  = transform(fun replace/1, Rhs),
  do_apply(E, Args);
apply_args(LHS, RHS) when is_tuple(LHS), is_list(RHS) ->
  do_apply(LHS, RHS);
apply_args(LHS, RHS) when is_tuple(LHS), is_tuple(RHS) ->
  continue.

do_apply({atom, Loc, _V} = Function, Arguments) ->
  {call, Loc, Function, Arguments};

do_apply({remote, Loc, _M, _F} = Function, Arguments) ->
  {call, Loc, Function, Arguments};

do_apply({call, Loc, Fun, []}, Arguments) ->
  {call, Loc, Fun, Arguments};

do_apply({'fun', Loc, {function, Fun, _}}, Arguments) ->
  {call, Loc, {atom, Loc, Fun}, Arguments};
do_apply({'fun', Loc, {function, _Mod, _Fun, _Arity}=F}, Arguments) ->
  {call, Loc, {'fun', Loc, F}, Arguments};
do_apply({'fun', Loc, {clauses, _}}=Fun, Arguments) ->
  {call, Loc, Fun, Arguments};

do_apply({call, _Loc, {atom, ALoc, tap}, [Arg]}, RHS) ->
  %% Tapping into a function's call (the return is a passed-through RHS argument)
  Res = do_apply(Arg, RHS),
  % If we are asked to tap into the fun's call, wrap the call in a block
  {block, ALoc, [{match, ALoc, {var, ALoc, '_'}, Res}, hd(RHS)]};

%% RHS is a tuple when it's the head of a pipeline:
%%   E.g.  [I || I <- L] / ...
do_apply({Op, Loc, Fun, Args} = LHS, RHS) when (Op =:= call orelse Op =:= lc), is_list(RHS) ->
  [NewLHS] = transform(fun(Forms) -> substitute(RHS, Forms) end, [LHS]),
  case NewLHS of
    LHS ->
      {Op, Loc, Fun, RHS ++ Args};
    ResLHS ->
      ResLHS
  end;

%% RHS is a list when it's in the middle of a pipeline:
%%   E.g.  ... / [I || I <- _] / ...
do_apply({Op, _, _, _} = LHS, RHS) when (Op =:= call orelse Op =:= lc), is_tuple(RHS) ->
  do_apply(RHS, [LHS]);

do_apply({Op, Loc, Fun, Args} = LHS, RHS) when Op =:= call; Op =:= lc ->
  [NewLHS] = transform(fun(Forms) -> substitute(RHS, Forms) end, [LHS]),
  case NewLHS of
    LHS when is_list(RHS) ->
      {Op, Loc, Fun, RHS ++ Args};
    LHS ->
      do_apply(RHS, [LHS]);
    ResLHS ->
      ResLHS
  end;

%% Use of operators
do_apply({op, Loc, Op, Lhs, Rhs}, Arguments) ->
  NewLhs = transform(fun replace/1, [Lhs]),
  NewRhs = transform(fun replace/1, [Rhs]),
  [LHS] = transform(fun(Forms) -> substitute(Arguments, Forms) end, NewLhs),
  [RHS] = transform(fun(Forms) -> substitute(Arguments, Forms) end, NewRhs),
  {op, Loc, Op, LHS, RHS};

do_apply({var, _, '_'}, [Arg]) ->
  Arg;
do_apply(Exp, _A) when is_tuple(Exp) ->
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
    "_" when is_list(Args) ->
      hd(Args);
    "_"    -> Args;
    [$_|T] ->
      try
        M = list_to_integer(T),
        lists:nth(M, Args)
      catch _:_ ->
        continue
      end;
    _ ->
      continue
  end;
substitute(_Args, _) ->
  continue.
