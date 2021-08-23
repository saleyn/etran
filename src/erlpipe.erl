%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc Erlang pipeline parse transform
%%%
%%% This transform implements a parser syntax extension that enables application
%%% of cascading function calls using the `/' operator.
%%%
%%% Successive function calls get passed the result of evaluation of the
%%% previous function, creating a pipeline of calls similarly how it's done in
%%% Linux and Elixir.
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
%%% For debugging the AST of the resulting transform, use `-Derlpipe_debug'
%%% command-line option.
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
apply_args(AArgs, Rhs) when is_list(AArgs), is_list(Rhs) ->
  Args = [hd(transform(fun replace/1, [F])) || F <- AArgs],
  [E]  = transform(fun replace/1, Rhs),
  do_apply(E, Args);
apply_args({Op, _Loc, _} = Arg, RHS) when Op==atom; Op==bin; Op==tuple; Op==string ->
  if is_tuple(RHS) ->
    do_apply(RHS, [Arg]);
  true ->
    do_apply(Arg, RHS)
  end;
%% List comprehension
apply_args({lc,_,_,_}=Lhs, Rhs) ->
  [LHS] = transform(fun(Forms) -> substitute(Rhs, Forms) end, Lhs),
  LHS;
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

do_apply({Op, Loc, Fun, Args} = LHS, RHS) when Op =:= call; Op =:= lc ->
  [NewLHS] = transform(fun(Forms) -> substitute(RHS, Forms) end, [LHS]),
  case NewLHS of
    LHS ->
      {Op, Loc, Fun, RHS ++ Args};
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
do_apply({'fun', Loc, Clause}, Arguments) ->
  [NewClause] = transform(fun(Forms) -> substitute(Arguments, Forms) end, [Clause]),
  {'fun', Loc, NewClause};

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
    "_"    -> hd(Args);
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
