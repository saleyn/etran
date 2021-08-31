%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc Erlang parse transform for permitting default arguments in functions
%%%
%%% Presently the Erlang syntax doesn't allow function arguments to have default
%%% parameters.  Consequently a developer needs to replicate the function
%%% definition multiple times passing constant defaults to some parameters of
%%% functions.
%%%
%%% This parse transform addresses this shortcoming by extending the syntax
%%% of function definitions at the top level in a module to have a default
%%% expression such that for `A / Default' argument the `Default' will be
%%% used if the function is called in code without that argument.
%%%
%%% ```
%%% -export([t/2]).
%%%
%%% test(A / 10, B / 20) ->
%%%   A + B.
%%% '''
%%% The code above is transformed to:
%%% ```
%%% -export([t/2]).
%%% -export([t/0, t/1]).
%%%
%%% test()    -> test(10);
%%% test(A)   -> test(A, 20);
%%% test(A,B) -> A+B.
%%% '''
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
-module(defarg).

-export([parse_transform/2]).

-import(etran_util, [transform/2]).

%% @doc parse_transform entry point
parse_transform(AST, Options) ->
  etran_util:process(?MODULE,
                     fun(Ast) -> replace(Ast, undefined, [], []) end,
                     AST, Options).

replace([], _Mod, Exports, Acc) ->
  Res = lists:reverse(Acc),
  {HeadAST, [{attribute, Loc, _, _} = ModAST|TailAST]} =
    lists:splitwith(fun({attribute, _, module, _}) -> false; (_) -> true end, Res),
  HeadAST ++ [ModAST, {attribute, Loc, export, Exports}] ++ TailAST;

replace([{attribute,_,module,Mod}=H|T], _, Exports, Acc) ->
  replace(T, Mod, Exports, [H|Acc]);

replace([{function, Loc, Fun, Arity, [{clause, CLoc, Args, Guards, Body}]}=H|T], Mod, Exports, Acc) ->
  {RevDef, RevRestArgs} =
    lists:splitwith(
      fun({op, _, '/', _Arg, _Def}) -> true;
         (_)                        -> false
      end,
      lists:reverse(Args)),
  {FrontArgs, DefArgs} =
    {lists:reverse(RevRestArgs), lists:reverse([{A,D} || {op, _, '/', A, D} <- RevDef])},

  case DefArgs of
    [] ->
      replace(T, Mod, Exports, [H|Acc]);
    _ ->
      lists:filter(fun({op, _, '/', _A, _D}) -> true; (_) -> false end, FrontArgs) /= []
        andalso throw(lists:flatten(
                        lists:format(
                          "Function ~w:~w/~w has default arguments not at the end of the argument list!",
                          [get(key), Fun, Arity]))),
      %% Add new exports, e.g.: -export([f/2]).
      N = Arity - length(DefArgs),
      NewExports = Exports ++ [{Fun,I} || I <- lists:seq(N, Arity-1)],

      LastClause = {function, Loc, Fun, Arity,
                     [{clause, CLoc, FrontArgs ++ [A || {A,_} <- DefArgs], Guards, Body}]},

      AddClauses = element(3,
                    lists:foldl(fun({A, D}, {Front, ArityN, Acc1}) ->
                      Acc2 = [{function, Loc, Fun, ArityN,
                               [{clause, CLoc, Front, [],
                                 [{call, CLoc, {atom, CLoc, Fun}, Front ++ [D]}]}]} | Acc1],
                      {Front ++ [A], ArityN+1, Acc2}
                    end, {FrontArgs, N, []}, DefArgs)),

      replace(T, Mod, NewExports, [LastClause | AddClauses] ++ Acc)
  end;

replace([H|T], Mod, Exports, Acc) ->
  replace(T, Mod, Exports, [H|Acc]).
