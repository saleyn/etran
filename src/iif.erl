%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc Conditional expression functions
%%%
%%% This module exports a parse transform and implements several
%%% condition-checking functions.
%%%
%%% When using this as a parse transform, include the `{parse_transform,iif}'
%%% compiler option.  In this case for given expressions `A',`B',`C', and `D'
%%% the following code transforms will be done:
%%% ```
%%% iif(A, B)      -> case A of true -> B; _ -> undefined end
%%% iif(A, B, C)   -> case A of true -> B; _ -> C end
%%% iif(A,B,C,D)   -> case A of B -> C; _ -> D end
%%% nvl(A,B)       -> case A of false -> B; undefined -> B; [] -> B; _ -> A end
%%% nvl(A,B,C)     -> case A of false -> B; undefined -> B; [] -> B; _ -> C end
%%% '''
%%% For debugging the AST of the resulting transform, use `iif_debug'
%%% command-line option:
%%% ```
%%% erlc -Diif_debug=1 ...    % Prints AST before the transform
%%% erlc -Diif_debug=2 ...    % Prints AST after the transform
%%% erlc -Diif_debug[=3] ...  % Prints AST before/after the transform
%%% '''
%%%
%%% Alternative to using this module as a parse_transform, it implements
%%% several `iif/3,4' exported functions that can be used without the transform.
%%%
%%% @author Serge Aleynikov <saleyn(at)gmail(dot)com>
%%% @end
%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 Serge Aleynikov
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
-module(iif).

% If using this module as a parse transform, we need to export the following:
-export([parse_transform/2]).

-export([iif/3, iif/4, nvl/2, ife/2, ife/3, ifne/2, ifne/3, format_ne/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

%% @doc Parse transform to be used by providing `{parse_transform, iif}' option.
%% `Opts' are compiler options passed from command line. E.g.:
%% ```
%% erlc -Diif_ast  ...  ->  Opts = [{d,iif_ast}|_]
%% erlc -Diif_orig ...  ->  Opts = [{d,iif_orig}|_]
%% erlc -Diif_src  ...  ->  Opts = [{d,iif_src}|_]
%% '''
parse_transform(AST, Opts) ->
  etran_util:process(?MODULE, fun(Ast) -> replace(Ast) end, AST, Opts).

replace(AST) ->
  Tree = erl_syntax:form_list(AST),
  put(count, 1),
  try
    ModifiedTree = recurse(Tree, #{}),
    erase(line),
    erase(count),
    erl_syntax:revert_forms(ModifiedTree)
  catch E:R:S ->
    io:format(standard_error, "Error transforming AST: ~p\n  ~p\n", [R, S]),
    erlang:raise(E,R,S)
  end.

%% @doc Return `True' if first argument is `true' or return `False' if
%%      the first argument is one of: `[]', `false', `undefined'.
iif([],            _True, False) -> execute([], False);
iif(false,         _True, False) -> execute([], False);
iif(undefined,     _True, False) -> execute([], False);
iif(true,           True,_False) -> execute([], True).

%% @doc Return `True' if first two arguments match
iif(Value, Value,   True,_False) -> execute(Value, True);
iif(Value,_Other,  _True, False) -> execute(Value, False).

%% @doc Alias for `ife/2'
nvl(Value, IfNull)     -> ife(Value, IfNull).

%% @doc Return `Value' if first argument is one of: `[]', `false', `undefined'.
%%      Otherwise return the value of the first argument.
ife([],         Value) -> execute([], Value);
ife(false,      Value) -> execute([], Value);
ife(undefined,  Value) -> execute([], Value);
ife(Test,      _Value) -> Test.

%% @doc Return `Empty' if first argument is one of: `[]', `false', `undefined'.
%%      Otherwise, if `NotEmpty' is `fun()', evaluate it, or if it's `fun(Arg)'
%%      evaluate it with `Value' argument.
ife([],         Empty,_NotEmpty) -> execute([], Empty);
ife(false,      Empty,_NotEmpty) -> execute([], Empty);
ife(undefined,  Empty,_NotEmpty) -> execute([], Empty);
ife(Value,     _Empty, NotEmpty) -> execute(Value, NotEmpty).

%% @doc Return `Value' if first argument is not one of: `[]', `false', `undefined'.
%%      Otherwise, if `Value' is `fun()', evaluate it, or if it's `fun(Arg)'
%%      evaluate it with `Test' argument.
% If not empty
ifne([],       _Value) -> [];
ifne(false,    _Value) -> [];
ifne(undefined,_Value) -> [];
ifne(Test,      Value) -> execute(Test, Value).

%% @doc Return `NotEmpty' if first argument is not one of: `[]', `false', `undefined'.
%%      Otherwise, if `NotEmpty' is `fun()', evaluate it, or if it's `fun(Arg)'
%%      evaluate it with `Value' argument.
ifne([],       _NotEmpty, Empty) -> execute([], Empty);
ifne(false,    _NotEmpty, Empty) -> execute([], Empty);
ifne(undefined,_NotEmpty, Empty) -> execute([], Empty);
ifne(Value,     NotEmpty,_Empty) -> execute(Value, NotEmpty).

%% @doc Format if first argument is not empty
format_ne(false, _Fmt, _Args) -> [];
format_ne([],    _Fmt, _Args) -> [];
format_ne(_True,  Fmt,  Args) -> io_lib:format(Fmt, Args).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

execute(_, F) when is_function(F,0) -> F();
execute(V, F) when is_function(F,1) -> F(V);
execute(_, V)                       -> V.

%% Parse transform support
recurse(Tree, Opts) ->
  update(case erl_syntax:subtrees(Tree) of
           []   -> Tree;
           List -> erl_syntax:update_tree(Tree, [[recurse(Subtree, Opts) || Subtree <- Group]
                                                 || Group <- List])
         end,
         Opts).

%set_pos([H|T], Line) ->
%  [set_pos(H, Line) | set_pos(T, Line)];
%set_pos([], _Line) ->
%  [];
%set_pos(T, Line) when is_tuple(T), tuple_size(T) > 1 ->
%  setelement(2, T, Line).

clause3(A,B,C)         -> clause3(A,B,C,get(line)).
clause3(A,B,C, Line)   -> erl_syntax:set_pos(erl_syntax:clause(A,B,C), Line).

syn_atom(A)            -> syn_atom(A, get(line)).
syn_atom(A, Line)      -> erl_syntax:set_pos(erl_syntax:atom(A), Line).
syn_var (V)            -> syn_var(V, get(line)).
syn_var (V, Line)      -> erl_syntax:set_pos(erl_syntax:variable(V), Line).
%syn_if  (V,Then,Else)  -> L=get(line), erl_syntax:set_pos(erl_syntax:if_expr(
%                                                    [clause3([],[[V]],[Then],L),
%                                                     clause3([],[[syn_atom('true',L)]],[Else], L)]),
%                                                  L).
syn_case (A, Clauses)  -> erl_syntax:set_pos(erl_syntax:case_expr(A, Clauses), get(line)).
syn_case (V,M,T,F)     -> syn_case(V, [clause3([M],            [], [T]),
                                       clause3([syn_var('_')], [], [F])]).
syn_block(Clauses)     -> erl_syntax:set_pos(erl_syntax:block_expr(Clauses), get(line)).
syn_match(A, B)        -> erl_syntax:set_pos(erl_syntax:match_expr(A, B), get(line)).
syn_nil()              -> erl_syntax:set_pos(erl_syntax:nil(), get(line)).

make_var_name({I,_} = Line) ->
  K = get(count),
  put(count, K+1),
  syn_var(list_to_atom(lists:append(["__I@",integer_to_list(I),"_",integer_to_list(K)])), Line).

update(Node, _Opts) ->
  case erl_syntax:type(Node) of
  application ->
      case erl_syntax:application_operator(Node) of
        {atom, Line, iif} ->
          put(line, Line),
          %io:format("Application: Op=~p ~1024p\n", [erl_syntax:application_operator(Node), erl_syntax:application_arguments(Node)]),
          case erl_syntax:application_arguments(Node) of
            [A,B] ->
              %% This is a call to iif(A, B).
              %% Replace it with:
              %%   case A of true -> B; _ -> undefined end
              syn_case(A, {atom, Line, true}, B, {atom, Line, undefined});
            [A,B,C] ->
              %% This is a call to iif(A, B, C).
              %% Replace it with:
              %%   case A of true -> B; _ -> C end
              syn_case(A, {atom, Line, true}, B, C);
            [A,B,C,D] ->
              %% This is a call to iif(A, B, C, D).
              %% Replace it with:
              %%   case A of B -> C; _ -> D end
              syn_case (A, B, C, D);
            _ ->
              Node
          end;
        {atom, Line, nvl} ->
          put(line, Line),
          case erl_syntax:application_arguments(Node) of
            [A,B] ->
              %% This is a call to ife(A, B).
              %% Replace it with a case expression:
              %%   begin
              %%     _V = A,
              %%     case _V of false -> B; undefined -> B; [] -> B; _ -> A end
              %%   end
              Var = make_var_name(Line),
              syn_block([
                syn_match(Var, A),
                syn_case(Var,
                  [clause3([syn_atom(false)    ],[],[B]),
                   clause3([syn_atom(undefined)],[],[B]),
                   clause3([syn_nil()],          [],[B]),
                   clause3([syn_var('_')],       [],[A])])
              ]);
            [A,B,C] ->
              %% This is a call to nvl(A, B, C).
              %% Replace it with a case expression:
              %%   begin
              %%     _V = A,
              %%     case _V of false -> B; undefined -> B; [] -> B; _ -> C end
              %%   end
              Var = make_var_name(Line),
              syn_block([
                syn_match(Var, A),
                syn_case(Var,
                  [clause3([syn_atom(false)    ],[],[B]),
                   clause3([syn_atom(undefined)],[],[B]),
                   clause3([syn_nil()],          [],[B]),
                   clause3([syn_var('_')],       [],[C])])
              ]);
            _ ->
              Node
          end;
        _ ->
          Node
      end;
  _ ->
   Node
  end.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

ife_test() ->
    ?assertEqual(abc, ife ([],        abc)),
    ?assertEqual(abc, ife (false,     abc)),
    ?assertEqual(abc, ife (undefined, abc)),
    ?assertEqual(xxx, ife (xxx,       abc)),
    ?assertEqual(ok,  ife (false,     fun()  -> ok end)),
    ?assertEqual([],  ife (false,     fun(V) -> V  end)),

    ?assertEqual(abc, ife ([],        abc, efg)),
    ?assertEqual(abc, ife (false,     abc, efg)),
    ?assertEqual(abc, ife (undefined, abc, efg)),
    ?assertEqual(efg, ife (xxx,       abc, efg)),
    ?assertEqual(xxx, ife (xxx,       abc, fun(V) -> V  end)).

ifne_test() ->
    ?assertEqual([],  ifne([],        abc)),
    ?assertEqual([],  ifne(false,     abc)),
    ?assertEqual([],  ifne(undefined, abc)),
    ?assertEqual(abc, ifne(xxx,       abc)),
    ?assertEqual(ok,  ifne(false,     abc, fun()  -> ok end)),
    ?assertEqual(x,   ifne(xxx,       fun()  -> x  end, efg)),
    ?assertEqual(xxx, ifne(xxx,       fun(V) -> V  end, efg)),

    ?assertEqual(efg, ifne([],        abc, efg)),
    ?assertEqual(efg, ifne(false,     abc, efg)),
    ?assertEqual(efg, ifne(undefined, abc, efg)),
    ?assertEqual(abc, ifne(xxx,       abc, efg)),
    ?assertEqual(xxx, ifne(xxx,       fun(V) -> V  end, efg)).

iif_test() ->
    ?assertEqual(abc, iif(x, x, abc, efg)),
    ?assertEqual(ok,  iif(x, x, fun() -> ok end, efg)),
    ?assertEqual(x,   iif(x, x, fun(X) -> X end, efg)),
    ?assertEqual(efg, iif(x, y, abc, efg)),
    ?assertEqual(ok,  iif(x, y, abc, fun() -> ok end)),
    ?assertEqual(x,   iif(x, y, abc, fun(X) -> X end)).

-endif.
