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
%%% iif(A, B, C)   -> begin V = A, if V -> B; true -> C end end
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

%% @doc Parse transform to be used by providing `{parse_transform, iif}' option.
%% `Opts' are compiler options passed from command line. E.g.:
%% ```
%% erlc -Diif_debug=N ...  ->  Opts = [{d,debug,N}|_]
%% erlc -Diif_debug ...    ->  Opts = [{d,debug}|_]
%% '''
parse_transform(Ast, Opts) ->
  Debug = case lists:keyfind(iif_debug, 2, Opts) of
            {d,iif_debug}              -> 3;
            {d,iif_debug,N} when N > 0 -> N;
            _                          -> 0
          end,
  Args = #{debug => Debug},
	(Debug band 1) > 0 andalso io:format("AST:\n  ~p~n",[Ast]),
  Tree = erl_syntax:form_list(Ast),
	(Debug band 4) > 0 andalso io:format("AST Tree:\n  ~p~n",[Tree]),
  put(count, 1),
  try
    ModifiedTree = recurse(Tree, Args),
    erase(line),
    erase(count),
    Res = erl_syntax:revert_forms(ModifiedTree),
    (Debug band 2) > 0 andalso io:format("AST After:\n  ~p~n",[Res]),
    Res
  catch E:R:S ->
    io:format(standard_error, "Error transforming AST: ~p\n  ~p\n", [R, S]),
    erlang:raise(E,R,S)
  end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

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
syn_if  (V,Then,Else)  -> L=get(line), erl_syntax:set_pos(erl_syntax:if_expr(
                                                    [clause3([],[[V]],[Then],L),
                                                     clause3([],[[syn_atom('true',L)]],[Else], L)]),
                                                  L).
syn_case (A, Clauses)  -> erl_syntax:set_pos(erl_syntax:case_expr(A, Clauses), get(line)).
syn_block(Clauses)     -> erl_syntax:set_pos(erl_syntax:block_expr(Clauses), get(line)).
syn_match(A, B)        -> erl_syntax:set_pos(erl_syntax:match_expr(A, B), get(line)).
syn_nil()              -> erl_syntax:set_pos(erl_syntax:nil(), get(line)).

make_var_name({I,_} = Line) ->
  K = get(count),
  put(count, K+1),
  syn_var(list_to_atom(lists:append(["__I",integer_to_list(I),"_",integer_to_list(K)])), Line).

update(Node, _Opts) ->
  case erl_syntax:type(Node) of
  application ->
      case erl_syntax:application_operator(Node) of
        {atom, Line, iif} ->
          put(line, Line),
          %io:format("Application: Op=~p ~1024p\n", [erl_syntax:application_operator(Node), erl_syntax:application_arguments(Node)]),
          case erl_syntax:application_arguments(Node) of
            [A,B,C] ->
              %% This is a call to iif(A, B, C).
              %% Replace it with:
              %%   begin
              %%     _V = A,
              %%     if _V -> B; _ -> C end
              %%   end
              Var = make_var_name(Line),
              syn_block([syn_match(Var, A), syn_if(Var, B, C)]);
            [A,B,C,D] ->
              %% This is a call to iif(A, B, C, D).
              %% Replace it with:
              %%   begin
              %%     _V = A,
              %%     case _V of B -> C; _ -> D end
              %%   end
              Var = make_var_name(Line),
              syn_block([
                  syn_match(Var, A),
                  syn_case(Var,
                    [clause3([B],            [], [C]),
                     clause3([syn_var('_')], [], [D])])
                ]);
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