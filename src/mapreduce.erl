%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc Erlang map-reduce parse transform
%%%
%%% This transform introduces two modifications of the list comprehension syntax
%%% that allow to perform a fold and mapfold on a list.
%%%
%%% === Fold Comprehension ===
%%%
%%% To invoke the fold comprehension transform include the initial state
%%% assignment into a comprehension that returns a non-tuple expression:
%%% ```
%%% [S+I || S = 1, I <- L].
%%%  ^^^    ^^^^^
%%% '''
%%%
%%% In this example the `S' variable gets assigned the initial state `1', and
%%% the `S+I' expression represents the body of the fold function that
%%% is passed the iteration variable `I' and the state variable `S':
%%% ```
%%% lists:foldl(fun(I, S) -> S+I end, 1, L).
%%% '''
%%%
%%% === MapFold Comprehension ===
%%%
%%% To invoke the mapfold comprehension transform include the initial state
%%% assignment into a comprehension, and return a tuple expression:
%%% ```
%%% [{I, S+I} || S = 1, I <- L].
%%%  ^^^^^^^^    ^^^^^
%%% '''
%%%
%%% In this example the `S' variable gets assigned the initial state `1', and
%%% the `{I, S+I}' two-elements tuple expression represents the body of the fold
%%% function that is passed the iteration variable `I' and the state variable `S':
%%% ```
%%% lists:mapfoldl(fun(I, S) -> S+I end, 1, L).
%%% '''
%%%
%%% == Compilation ==
%%%
%%% When using this as a parse transform, include the
%%% `{parse_transform,mapreduce}' compiler option.
%%%
%%% For debugging the AST of the resulting transform, pass the following
%%% options to the `erlc' compiler:
%%% <dl>
%%% <li>`-Dmapreduce_orig' - print the original AST before the transform</li>
%%% <li>`-Dmapreduce_ast'  - print the transformed AST</li>
%%% <li>`-Dmapreduce_src'  - print the resulting source code after the transform</li>
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
-module(mapreduce).

-export([parse_transform/2]).
-import(etran_util, [transform/2]).

%% @doc parse_transform entry point
parse_transform(AST, Options) ->
  etran_util:apply_transform(?MODULE, fun replace/1, AST, Options).

%% MapReduce transform
%% ===================
%% [{I, S+I} || S = 1, I <- L]
%%   Rewrite: lists:mapfoldl(fun(I, S) -> {S+I, I} end, 1, L).
%% [{I, S+I} || S = 1, I <- L, I > 10]
%%   Rewrite: lists:mapfoldl(fun(I, S) -> {S+I, I} end, 1, [_I || _I <- L, _I > 10]).
%% [{I, S+I} || S = 1, I <- L1, J <- L2]
%%   Rewrite: lists:mapfoldl(fun({I,J}, S) -> {S+I, I} end, 1, [ || I <- L1, J <- L2]).
replace({lc,Loc,{tuple,_,ResBody0},
                [{match,_,{var,_,_}=StateVar,StateInit0} | Generators]})
    when element(1, hd(Generators)) == generate ->
  ResBody = [hd(transform(fun replace/1, [A])) || A <- ResBody0],
  [Init]  =  transform(fun replace/1, [StateInit0]),

  % The body of the mapfold's fun is a tuple:
  FunBody = {tuple, Loc, ResBody},

  % Split generators from filters
  {Gens, Filters} =
    lists:splitwith(fun(G) -> element(1, G) == generate end, Generators),
  {GLoc, FunArgs, ListOfLCs}  =
    case Gens of
      [{generate, Loc0, FunArg0, List0}] when Filters == [] ->
        % Simple case with one generator and no filters:
        % [{I, S+I} || S = 1, I <- L]
        {Loc0, FunArg0, List0};
      [{generate, Loc0, FunArg0, List0}] when Filters /= [] ->
        % Simple case with one generator and no filters:
        %   [{I, S+I} || S = 1, {I,_} <- L, I > 1, I < 10]
        % Convert the comprehension with filter into:
        %   lists:mapfoldl(fun({I,_}, S) -> {I,S+I} end, 1, [_V || _V = {I,_} <- L, I > 1, I < 10])
        {Var,Match} = maybe_make_var(Loc0, FunArg0),
        {Loc0, FunArg0, {lc, Loc0, Var,
                               [{generate, Loc0, Match, List0}| Filters]}};
      _ ->
        % More than one generator:
        % [{I, S+I} || S = 1, I = FunArg1 <- List1, J = FunArg2 <- List2, ...]
        % - Make a list:
        %      [{I,FunArg1,LCList1}, {J,FunArg2,LCList2}, ...]
        VarsList = lists:reverse(
          lists:foldl(fun({generate, GLoc1, FunArg0, List0}, ALists) ->
            {Var,Match} = maybe_make_var(GLoc1, FunArg0),
            [{Var, Match, List0}|ALists]
          end, [], Gens)),

        % - Create a new list comprehension:
        % [{I,J,...} || I <- L1, J <- L2, ..., Filters]
        Vars    = [V || {V,_,_} <- VarsList],
        ArgVars = {tuple, Loc, Vars},
        FArgs   = {tuple, Loc, [A || {_,A,_} <- VarsList]},
        ListLCs = {lc, Loc, ArgVars,
                      [{generate, GLoc, Match, LList}
                       || {{var,GLoc,_}, Match, LList} <- VarsList] ++ Filters},
        {Loc, FArgs, ListLCs}
    end,
  % Finally, rewrite into:
  %   lists:mapfoldl(fun(Arg, State) -> {Arg1, State1} end,
  %                  Init,
  %                  _ListOfLCs = [{I,J,...} || I <- L1, J <- L2, ...])
  {call, GLoc,
    {remote,GLoc,{atom,GLoc,lists},{atom,GLoc,mapfoldl}},
    [{'fun', GLoc,
      {clauses,
       [{clause, Loc,
         [FunArgs, StateVar], % The fun has 2 arguments: ({I,J, ...}, S) -> ...
         [],                  % No guards
         [FunBody]            % Body
        }]}},
     Init,
     ListOfLCs]};

%% Fold Transform
%% ==============
%% Example:
%%   L = [1,2,3]
%%   [S+I || S = 0, I <- L].      Returns: 6
%%           ^^^^^
%%
%% [S+I || S = 0, I <- L]
%%   Rewrite: lists:foldl(fun(I, S) -> {S+I, I} end, 1, L).
%% [{S+I, I} || S = 1, I <- L, I > 10]
%%   Rewrite: lists:foldl(fun(I, S) -> {S+I, I} end, 1, [_I || _I <- L, _I > 10]).
%% [{S+I, I} || S = 1, I <- L1, J <- L2]
%%   Rewrite: lists:foldl(fun({I,J}, S) -> {S+I, I} end, 1, [ || I <- L1, J <- L2]).
replace({lc,Loc, ResBody0,
                [{match,_,{var,_,_}=StateVar,StateInit0} | Generators]})
    when element(1, ResBody0) /= tuple
       , element(1, hd(Generators)) == generate ->
  [FunBody] = transform(fun replace/1, [ResBody0]),
  [Init]    = transform(fun replace/1, [StateInit0]),

  % Split generators from filters
  {Gens, Filters} =
    lists:splitwith(fun(G) -> element(1, G) == generate end, Generators),
  {GLoc, FunArgs, ListOfLCs}  =
    case Gens of
      [{generate, Loc0, FunArg0, List0}] when Filters == [] ->
        % Simple case with one generator and no filters:
        % [{I, S+I} || S = 1, I <- L]
        {Loc0, FunArg0, List0};
      [{generate, Loc0, FunArg0, List0}] when Filters /= [] ->
        % Simple case with one generator and no filters:
        %   [{I, S+I} || S = 1, {I,_} <- L, I > 1, I < 10]
        % Convert the comprehension with filter into:
        %   lists:mapfoldl(fun({I,_}, S) -> {I,S+I} end, 1, [_V || _V = {I,_} <- L, I > 1, I < 10])
        {Var,Match} = maybe_make_var(Loc0, FunArg0),
        {Loc0, FunArg0, {lc, Loc0, Var,
                               [{generate, Loc0, Match, List0}| Filters]}};
      _ ->
        % More than one generator:
        % [{I, S+I} || S = 1, I = FunArg1 <- List1, J = FunArg2 <- List2, ...]
        % - Make a list:
        %      [{I,FunArg1,LCList1}, {J,FunArg2,LCList2}, ...]
        VarsList = lists:reverse(
          lists:foldl(fun({generate, GLoc, FunArg0, List0}, ALists) ->
            {Var,Match} = maybe_make_var(GLoc, FunArg0),
            [{Var, Match, List0}|ALists]
          end, [], Gens)),

        % - Create a new list comprehension:
        % [{I,J,...} || I <- L1, J <- L2, ..., Filters]
        Vars    = [V || {V,_,_} <- VarsList],
        ArgVars = {tuple, Loc, Vars},
        FArgs   = {tuple, Loc, [A || {_,A,_} <- VarsList]},
        ListLCs = {lc, Loc, ArgVars,
                      [{generate, GLoc, Match, LList}
                       || {{var,GLoc,_}, Match, LList} <- VarsList] ++ Filters},
        {Loc, FArgs, ListLCs}
    end,
  % Finally, rewrite into:
  %   lists:mapfoldl(fun(Arg, State) -> {Arg1, State1} end,
  %                  Init,
  %                  _ListOfLCs = [{I,J,...} || I <- L1, J <- L2, ...])
  {call, GLoc,
    {remote,GLoc,{atom,GLoc,lists},{atom,GLoc,foldl}},
    [{'fun', GLoc,
      {clauses,
       [{clause, Loc,
         [FunArgs, StateVar], % The fun has 2 arguments: ({I,J, ...}, S) -> ...
         [],                  % No guards
         [FunBody]            % Body
        }]}},
     Init,
     ListOfLCs]};

replace(_Exp) ->
  continue.

maybe_make_var(_, {var,_,_} = Arg) ->
  {Arg, Arg};
maybe_make_var({Ln,Pos}=Loc, Arg) ->
  Var   = {var,Loc,list_to_atom("_I@"++integer_to_list(Ln)++"_"++integer_to_list(Pos))},
  Match = {match, Loc, Var, Arg},
  {Var, Match}.
