%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc Erlang map-reduce parse transform
%%%
%%% This transform introduces two modifications of the list comprehension syntax
%%% that allow to perform a fold and mapfold on a list.
%%%
%%% ==== Indexed List Comprehension ====
%%%
%%% This extension of a list comprehension, passes an additional argument to the
%%% left hand side of the comprehension, which is the index of the current item
%%% in the list:
%%% ```
%%% [ io:format("Rec#~w: ~p\n", [I, N]) || I, N <- L]
%%%                                        ^^
%%% ```
%%% The index is defined by the a variable listed after the `||' operator.
%%% This is equivalent to the following:
%%% ```
%%% lists:mapfoldl(
%%%   fun(N, I) ->
%%%     io:format("Rec#~w: ~p\n", [I, N]),
%%%     I+1
%%%   end, 1, L)
%%% ```
%%%
%%% === Fold Comprehension ===
%%%
%%% To invoke the fold comprehension transform include the initial state
%%% assignment into a comprehension that returns a non-tuple expression:
%%% ```
%%% [S+N || S = 1, N <- L].
%%%  ^^^    ^^^^^
%%% '''
%%%
%%% In this example the `S' variable gets assigned the initial state `1', and
%%% the `S+N' expression represents the body of the fold function that
%%% is passed the iteration variable `N' and the state variable `S':
%%% ```
%%% lists:foldl(fun(N, S) -> S+N end, 1, L).
%%% '''
%%%
%%% Fold comprehension can be combined with the indexed list comprehension:
%%% ```
%%% [running_sum(I, N, S+N) || I, S=1, N <- L].
%%%
%%% running_sum(I, N, RunningSum) ->
%%%   io:format("Rec#~w: ~p (~w)\n", [I, N, RunningSum]),
%%%   S.
%%% ```
%%%
%%% In this case the definition of the indexed fold comprehension would be
%%% transformed to:
%%% ```
%%% lists:foldl(fun(
%%% ```
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
-export([foldl/3, foldr/3]).

-import(etran_util, [transform/2]).

%% @doc parse_transform entry point
parse_transform(AST, Options) ->
  etran_util:apply_transform(?MODULE, fun replace/1, AST, Options).

%%------------------------------------------------------------------------------
%% @doc Fold over a list by additionally passing the list's current item number
%%      to the folding fun.  This function is similar to lists:foldl/3, except
%%      that the fun takes the extra second integer argument that represents
%%      the sequential number of the item from the list.
%% @end
%%------------------------------------------------------------------------------
-spec foldl(fun((Position::integer(), Item::term(), Acc::term()) -> NewAcc::term()),
            Init::term(), list()) -> term().
foldl(Fun, Init, List) when is_function(Fun, 3) ->
  element(2, lists:foldl(fun(V, {I, S}) -> R = Fun(V, I, S), {I+1, R} end, {1, Init}, List)).

%%------------------------------------------------------------------------------
%% @doc Fold over a list by additionally passing the list's current item number
%%      to the folding fun.  This function is similar to lists:foldr/3, except
%%      that the fun takes the extra second integer argument that represents
%%      the sequential number of the item from the list.
%% @end
%%------------------------------------------------------------------------------
-spec foldr(fun((Position::integer(), Item::term(), Acc::term()) -> NewAcc::term()),
            Init::term(), list()) -> term().
foldr(Fun, Init, List) when is_function(Fun, 3) ->
  N = length(List),
  element(2, lists:foldr(fun(V, {I, S}) -> R = Fun(V, I, S), {I-1, R} end, {N, Init}, List)).

%% MapReduce transform
%% ===================
%% <<{I, S+I} || S = 10, I <- L>>
%%   Rewrite: lists:mapfoldl(fun(I, S) -> {I, S+I} end, 10, L).
%% <<{I, S+I} || N, S = 10, I <- L>>
%%   Rewrite:
%%      begin
%%        {_V1, {_, _V2}} = lists:mapfoldl(fun(I, {N, S}) -> {I, {N+1, S+I}} end, {1, 10}, L),
%%        {_V1, _V2}
%%      end.
replace({bc,Loc,{tuple, _, _} = Body,
                [{match,_,{var,_,_},_StateInit0}=Match | Generators]})
    when element(1, hd(Generators)) == generate ->
  replace2(Loc, Body, undefined, Match, Generators);

replace({bc,Loc,{tuple, _, _} = Body,
                [{var,  _, V} = Var,
                 {match,_,{var,_,_},_StateInit0}=Match | Generators]})
    when is_atom(V), element(1, hd(Generators)) == generate ->
  replace2(Loc, Body, Var, Match, Generators);

%% Fold Transform
%% ==============
%% Example:
%%   L = [1,2,3]
%%   [S+I || S = 0, I <- L].                   %% Returns: 6
%%           ^^^^^
%%   [do(Index,S+I) || Index, S = 0, I <- L].  %% Returns: 6, Uses Index as the item index
%%                     ^^^^^  ^^^^^
%%   do(N, Sum) ->
%%     io:format("Item#~w running sum: ~w\n", [N, Sum]),
%%     Sum.
%%
%% [S+I || S = 0, I <- L]
%%   Rewrite: lists:foldl(fun(I, S) -> S+I end, 1, L).
%% [S+I || N, S = 0, I <- L]
%%   Rewrite: lists:foldl(fun(I, S) -> S+I end, 1, L).
%% [S+I || S = 1, I <- L, I > 10]
%%   Rewrite: lists:foldl(fun(I, S) -> S+I end, 1, [_I || _I <- L, _I > 10]).
%% [S+I+J || S = 1, I <- L1, J <- L2]
%%   Rewrite: lists:foldl(fun({I,J}, S) -> S+I+J end, 1, [{I,J} || I <- L1, J <- L2]).
%%
replace({lc,Loc,ResBody0,
                [{var, _, V}=Var,
                 {match,_,{var,_,_},_StateInit0}=Match | Generators]})
    when is_atom(V)
       , element(1, ResBody0) /= tuple
       , element(1, hd(Generators)) == generate ->
  replace3(Loc, ResBody0, Var, Match, Generators);

replace({lc,Loc,ResBody0,
                [{match,_,{var,_,_},_StateInit0}=Match | Generators]})
    when element(1, ResBody0) /= tuple
       , element(1, hd(Generators)) == generate ->
  replace3(Loc, ResBody0, undefined, Match, Generators);

%% Indexed list comprehension
%% ==========================
%% [I || _Index, I <- L1]
%%   Rewrite: element(1, lists:mapfoldl(fun(I, _Index) -> {I, _Index+1} end, 1, L)).
replace({lc,Loc,ResBody0, [{var, _, V}=Var | Generators]})
    when is_atom(V)
       , element(1, hd(Generators)) == generate ->
  replace3(Loc, ResBody0, Var, undefined, Generators);

replace(_Exp) ->
  continue.

maybe_make_var(_, {var,_,_} = Arg) ->
  {Arg, Arg};
maybe_make_var({Ln,Pos}=Loc, Arg) ->
  Var   = {var,Loc,list_to_atom("_I@"++integer_to_list(Ln)++"_"++integer_to_list(Pos))},
  Match = {match, Loc, Var, Arg},
  {Var, Match}.

%% MapFold Rewriting rules:
%% ========================
%%
%% <<{I, S+I} || S = 1, I <- L>>
%%    Rewrite: lists:mapfoldl(fun(I, S) -> {I, S+I} end, 1, L).
%% <<{I, S+I} || S = 1, I <- L, I > 10>>
%%    Rewrite: lists:mapfoldl(fun(I, S) -> {I, S+I} end, 1, [_I || _I <- L, _I > 10]).
%% <<{I, S+I} || S = 1, I <- L1, J <- L2>>
%%    Rewrite: lists:mapfoldl(fun({I,J}, S) -> {I, S+I} end, 1, [ || I <- L1, J <- L2]).
%%
%%  <<{I, S+I} || N, S = 1, I <- L>>
%%    Rewrite:
%%       begin
%%         {L1, {_, V}} = lists:mapfoldl(fun(I, S) -> {I, S+I} end, 1, L),
%%         {L1, V}
%%       end
%%  <<{I, S+I} || N, S = 1, I <- L, I > 10>>
%%    Rewrite:
%%       begin
%%         {L1, {_, V}} = lists:mapfoldl(fun(I, S) -> {I, S+I} end, 1, [_I || _I <- L, _I > 10]),
%%         {L1, V}
%%       end
%%  <<{I, S+I} || N, S = 1, I <- L1, J <- L2>>
%%    Rewrite:
%%       begin
%%         {L1, {_, V}} = lists:mapfoldl(fun({I,J}, S) -> {I, S+I} end, 1, [ || I <- L1, J <- L2]),
%%         {L1, V}
%%       end
replace2(Loc,{tuple,_,ResBody0}, Index, {match,_,{var,_,_}=StateVar,StateInit0}, Generators) ->
  ResBody = [hd(transform(fun replace/1, [A])) || A <- ResBody0],
  [Init]  =  transform(fun replace/1, [StateInit0]),

  % Split generators from filters
  {Gens, Filters} =
    lists:splitwith(fun(G) -> element(1, G) == generate end, Generators),
  {GLoc, FunArgs, ListOfLCs} =
    case Gens of
      [{generate, Loc0, FunArg0, List0}] when Filters == [] ->
        % Simple case with one generator and no filters:
        % <<{I, S+I} || S = 1, I <- L>>
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
        % <<{I, S+I} || S = 1, I = FunArg1 <- List1, J = FunArg2 <- List2, ...>>
        % - Make a list:
        %      [{I,FunArg1,LCList1}, {J,FunArg2,LCList2}, ...]
        VarsList = lists:reverse(
          lists:foldl(fun({generate, GLoc1, FunArg0, List0}, ALists) ->
            {Var,Match} = maybe_make_var(GLoc1, FunArg0),
            [{Var, Match, List0}|ALists]
          end, [], Gens)),

        % - Create a new list comprehension:
        % <<{I,J,...} || I <- L1, J <- L2, ..., Filters>>
        Vars    = [V || {V,_,_} <- VarsList],
        ArgVars = {tuple, Loc, Vars},
        FArgs   = {tuple, Loc, [A || {_,A,_} <- VarsList]},
        ListLCs = {lc, Loc, ArgVars,
                      [{generate, GLoc, Match, LList}
                       || {{var,GLoc,_}, Match, LList} <- VarsList] ++ Filters},
        {Loc, FArgs, ListLCs}
    end,

  CallMapFoldl = fun(Args, InitForm, StateForm, FunBodyForm) ->
    {call, GLoc,
      {remote,GLoc,{atom,GLoc,lists},{atom,GLoc,mapfoldl}},
      [{'fun', GLoc,
        {clauses,
         [{clause, Loc,
           [Args, StateForm],    % The fun has 2 arguments: ({I,J, ...}, S) -> ...
           [],                   % No guards
           [FunBodyForm]         % Body
          }]}},
       InitForm,
       ListOfLCs]}
  end,

  % Finally, rewrite the call:
  case Index of
    undefined ->
      % <<{I, S+I} || S = 1, I <- L>>
      % Rewrite:
      %   lists:mapfoldl(fun(Arg, State) -> {Arg1, State1} end,
      %                  Init, _ListOfLCs = [{I,J,...} || I <- L1, J <- L2, ...])

      FunBody = {tuple, Loc, ResBody}, % The body of the mapfold's fun is a tuple:

      CallMapFoldl(FunArgs, Init, StateVar, FunBody);
    {var, {_I,_J} = VLoc,_} ->
      % <<{I, S+I} || N, S = 1, I <- L>>
      % Rewrite:
      %   begin
      %     {_V1, {_, _V2}} =
      %       lists:mapfoldl(fun(Arg, {I, State}) -> {Arg1, {I+1, State1}} end,
      %                      {1, Init},
      %                      _ListOfLCs = [{I,J,...} || I <- L1, J <- L2, ...]),
      %     {_V1, _V2}
      %   end
      Init0  = {tuple, VLoc, [{integer, VLoc, 1}, Init]},
      State0 = {tuple, VLoc, [Index, StateVar]},
      % Replace the last element of the ResBody with the tuple containing
      % an increment of the index and that last element:
      {BodyH, BodyT} = lists:split(length(ResBody)-1, ResBody),
      NBody  = BodyH ++ [{tuple, VLoc, [{op, VLoc, '+', Index, {integer, VLoc, 1}}] ++ BodyT}],
      FBody  = {tuple, VLoc, NBody},
      Sfx    = integer_to_list(_I) ++ "_" ++ integer_to_list(_J),
      V1     = {var, GLoc, list_to_atom("_V1@" ++ Sfx)},
      V2     = {var, GLoc, list_to_atom("_V2@" ++ Sfx)},
      {block, GLoc, [
        {match, GLoc, {tuple, GLoc, [V1, {tuple, GLoc, [{var, GLoc, '_'}, V2]}]},
                      CallMapFoldl(FunArgs, Init0, State0, FBody)},
        {tuple, GLoc, [V1, V2]}
      ]}
  end.

%% Fold Comprehension rules
%% ========================
%%
%% [S+I || S = 0, I <- L]
%%   Rewrite: lists:foldl(fun(I, S) -> S+I end, 0, L).
%% [S+I || S = 1, I <- L1, J <- L2]
%%   Rewrite: lists:foldl(fun({I,J}, S) -> {I, S+I} end, 1, [{I,J} || I <- L1, J <- L2]).
%%
%% [I || N, I <- L]
%%   Rewrite: element(1, lists:mapfoldl(fun(I, N) -> {I, N+1} end, 1, L)).

replace3(Loc, ResBody0, Index, Match, Generators) ->
  [FunBody] = transform(fun replace/1, [ResBody0]),
  {Init,StateVar} =
    case {Index, Match} of
      {{var,_,_}, {match,_,{var,VLoc,_}=StateVar0,StateInit0}} ->
        %% [S+I || N, S = 5, I <- L, I > 10]
        %%   Rewrite fold init, and state passed to fun:
        %%     Init:  {1, 5}
        %%     State: {N, S}
        [Init0] = transform(fun replace/1, [StateInit0]),
        {{tuple, VLoc, [{integer, VLoc, 1}, Init0]},
         {tuple, VLoc, [Index, StateVar0]}};
      {undefined, {match,_,{var, _, _}=StateVar0,StateInit0}} ->
        %% [S+I || S = 0, I <- L]
        %%   Rewrite:
        %%     Init:  0
        %%     State: S
        [Init0] = transform(fun replace/1, [StateInit0]),
        {Init0, StateVar0};
      {{var,VLoc,_}, undefined} ->
        %% [I || N, I <- L]
        %%   Rewrite:
        %%     Init:  1
        %%     State: N
        {{integer, VLoc, 1}, undefined}
    end,

  % Split generators from filters
  {Gens, Filters} =
    lists:splitwith(fun(G) -> element(1, G) == generate end, Generators),
  {GLoc, FunArgs, ListOfLCs}  =
    case Gens of
      [{generate, Loc0, FunArg0, List0}] when Filters == [] ->
        % Simple case with one generator and no filters:
        % [S+I || S = 1, I <- L]
        {Loc0, FunArg0, List0};
      [{generate, Loc0, FunArg0, List0}] when Filters /= [] ->
        % Simple case with one generator and no filters:
        %   [S+I || S = 1, {I,_} <- L, I > 1, I < 10]
        % Convert the comprehension with filter into:
        %   lists:mapfoldl(fun({I,_}, S) -> {I,S+I} end, 1, [_V || _V = {I,_} <- L, I > 1, I < 10])
        {VarForm, MatchForm} = maybe_make_var(Loc0, FunArg0),
        {Loc0, FunArg0, {lc, Loc0, VarForm,
                               [{generate, Loc0, MatchForm, List0}| Filters]}};
      _ ->
        % More than one generator:
        % [S+I || S = 1, I = FunArg1 <- List1, J = FunArg2 <- List2, ...]
        % - Make a list:
        %      [{I,FunArg1,LCList1}, {J,FunArg2,LCList2}, ...]
        VarsList = lists:reverse(
          lists:foldl(fun({generate, GLoc, FunArg0, List0}, ALists) ->
            {VarForm,MatchForm} = maybe_make_var(GLoc, FunArg0),
            [{VarForm, MatchForm, List0}|ALists]
          end, [], Gens)),

        % - Create a new list comprehension:
        % [{I,J,...} || I <- L1, J <- L2, ..., Filters]
        Vars    = [V || {V,_,_} <- VarsList],
        ArgVars = {tuple, Loc, Vars},
        FArgs   = {tuple, Loc, [A || {_,A,_} <- VarsList]},
        ListLCs = {lc, Loc, ArgVars,
                      [{generate, GLoc, _Match, LList}
                       || {{var,GLoc,_}, _Match, LList} <- VarsList] ++ Filters},
        {Loc, FArgs, ListLCs}
    end,

  CallMapFoldl = fun(InitForm, StateForm, FoldFun, FunBodyForm) ->
    %  lists:FoldFun(fun(FunArgs, StateForm) -> FunBodyForm end,
    %                InitForm, _ListOfLCs = [{I,J,...} || I <- L1, J <- L2, ...])
    {call, GLoc,
      {remote,GLoc,{atom,GLoc,lists},{atom,GLoc,FoldFun}},
      [{'fun', GLoc,
        {clauses,
         [{clause, Loc,
           [FunArgs, StateForm], % The fun has 2 arguments: ({I,J, ...}, S) -> ...
           [],                   % No guards
           [FunBodyForm]         % Body
          }]}},
       InitForm,
       ListOfLCs]}
  end,

  % Finally, rewrite the call:
  case {Index,Match} of
    {{var,_,_}, {match,_,{var,_VLoc,_}, _}} ->
      % For [FunBody || N, StateVar = Init, ...]
      %   produce: element(2, lists:foldl(fun(I, {N,StateVar}) -> {N+1,FunBody} end, {1,Init}, L)).
      FunBodyForm =
        {tuple, _VLoc, [{op, _VLoc, '+', Index, {integer, _VLoc, 1}}, FunBody]},
      {call, GLoc,
        {atom, GLoc, element},
          [{integer, GLoc, 2}, CallMapFoldl(Init, StateVar, foldl, FunBodyForm)]};

    {undefined, {match,_,{var,_,_}, _}} ->
      % For [FunBody || StateVar = Init, I <- L, ...]
      %   produce: lists:foldl(fun(I, S) -> FunBody end, 1, L).
      CallMapFoldl(Init, StateVar, foldl, FunBody);

    {{var, _VLoc, _}, undefined} ->
      % For [FunBody || N, I <- L]
      %   produce: element(1, lists:mapfoldl(fun(FunBody, N) -> {FunBody, N+1} end, 1, L)).
      FunBodyForm =
        {tuple, _VLoc, [FunBody, {op, _VLoc, '+', Index, {integer, _VLoc, 1}}]},
      {call, GLoc,
        {atom, GLoc, element},
         [{integer, GLoc, 1},
          CallMapFoldl(Init, Index, mapfoldl, FunBodyForm)]}
  end.
