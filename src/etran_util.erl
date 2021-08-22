%%%-----------------------------------------------------------------------------
%%% @doc    Erlang parse transform utility functions
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
-module(etran_util).
-export([transform/2, transform/3, apply_transform/4]).
-export([parse_options/2, debug_options/2, source_forms/2]).

%%------------------------------------------------------------------------------
%% @doc Transform `Forms' by applying a lambda `Fun'.
%% @end
%%------------------------------------------------------------------------------
-spec transform(fun((Forms::term()) -> tuple()|continue), Forms::term()) -> list().
transform(Fun, Forms) when is_function(Fun, 1), is_list(Forms) ->
  transform2(Fun, Forms);
transform(Fun, Form) when is_function(Fun, 1), is_tuple(Form) ->
  transform2(Fun, [Form]).

transform2(_, []) ->
  [];
transform2(Fun, [L|Fs]) when is_list(L) ->
  [transform2(Fun, L) | transform2(Fun, Fs)];
transform2(Fun, [F|Fs]) when is_tuple(F), is_atom(element(1,F)) ->
  case Fun(F) of
    NewF when is_tuple(NewF) ->
      [NewF | transform2(Fun, Fs)];
    continue ->
      [list_to_tuple(transform2(Fun, tuple_to_list(F))) | transform2(Fun, Fs)]
  end;
transform2(Fun, [F|Fs]) ->
  [F | transform2(Fun, Fs)];
transform2(_, F) ->
  F.

%%------------------------------------------------------------------------------
%% @doc Transform `Forms' by applying a lambda `Fun'.
%% @end
%%------------------------------------------------------------------------------
-spec transform(fun((Forms::term(), State::term()) ->
                    {tuple()|continue, NewState::term()}),
                Forms::term(), State::term()) ->
        {list(), NewState::term()}.
transform(Fun, Forms, State) when is_function(Fun, 2), is_list(Forms) ->
  transform2(Fun, Forms, State).

transform2(_, [], State) ->
  {[], State};
transform2(Fun, [L|Fs], State) when is_list(L) ->
  {Res1, ResSt1} = transform2(Fun, L,  State),
  {Res2, ResSt2} = transform2(Fun, Fs, ResSt1),
  {[Res1 | Res2], ResSt2};
transform2(Fun, [F|Fs], State) when is_tuple(F), is_atom(element(1,F)) ->
  case Fun(F, State) of
    {NewF, NewS} when is_tuple(NewF) ->
      {Res, ResSt} = transform2(Fun, Fs, NewS),
      {[NewF | Res], ResSt};
    {continue, ResSt} ->
      {Res1, ResSt1} = transform2(Fun, tuple_to_list(F), ResSt),
      {Res2, ResSt2} = transform2(Fun, Fs, ResSt1),
      {[list_to_tuple(Res1) | Res2], ResSt2}
  end;
transform2(Fun, [F|Fs], State) ->
  {Res, ResSt} = transform2(Fun, Fs, State),
  {[F | Res], ResSt};
transform2(_, F, State) ->
  {F, State}.

%%------------------------------------------------------------------------------
%% @doc Apply parse transform with debug printing options
%% @end
%%------------------------------------------------------------------------------
apply_transform(Module, Fun, AST, Options) when is_atom(Module)
                                              , is_function(Fun, 1)
                                              , is_list(Options) ->
  M = atom_to_list(Module),
  DbgOrig = list_to_atom(M ++ "_orig"),
  DbgAST  = list_to_atom(M ++ "_ast"),
  DbgSrc  = list_to_atom(M ++ "_src"),
  [OrigAST, ResAST, SrcAST] =
    parse_options([DbgOrig, DbgAST, DbgSrc], Options),

  OrigAST andalso io:format(">>> Before ~s:\n  ~p~n", [M, AST]),
  Transformed   = transform(Fun, AST),
  ResAST  andalso io:format(">>> After ~s:  ~p~n", [M, Transformed]),
  SrcAST  andalso source_forms(Transformed,
                    [print, {format, ">>> Resulting Source:\n  ~s~n"}]),
  Transformed.

%%------------------------------------------------------------------------------
%% @doc Check if `KnownFlags' are found in Options.
%% @end
%%------------------------------------------------------------------------------
-spec parse_options(list(), list()) -> [boolean()].
parse_options(KnownFlags, Options) when is_list(KnownFlags), is_list(Options) ->
  [case lists:keyfind(I, 2, Options) of
     {d, I, Val} -> Val;
     {d, I}      -> true;
     false       -> false
   end || I <- KnownFlags].

%%------------------------------------------------------------------------------
%% @doc Get parse transforms debug options
%% @end
%%------------------------------------------------------------------------------
-spec debug_options(atom(), list()) ->
        #{orig => boolean(), ast => boolean(), src => boolean()}.
debug_options(Module, Options) when is_atom(Module), is_list(Options) ->
  M = atom_to_list(Module),
  DbgOrig = list_to_atom(M ++ "_orig"),
  DbgAST  = list_to_atom(M ++ "_ast"),
  DbgSrc  = list_to_atom(M ++ "_src"),
  [OrigAST, ResAST, SrcAST] =
    parse_options([DbgOrig, DbgAST, DbgSrc], Options),
  #{orig => OrigAST, ast => ResAST, src => SrcAST}.

%%------------------------------------------------------------------------------
%% @doc Decompile source code from the AST
%% @end
%%------------------------------------------------------------------------------
-spec source_forms(list(), list()) -> ok | string().
source_forms(AST, Options) ->
  Res = erl_prettypr:format(erl_syntax:form_list(tl(AST))),
  case lists:member(print, Options) of
    true ->
      io:format(proplists:get_value(format, Options, "~s\n"), [Res]);
    false ->
      Res
  end.
