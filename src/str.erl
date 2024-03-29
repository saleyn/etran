%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc Parse transform that implements `str/2'
%%%
%%% Use `{parse_transform,str}' compiler's option to use this transform.
%%% ```
%%% str(Fmt, Args)     -> lists:flatten(io_lib:format(Fmt, Args))
%%% bin(Fmt, Args)     -> list_to_binary(io_lib:format(Fmt, Args))
%%% throw(Fmt, Args)   -> erlang:throw(list_to_binary(io_lib:format(Fmt, Args))
%%% error(Fmt, Args)   -> erlang:error(list_to_binary(io_lib:format(Fmt, Args))
%%% i2l(Int)           -> integer_to_list(Int)      % Enabled with compiled with
%%%                                                 % the `{d,str_i2l}' option
%%% b2l(Bin)           -> binary_to_list(Bin)       % Enabled with compiled with
%%%                                                 % the `{d,str_b2l}' option
%%% str(Term)          -> str:str(Term)
%%% bin(Term)          -> str:bin(Term)
%%% '''
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
-module(str).

% If using this module as a parse transform, we need to export the following:
-export([parse_transform/2]).
-export([str/1, str/2, bin/1, bin/2]).
-export([reset_float_fmt/0, set_float_fmt/1, get_float_fmt/0]).

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

%% @doc Stringify an argument
-spec str(term()) -> string().
str(I) when is_list(I)    ->
  lists:flatten(
    try          io_lib:format("~s", [I])
    catch _:_ -> io_lib:format("~p", [I])
    end);
str(I) when is_integer(I) -> integer_to_list(I);
str(I) when is_binary(I)  -> binary_to_list(I);
str(I) when is_float(I)   -> float_to_list(I, get_float_fmt());
str(I) when is_atom(I)    -> atom_to_list(I);
str(I) ->
  lists:flatten(io_lib:format("~p", [I])).

-type fmt_args() :: [
  {decimals, Decimals :: 0..253} |
  {scientific, Decimals :: 0..249} |
  compact | short
].

%% @doc Stringify an argument with options passed to float_to_list/2 when 
%% the first argument is a float
-spec str(term(), fmt_args()) -> string().
str(I, undefined) when is_float(I) -> float_to_list(I);
str(I, Opts)      when is_float(I) -> float_to_list(I, Opts);
str(I,_Opts)                       -> str(I).

%% @doc Stringify an argument and return as binary
-spec bin(term()) -> binary().
bin(I) -> list_to_binary(str(I)).

%% @doc Stringify an argument and return as binary
-spec bin(term(), fmt_args()) -> binary().
bin(I, undefined) when is_float(I)   -> float_to_binary(I);
bin(I, Opts)      when is_float(I)   -> float_to_binary(I, Opts);
bin(I, _)         when is_binary(I)  -> I;
bin(I, _)         when is_list(I)    ->
  list_to_binary(
    try          io_lib:format("~s", [I])
    catch _:_ -> io_lib:format("~p", [I])
    end);
bin(I, _)         when is_integer(I) -> integer_to_binary(I);
bin(I, _)         when is_atom(I)    -> atom_to_binary(I);
bin(I, _)                            -> list_to_binary(io_lib:format("~p", [I])).

%% @doc Erase custom float format from the process dictionary
reset_float_fmt()   -> erase(float_fmt).

%% @doc Store custom float format in the process dictionary
%%      Return previously stored format.
%% Also see float_to_list/2 [http://erlang.org/doc/man/erlang.html#float_to_list-2]
set_float_fmt(Opts) -> V=get(float_fmt), put(float_fmt, Opts), V.

%% @doc Get custom float format from the process dictionary
get_float_fmt()     -> get(float_fmt).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-record(opts, {
  i2l = false,
  b2l = false
}).

%% @doc Parse transform to be used by providing `{parse_transform, str}' option.
parse_transform(AST, Opts) ->
  I2L  = lists:member({d,str_i2l}, Opts),
  B2L  = lists:member({d,str_b2l}, Opts),
  Tree = erl_syntax:form_list(AST),
  ModifiedTree = recurse(Tree, #opts{i2l=I2L, b2l=B2L}),
  erase(line),
  erl_syntax:revert_forms(ModifiedTree).

%% Parse transform support
recurse(Tree, Opt) ->
  update(case erl_syntax:subtrees(Tree) of
           []   -> Tree;
           List -> erl_syntax:update_tree(Tree, [[recurse(Subtree, Opt) || Subtree <- Group]
                                                 || Group <- List])
         end, Opt).

syn_atom(A, Line) -> erl_syntax:set_pos(erl_syntax:atom(A), Line).
syn_call(F,A)     -> L=get(line),
                     erl_syntax:set_pos(
                       erl_syntax:application(syn_atom(F, L), A), L).
syn_call(M,F,A)   -> L=get(line),
                     erl_syntax:set_pos(
                       erl_syntax:application(syn_atom(M, L), syn_atom(F, L), A), L).

update(Node, Opt)                -> update2(Node, erl_syntax:type(Node), Opt).
update2(Node, application, Opt)  -> update3(Node, erl_syntax:application_operator(Node), Opt);
update2(Node, _, _)              -> Node.

update3(Node, {atom, L, F}, Opt) -> update4(F, Node, L, Opt);
update3(Node, _, _)              -> Node.

update4(Arg, Node, Line, _Opt) when Arg==str; Arg==bin ->
  %% Replace str(A, B) -> lists:flatten(io_lib:format(A, B)).
  %%         str(A)    -> str:str(A).
  put(line, Line),
  case erl_syntax:application_arguments(Node) of
    [A,B] ->
      %% This is a call to str(Fmt, Args).
      %% Replace it with:
      %%   lists:flatten(io_libs:format(Fmt, Args)
      Res = syn_call(lists, flatten, [syn_call(io_lib, format, [A,B])]),
      case Arg of
        str -> Res;
        bin -> syn_call(erlang, list_to_binary, [Res])
      end;
    [A] ->
      %% This is a call to str(Arg).
      %% Replace it with:
      %%   str:str(Args) or str:bin(Args)
      syn_call(str, Arg, [A]);
    _ ->
      Node
  end;
update4(I, Node, Line, _Opt) when I==throw; I==error ->
  %% Replace throw(A, B) -> throw(list_to_binary(io_lib:format(A, B))).
  %% Replace error(A, B) -> error(list_to_binary(io_lib:format(A, B))).
  put(line, Line),
  case erl_syntax:application_arguments(Node) of
    [A,B] ->
      syn_call(I, [syn_call(erlang, list_to_binary, [syn_call(io_lib, format, [A,B])])]);
    _ ->
      Node
  end;
update4(i2l, Node, Line, #opts{i2l=true}) ->
  %% Replace i2l(A) -> integer_to_list(A).
  put(line, Line),
  case erl_syntax:application_arguments(Node) of
    [A] -> syn_call(integer_to_list, [A]);
    _   -> Node
  end;
update4(b2l, Node, Line, #opts{b2l=true}) ->
  %% Replace b2l(A) -> binary_to_list(A).
  put(line, Line),
  case erl_syntax:application_arguments(Node) of
    [A] -> syn_call(binary_to_list, [A]);
    _   -> Node
  end;
update4(_, Node, _, _) ->
  Node.
