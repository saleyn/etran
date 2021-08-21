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
-export([transform/2]).

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
