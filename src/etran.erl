%%%-----------------------------------------------------------------------------
%%% @doc Apply all transforms in the `etran' application
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
-module(etran).

-export([parse_transform/2]).

%% @doc parse_transform entry point
parse_transform(AST, Options) ->
  A0 = defarg:parse_transform  (AST, Options),
  A1 = erlpipe:parse_transform (A0,  Options),
  A2 = iif:parse_transform     (A1,  Options),
  A3 = listcomp:parse_transform(A2,  Options),
  A4 = str:parse_transform     (A3,  Options),
  A5 = gin_transform           (A4,  Options),
  A5.

gin_transform(AST, Options) ->
  %% Apply the `gin' transform if it's found
  %% See: https://github.com/mad-cocktail/gin
  case code:which(gin) of
    non_existing ->
      AST;
    _ ->
      gin:parse_transform(AST, Options)
  end.
