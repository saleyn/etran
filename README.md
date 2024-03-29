# Collection of Erlang Parse Transforms

**Author**: Serge Aleynikov <saleyn(at)gmail.com>

**License**: MIT License

[![build](https://github.com/saleyn/etran/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/etran/actions/workflows/erlang.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/etran.svg)](https://hex.pm/packages/etran)
[![Hex.pm](https://img.shields.io/hexpm/dt/etran.svg)](https://hex.pm/packages/etran)

This library includes useful parse transforms including Elixir-like pipeline operator for
cascading function calls.

## Content

| Module      | Description                                                                          |
| ----------- | ------------------------------------------------------------------------------------ |
| `defarg`    | Support default argument values in Erlang functions                                  |
| `erlpipe`   | Elixir-like pipe operator for Erlang                                                 |
| `listcomp`  | Fold Comprehension and Indexed List Comprehension                                    |
| `iif`       | Ternary if function including `iif/3`, `iif/4`, `nvl/2`, `nvl/3` parse transforms    |
| `str`       | Stringification functions including `str/1`, `str/2`, and `throw/2` parse transforms |

## `defarg`: Support default argument values in Erlang functions

Presently the Erlang syntax doesn't allow function arguments to have default
parameters.  Consequently a developer needs to replicate the function
definition multiple times passing constant defaults to some parameters of
functions.

This parse transform addresses this shortcoming by extending the syntax
of function definitions at the top level in a module to have a default
expression such that for `A / Default` argument the `Default` will be
used if the function is called in code without that argument.

Though it might seem more intuitive for programmers coming from other
languages to use the assignment operator `=` for defining default arguments,
using that operator would change the current meaning of pattern matching of
arguments in function calls (i.e. `test(A=10)` is presently a valid expression).
Therefore we chose the `/` operator for declaring default arguments because
it has no valid meaning when applied in declaration of function arguments,
and presently without the `defarg` transform, using this operator
(e.g. `test(A / 10) -> ...`) would result in a syntax error detected by the
compiler.

```erlang
-export([t/2]).

test(A / 10, B / 20) ->
  A + B.
```
The code above is transformed to:
```erlang
-export([t/2]).
-export([t/0, t/1]).

test()    -> test(10);
test(A)   -> test(A, 20);
test(A,B) -> A+B.
```

The arguments with default values must be at the end of the argument list:
```erlang
test(A, B, C / 1) ->    %% This is valid
  ...

test(A / 1, B, C) ->    %% This is invalid
  ...
```

NOTE: The default arguments should be constant expressions.  Function calls in default
arguments are not supported!
```erlang
test(A / erlang:timestamp()) ->     %% !!! Bad syntax
  ...
```

## `erlpipe`: Erlang Pipe Operator

Inspired by the Elixir's `|>` pipeline operator.
This transform makes code with cascading function calls much more readable by using the `/` as the
pipeline operator. In the `LHS / RHS / ... Last.` notation, the result of evaluation of the LHS
expression is passed as an argument to the RHS expression. This process continues until the `Last`
expression is evaluated.  The head element of the pipeline must be either a term to which the
arithmetic division `/` operator cannot apply (i.e. not integers, floats, variables, functions),
or if you need to pass an integer, float, variable, or a result of a function call, wrap it in a
list brackets.

It transforms code from:

```erlang
print(L) when is_list(L) ->
  [3, L]                                         %% Multiple items in a list become arguments to the first function
  / lists:split                                  %% In Module:Function calls parenthesis are optional
  / element(1, _)                                %% '_' is the placeholder for the return value of a previous call
  / binary_to_list
  / io:format("~s\n", [_]).

test1(Arg1, Arg2, Arg3) ->
  [Arg1, Arg2]                                   %% Arguments must be enclosed in `[...]`
  / fun1                                         %% In function calls parenthesis are optional
  / mod:fun2
  / fun3()
  / fun4(Arg3, _)                                %% '_' is the placeholder for the return value of a previous call
  / fun ff/1                                     %% Inplace function references are supported
  / fun erlang:length/1                          %% Inplace Mod:Fun/Arity function references are supported
  / fun(I) -> I end                              %% This lambda will be evaluated as: (fun(I) -> I end)(_)
  / io_lib:format("~p\n", [_])
  / fun6([1,2,3], _, other_param)
  / fun7.

test2() ->
  % Result = Argument   / Function
  3        = abc        / atom_to_list / length, %% Atoms    can be passed to '/' as is
  3        = "abc"      / length,                %% Strings  can be passed to '/' as is
  "abc"    = <<"abc">>  / binary_to_list,        %% Binaries can be passed to '/' as is

  "1,2,3"  = {$1,$2,$3} / tuple_to_list          %% Tuples   can be passed to '/' as is
                        / [[I] || I <- _]        %% The '_' placeholder is replaced by the return of tuple_to_list/1
                        / string:join(","),      %% Here a call to string:join/2 is made

  "1"      = [min(1,2)] / integer_to_list,       %% Function calls, integer and float value
  "1"      = [1]        / integer_to_list,       %% arguments must be enclosed in a list.
  "1.0"    = [1.0]      / float_to_list([{decimals,1}]),
  "abc\n"  = "abc"      / (_ ++ "\n"),           %% Can use operators on the right hand side
  2.0      = 4.0        / max(1.0, 2.0),         %% Expressions with lhs floats are unmodified
  2        = 4          / max(1, 2).             %% Expressions with lhs integers are unmodified

test3() ->
  A   = 10,
  B   = 5,
  2   = A / B,                                   %% LHS variables (e.g. A) are not affected by the transform
  2.0 = 10 / 5,                                  %% Arithmetic division for integers, floats, variables is unmodified
  2.0 = A / 5,                                   %% (ditto)
  5   = max(A,B) / 2.                            %% Use of division on LHS function calls is unaffected by the transform  
```

to the following equivalent:

```erlang
test1(Arg1, Arg2, Arg3) ->
  fun7(fun6([1,2,3],
            io_lib:format("~p\n", [
              (fun(I) -> I end)(
                  erlang:length(
                      ff(fun4(Arg3, fun3(mod2:fun2(fun1(Arg1, Arg2)))))))]),
            other_param)).

print(L) when is_list(L) ->
  io:format("~s\n", [binary_to_list(element(1, lists:split(3, L)))]).

test2() ->
  3       = length(atom_to_list(abc)),
  3       = length("abc"),
  "abc"   = binary_to_list(<<"abc">>),
  "1,2,3" = string:join([[I] || I <- tuple_to_list({$1,$2,$3})], ","),
  "1"     = integer_to_list(min(1,2)),
  "1"     = integer_to_list(1),
  "1.0"   = float_to_list(1.0, [{decimals,1}]),
  "abc\n" = "abc" ++ "\n",
  2.0     = 4.0 / max(1.0, 2.0),
  2       = 4   / max(1, 2).
```

Similarly to Elixir, a special `tap/2` function is implemented, which
passes the given argument to an anonymous function, returning the argument
itself. The following:
```erlang
f(A) -> A+1.
...
test_tap() ->
  [10] / tap(f)
       / tap(fun f/1)
       / tap(fun(I) -> I+1 end).
```
is equivalent to:
```erlang
...
test_tap() ->
  begin
    f(10),
    begin
      f(10),
      begin
        (fun(I) -> I+1 end)(10),
        10
      end
    end
  end.
```

Some attempts to tackle this pipeline transform have been done by other developers:

* https://github.com/fenollp/fancyflow
* https://github.com/stolen/pipeline
* https://github.com/oltarasenko/epipe
* https://github.com/clanchun/epipe
* https://github.com/pouriya/pipeline

Yet, we subjectively believe that the choice of syntax in this implementation of transform
is more succinct and elegant, and doesn't attempt to modify the meaning of the `/` operator
for arithmetic LHS types (i.e. integers, floats, variables, and function calls).

Why didn't we use `|>` operator instead of `/` to make it equivalent to Elixir?
Parse transforms are applied only after the Erlang source code gets parsed to the AST
representation, which must be in valid Erlang syntax.  The `|>` operator is not known to
the Erlang parser, and therefore, using it would result in the compile-time error.  We
had to select an operator that the Erlang parser would be happy with, and `/` was our choice
because visually it resembles the pipe `|` character more than the other operators.

## `listcomp`: Fold and Indexed List Comprehensions

### Indexed List Comprehension

Occasionally the body of a list comprehension needs to know the index
of the current item in the fold.  Consider this example:
```erlang
[{1,10}, {2,20}] = element(1, lists:foldmapl(fun(I, N) -> {{N, I}, N+1} end, 1, [10,20])).
```
Here the `N` variable is tracking the index of the current item `I` in the list.
While the same result in this specific case can be achieved with
`lists:zip(lists:seq(1,2), [10,20])`, in a more general case, there is no way to have
an item counter propagated with the current list comprehension syntax.

The **Indexed List Comprehension** accomplishes just that through the use of an unassigned
variable immediately to the right of the `||` operator:
```erlang
  [{Idx, I} || Idx, I <- L].
%              ^^^
%               |
%               +--- This variable becomes the index counter
```
Example:
```erlang
[{1,10}, {2,20}] = [{Idx, I} || Idx, I <- [10,20]].
```

### Fold Comprehension

To invoke the fold comprehension transform include the initial state
assignment into a list comprehension:
```erlang
  [S+I || S = 1, I <- L].
%  ^^^    ^^^^^
%   |       |
%   |       +--- State variable bound to the initial value
%   +----------- The body of the foldl function
```

In this example the `S` variable gets assigned the initial state `1`, and
the `S+I` expression represents the body of the fold function that
is passed the iteration variable `I` and the state variable `S`:
```erlang
lists:foldl(fun(I, S) -> S+I end, 1, L).
```

A fold comprehension can be combined with the indexed list comprehension
by using this syntax:

```erlang
  [do(Idx, S+I) || Idx, S = 10, I <- L].
%  ^^^^^^^^^^^^    ^^^  ^^^^^^
%       |           |     |
%       |           |     +--- State variable bound to the initial value (e.g. 10)
%       |           +--------- The index variable bound to the initial value of 1
%       +--------------------- The body of the foldl function can use Idx and S
```

This code is transformed to:
```erlang
element(2, lists:foldl(fun(I, {Idx, S}) -> {Idx+1, do(Idx, S+I)} end, {1, 10}, L)).
```

Example:
```erlang
33 = [S + Idx*I || Idx, S = 1, I <- [10,20]],

30 = [print(Idx, I, S) || Idx, S=0, I <- [10,20]].
% Prints:
%   Item#1 running sum: 10
%   Item#2 running sum: 30

print(Idx, I, S) ->
  Res = S+I,
  io:format("Item#~w running sum: ~w\n", [Idx, Res]),
  Res.
```

## `iif`: Ternary and quaternary if

This transform improves the code readability for cases that involve simple conditional
`if/then/else` tests in the form `iif(Condition, Then, Else)`.  Since this is a parse
transform, the `Then` and `Else` expressions are evaluated **only** if the `Condition`
evaluates to `true` or `false` respectively.

E.g.:

```erlang
iif(tuple_size(T) == 3, good, bad).       %% Ternary if

iif(some_fun(A), match, ok, error).       %% Quaternary if

nvl(L, undefined).

nvl(L, nil, hd(L))
```

are transformed to:

```erlang
case tuple_size(T) == 3 of
  true      -> good;
  _         -> bad
end.

case some_fun(A) of
  match     -> ok;
  nomatch   -> error
end.

case L of
  []        -> undefined;
  false     -> undefined;
  undefined -> undefined;
  _         -> L
end.

case L of
  []        -> nil;
  false     -> nil;
  undefined -> nil;
  _         -> hd(L)
end.
```

## `str`: String transforms

This module implements a transform to stringify an Erlang term.

* `str(Term)` is equivalent to `lists:flatten(io_lib:format("~p", [Term]))` for
  terms that are not integers, floats, atoms, binaries and lists.
  Integers, atoms, and binaries are converted to string using `*_to_list/1`
  functions. Floats are converted using `float_to_list/2` where the second
  argument is controled by `str:set_float_fmt/1` and `str:reset_float_fmt/0`
  calls. Lists are converted to string using
  `lists:flatten(io_lib:format("~s", [Term]))` and if that fails, then using
  `lists:flatten(io_lib:format("~p", [Term]))` format.
* `str(Fmt, Args)`  is equivalent to `lists:flatten(io_lib:format(Fmt, Args))`.
* `bin(Fmt, Args)`  is equivalent to `list_to_binary(lists:flatten(io_lib:format(Fmt, Args)))`.
* `throw(Fmt,Args)` is equivalent to `throw(list_to_binary(io_lib:format(Fmt, Args)))`.
* `error(Fmt,Args)` is equivalent to `error(list_to_binary(io_lib:format(Fmt, Args)))`.

Two other shorthand transforms are optionally supported:

* `b2l(Binary)` is equivalent to `binary_to_list(Binary)` (enabled by giving `{d,str_b2l}`)
  compilation option.
* `i2l(Integer)` is equivalent to `integer_to_list(Binary)` (enabled by giving `{d,str_i2l}`)
  compilation option.

E.g.:
```
erlc +debug_info -Dstr_b2l -Dstr_i2l +'{parse_transform, str}' -o ebin your_module.erl
```

## Dowloading

* [Github](https://github.com/saleyn/etran)
* [Hex.pm](https://hex.pm/packages/etran)

## Building and Using

```
$ make
```

To use the transforms, compile your module with the `+'{parse_transform, Module}'` command-line
option, or include `-compile({parse_transform, Module}).` in your source code, where `Module`
is one of the transform modules implemented in this project.

To use all transforms implemented by the `etran` application, compile your module with this
command-line option: `+'{parse_transform, etran}'`.
```
erlc +debug_info +'{parse_transform, etran}' -o ebin your_module.erl
```

If you are using `rebar3` to build your project, then add to `rebar.config`:
```
{deps, [{etran, "0.5.1"}]}.

{erl_opts, [debug_info, {parse_transform, etran}]}.
```
