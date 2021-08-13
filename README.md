# Collection of Erlang Parse Transforms

[![build](https://github.com/saleyn/etran/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/etran/actions/workflows/erlang.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/etran.svg)](https://hex.pm/packages/etran)
[![Hex.pm](https://img.shields.io/hexpm/dt/etran.svg)](https://hex.pm/packages/etran)

## Author

Serge Aleynikov <saleyn(at)gmail.com>

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

## Content

| Module                | Description                                                                          |
| --------------------- | ------------------------------------------------------------------------------------ |
| erlpipe               | Elixir-like pipeline for Erlang                                                      |
| iif                   | Ternary if function including `iif/3`, `iif/4`, `ife/3`, `ife/4` parse transforms    |
| str                   | Stringification functions including `str/1`, `str/2`, and `throw/2` parse transforms |

### Erlang Pipeline (`erlpipe`)

Inspired by the Elixir's `|>` pipeline operator.
This tranform makes code with cascading function calls much more readable.
It transforms code from:

```erlang
print(L) when is_list(L) ->
  [lists:split(3, L)]
    / element(1, _)
    / io:format("~s\n", [_]).

test(Arg1, Arg2) ->
  [Arg1, Arg2]
  / fun1
  / mod:fun2
  / fun3()
  / fun4(Arg3, _)
  / io_lib:format("~p\n", [_])
  / fun6([1,2,3], _, other_param).
  / fun7.
```

to the following equivalent:

```erlang
print(L) when is_list(L) ->
  io:format("~s\n", [element(1, lists:split(3, L))]).

test(Arg1, Arg2) ->
  fun7(fun6([1,2,3],
            io_lib:format("~p\n", [fun4(Arg3, fun3(mod2:fun2(fun1(Arg1, Arg2))))]),
            other_param)).
```

Similar attempts to tackle this pipeline transform have been done by other developers:

* https://github.com/stolen/pipeline
* https://github.com/oltarasenko/epipe
* https://github.com/clanchun/epipe
* https://github.com/pouriya/pipeline

### Ternary if (`iif`)

This transform improves the code readability for cases that involve simple conditional tests.
E.g.:

```
iif(tuple_size(T) == 3, good, bad).

iif(some_fun(A), match, ok, error).

nvl(L, undefined).

nvl(L, nil, hd(L))
```

are transformed to:

```

if tuple_size(T) == 3 ->
  good;
true ->
  bad
end.

case some_fun(A) of
  match   -> ok;
  nomatch -> error
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

### String transforms (`str`)

This module implements a transform to stringify an Erlang term.

* `str(Term)`       is equivalent to `lists:flatten(io_lib:format("~p", [Term]))`.
* `str(Fmt, Args)`  is equivalent to `lists:flatten(io_lib:format(Fmt,    Args))`.
* `throw(Fmt,Args)` is equivalent to `throw(lists:flatten(io_lib:format(Fmt, Args)))`.
