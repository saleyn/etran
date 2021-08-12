# Collection of Erlang Parse Transforms

[![build](https://github.com/saleyn/etran/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/etran/actions/workflows/erlang.yml)

## Author

Serge Aleynikov <saleyn(at)gmail.com>

## Included transform modules

| Module                | Description                                                                          |
| --------------------- | ------------------------------------------------------------------------------------ |
| erlpipe               | Elixir-like pipeline for Erlang                                                      |
| iif                   | Ternary if function including `iif/3`, `iif/4`, `ife/3`, `ife/4` parse transforms    |
| str                   | Stringification functions including `str/1` and `str/2` parse transforms             |

### Erlang Pipeline (erlpipe)

Inspired by the Elixir's `|>` pipeline operator.
This tranform makes code with cascading function calls much more readable.
It transforms code from:

```erlang
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
test(Arg1, Arg2) ->
  fun7(fun6([1,2,3],
            io_lib:format("~p\n", [fun4(Arg3, fun3(mod2:fun2(fun1(Arg1, Arg2))))]),
            other_param)).
```

### Ternary if

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

## Building and Using

```
$ make
```

To use `erlpipe`, compile your module with the `+'{parse_transform, Module}'` command-line
option, or include `-compile({parse_transform, Module}).` in your source code, where `Module`
is one of the transform modules implemented in this project.
