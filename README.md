# Collection of Erlang Parse Transforms

[![build](https://github.com/saleyn/etran/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/etran/actions/workflows/erlang.yml)

## Author

Serge Aleynikov <saleyn(at)gmail.com>

## Erlang Pipeline (erlpipe)

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

## Building and using

```
$ make
```

To use `erlpipe`, compile your module with the `+'{parse_transform, erlpipe}'` command-line
option, or include `-compile({parse_transform, erlpipe}).` in your source code.
