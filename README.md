# Collection of Erlang Parse Transforms

## Erlang Pipeline (erlpipe)

Inspired by the Elixir's `|>` pipeline operator.
Transforms code from:
```erlang
Result = [Arg1, Arg2]
       / fun1
       / mod:fun2
       / fun3()
       / fun4(Arg3, _)
       / io_lib:format("~p\n", [_])
       / fun6.
```
to the following equivalent:
```erlang
Result = fun6(io_lib:format("~p\n", [fun4(Arg3, fun3(mod2:fun2(fun1(Arg1, Arg2))))])).
```

## Building and using

```
$ make
```

To use `erlpipe`, compile target module with the `+'{parse_transform, erlpipe}'` option,
or include `-compile({parse_transform, erlpipe}).` in your module.
