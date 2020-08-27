This is a basic tetris that I made while learning haskell.

## Building and running
If you use nix, you can install dependencies using

```shell
nix develop
```

Alternatively, install ghc with `brick`, `random-shuffle`, `tf-random` and `timers` some other
way. To play tetris in the interpreter:


```shell
runhaskell -Wall -XNamedFieldPuns Tetris.Main
```

or if you want an executable:

```shell
ghc -threaded -O -Wall -XNamedFieldPuns Tetris.Main
./Tetris/Main
```
