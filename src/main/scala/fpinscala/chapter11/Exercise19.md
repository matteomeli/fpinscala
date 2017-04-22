# What laws mutually hold for getState, setState, unit and flatMap for the State monad?

Getting and setting the same state does nothing:

`getState.flatMap(setState) == unit(())`, or

```scala
for {
  x <- get
  _ <- set(x)
} yield ()
```

Setting the state to `s` and getting it back out yields `s`:

`setState(s).flatMap(_ => getState) == unit(s)`, or

```scala
for {
  _ <- set(s)
  x <- get
} yield x
```
