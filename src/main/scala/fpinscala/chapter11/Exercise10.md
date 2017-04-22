# Prove monad identity law is equivalent for compose and flatMap

compose(f, unit) == f
compose(unit, f) == f

flatMap(x)(unit) == x
flatMap(unit(y))(f) = f(y)

For all `v` anf `f`:

compose(f, unit)(v) == f(v)
(a => flatMap(f(a))(unit))(v) == f(v)
flatMap(f(v))(unit) == f(v)

If `x` is substituted for `f(v)`:

flatMap(x)(unit) == x.

compose(unit, f)(x) == f(x)
flatMap(unit(x))(f) == f(x).

Q.E.D
