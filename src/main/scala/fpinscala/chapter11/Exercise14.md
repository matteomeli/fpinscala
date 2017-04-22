# Restate the monad laws only in terms of join, map and unit

## Associative law

x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

It's valid _for all_ `f` and `g`, so let's pick the identity function:

x.flatMap(z => z).flatMap(z => z) == x.flatMap(a => a.flatMap(z => z))  // flatMap(identity) == join
join(join(x)) == x.flatMap(join)  // flatMap == map(join)
join(join(x)) == join(map(x)(join))

## Identity law

flatMap(x)(unit) == x
join(map(x)(unit)) == x

flatMap(unit(y))(f) == f(y) // If `f` is the identity
join(unit(y)) == y
