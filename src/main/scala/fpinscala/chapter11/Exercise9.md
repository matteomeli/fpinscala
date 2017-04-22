# Demonstrate monad associative law for compose and flatMap

You want to show that these two are equivalent:

flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
compose(compose(f, g), h) == compose(f, compose(g, h))

Rewrite one in terms of the other.

compose(compose(f, g), h) == compose(f, compose(g, h))
a => flatMap(compose(f, g)(a))(h) == a => flatMap(f(a))(compose(g, h))
a => flatMap((b => flatMap(f(b))(g))(a))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
a => flatMap(flatMap(f(a))(g))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))

If argument `a` is eliminated and `x` is substituted for `f(a)`

flatMap(flatMap(x)(g))(h) == flatMap(x)(b => flatMap(g(b))(h))

Q.E.D
