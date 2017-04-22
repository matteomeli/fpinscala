# Prove monad identity law holds for a monad of your choice

Proving the monad identity law for Option:

flatMap(x)(unit) == x
flatMap(Some(v))(unit) == Some(v)
unit(v) == Some(v)
Some(v) == Some(v)

flatMap(x)(unit) == x
flatMap(None))(unit) == None
None == None

flatMap(unit(y))(f) == f(y)
flatMap(Some(y))(f) == f(y)
f(y) == f(y)
Some(y) == Some(y)  // If f(y) returns Some(y)
None == None        // If f(y) fails with None

Q.E.D
