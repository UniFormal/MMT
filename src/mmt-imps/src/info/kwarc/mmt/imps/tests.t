(herald Foobar)

(load-section foundation)

(def-atomic-sort NN
  "lambda(x:zz, 0<=x)"
  (theory h-o-real-arithmetic)
  (witness "0"))

(def-constant ABS
  "lambda(r:rr, if(0<=r,r,-r))"
  (theory h-o-real-arithmetic))

(def-constant >
  "lambda(x,y:rr,y<x)"
  (theory h-o-real-arithmetic)
  (usages rewrite foobar))

(def-constant POWER%OF%TWO
  "lambda(x:zz,2^x)"
  (sort "[zz,zz]")
  (theory h-o-real-arithmetic))

(def-quasi-constructor PREDICATE-TO-INDICATOR
  "lambda(s:[uu,prop],
  lambda(x:uu, if(s(x),an%individual, ?unit%sort)))"
  (language indicators)
  (fixed-theories the-kernel-theory))

(def-cartesian-product complex
  (rr rr)
  (constructor make%complex)
  (accessors real imaginary)
  (theory h-o-real-arithmetic))

(def-imported-rewrite-rules target-theory
  (src-theories foo bar baz))

(def-schematic-macete ABSTRACTION-FOR-DIFF-PROD
  "with(a,b:rr,y:rr,
    diff(lambda(x:rr,a*b))(y)=
    diff(lambda(x:rr,
    lambda(x:rr,a)(x)*lambda(x:rr,b)(x)))(y))"
  null
  (theory calculus-theory))
