package info.kwarc.mmt.moduleexpressions.newoperators

// Not yet created, but useful comments/spec in this file

/*
  Quotient operator (needs quotient types in logic)

  t: tp
    |-> t^p: tp
        t^q: tm t^p -> tm t^p -> prop
        <axioms on t^q>

  f: tm t_1 -> ... -> tm t_n -> tm t
    |-> f^p: tm t_1^p -> ... -> tm t_n^p -> tm t^p
        f^q: |- forall [x_1: tm t_1^p ... x_n: tm t_n^p]             "t^q is congruence wrt. f"
                  forall [x_1': tm t_1^p ... x_n': tm t_n^p]
                     t_1^q x_1 x_1' -> ... -> t_n^q x_n x_n' -> t^q (f^p x_1 ... x_n) (f^p x_1' ... x_n')

  c: tm t_1 -> ... -> tm t_n -> prop
    |-> c^p: tm t_1^p -> ... -> tm t_n^p -> prop
        c^q: |- forall [x_1: tm t_1^p ... x_n: tm t_n^p]
                  forall [x_1': tm t_1^p ... x_n': tm t_n^p]
                     t_1^q x_1 x_1' -> ... -> t_n^q x_n x_n' -> (c^p x_1 ... x_n) <-> (c^p x_1' ... x_n')

  map `mod: T -> Quot(T)`

  t |-> t^p quot. t^q
  f |-> [x_1: tm (t_1^p quot. t_1^q), ..., x_n: tm (t_n^p quot. t_n^q)]
           eqv. class of (f^p (some repr of x_1 in t_1^p) ... (some repr of x_n in t_n^p))
  c |-> [x_1: tm (t_1^p quot. t_1^q), ..., x_n: tm (t_n^p quot. t_n^q)]
           (c^p (some repr of x_1 in t_1^p) ... (some repr of x_n in t_n^p))
  ax |-> ???
 */