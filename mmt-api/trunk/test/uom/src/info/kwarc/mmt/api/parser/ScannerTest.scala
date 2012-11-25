package info.kwarc.mmt.api.parser.test

object Test {
   val plus = Notation("plus")(1,"+",2)
   val sembr = Notation("sembr")("[",1,"]")
   val fact = Notation("fact")(1, "!")
   val umin = Notation("umin")("-", 1)
   val fun = Notation("fun")("fun", 1, 2)
   val pair = Notation("pair")("<", 1, ",", 2, ">")
   val weird = Notation("weird")("W", 1, 2, 3, "W", 4, 5, "W", 6, 7)
   val tj = Notation("tj")(1, "|-", 2, ":", 3)
   val lam = Notation("lam")("lam", 1, ".", 2)
   
   val nots = List(plus, sembr, fact, umin, fun, pair, tj, lam, weird)
   
   def apply(): TokenList = apply("a + [ - b + < fun c ! d , lam x . t1 [ t2 ] t3 > ] |- e : f")
   
   def apply(s: String): TokenList = {
      val tl = TokenList(s)
      val sc = new Scanner(tl)
      sc.scan(nots)
      sc.tl
   }
   
   def main(args: Array[String]) {
     apply("W e1 e2 [ e3 + e4 ] W e5 e6 W e7 e8")
   }
}