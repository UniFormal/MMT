package info.kwarc.mmt.api.parser.test

object Test {
   val plus = Notation("plus")(SeqArg(1, Delim("+")))
   val umin = Notation("umin")("-", 1)
   val times = Notation("times")(SeqArg(1, Delim("*")))
   val div = Notation("div")(1, "/", 2)
   val fact = Notation("fact")(1, "!")

   val app = Notation("app")(1, "(", SeqArg(2, Delim(",")), ")")
   val lam = Notation("lam")("lam", Var(1,Delim(":")), ".", 2)
   val Pi = Notation("Pi")("Pi", Var(1,Delim(":")), ".", 2)
   val fun = Notation("fun")("fun", 1, 2)
   
   val pair = Notation("pair")("<", 1, SeqArg(2, Delim(",")), ">")
   val tj = Notation("tj")(1, "|-", 2, ":", 3)

   val sembr = Notation("sembr")("[",1,"]")
   val weird = Notation("weird")("W", 1, 2, 3, "W", 4, 5, "W", 6, 7)

   val compr = Notation("compr")("{", Var(1,Delim("in")), "|", 2, "}")
   val repl  = Notation("repl")("{", 2, ":", Var(1,Delim("in")), "}")
   val set   = Notation("set")("{", SeqArg(1, Delim(",")), "}")

   val first = List(plus, umin, sembr, lam, Pi, app, fun, pair, tj, weird, compr)
   val second = List(times, div, fact, repl)
   
   def apply(): TokenList = apply("g |- lam x : fun nat nat . x + y * q * z + x ! : Pi x : nat . nat")
      
   val old = "a + [ - b / < fun c ! d , lam x : t t . t1 [ t2 + t2' ] t3 > ] |- e : f + g"
   val oldW = "W e1 e2 [ e3 + e4 ] W e5 e6 W e7 e8"
   
   def apply(s: String): TokenList = {
      val tl = TokenList(s)
      val sc = new Scanner(tl)
      sc.scan(first)
      sc.scan(second)
      sc.tl
   }
   
   def main(args: Array[String]) {
     apply()
   }
}