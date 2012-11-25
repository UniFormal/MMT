package info.kwarc.mmt.api.parser.test

object Test {
   val plus = Notation(1,"+",2)
   val sembr = Notation("[",1,"]")
   val fact = Notation(1, "!")
   val umin = Notation("-", 1)
   val nots = List(plus, sembr, fact, umin)
   
   def apply(): TokenList = apply("a + b")
   
   def apply(s: String): TokenList = {
      val tl = TokenList(s)
      val sc = new Scanner(tl)
      sc.scan(nots)
      sc.tl
   }
}