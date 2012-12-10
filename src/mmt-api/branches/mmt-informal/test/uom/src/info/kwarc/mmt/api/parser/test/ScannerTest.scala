package info.kwarc.mmt.api.parser.test
import info.kwarc.mmt.api._
import utils.MyList._
import NotationConversions._

object Test {
   var notations:List[Notation] = Nil
   var qnotations:List[(Int,List[Notation])] = Nil
   def binder(s:String) = Notation("bind" + s, 10)(s, "1:_", ".", -2)
   def bass(s:String) = Notation(s,5)(1 by s)
   def bigop(s:String) = Notation("big" + s,16)(s, Var(2, "="), "^", 1, ".", -3)
   def register(ns:Notation*) {
      ns.foreach {n => notations ::= n} 
   }
   def init {register(
      Notation("umin",2)("-", 1),
      Notation("div",10)(1, "/", 2),
      Notation("fact",2)(1, "!"),
   
      Notation("app",101)("@", "(", 2 by ",", ")"),
      Notation("fun",10)("fun", 1, 2),
      Notation("nfun",5)(1 by "×", "→", 2),
   
      binder("λ"),
      binder("Π"),
      binder("∀"),
      binder("∃"),
      
      Notation("pair",10)("<", 1, 2 by ",", ">"),
      Notation("tj",10)(1, "|", SecDelim("-",false), 2, ":", 3),
   
      Notation("sembr", 10)("⟦",1,"⟧"),
      Notation("weird",10)("W", 1, 2, 3, "W", 4, 5, "W", 6, 7),
   
      bass("+"),
      bass("*"),
      bass("∪"),
      bass("∩"),
      bass("∧"),
      bass("∨"),
      
      bigop("∏"),
      bigop("∑"),
      bigop("⋀"),
      bigop("⋁"),
      
      bass(";"),
      Notation("ell",10)("[", -3, "]", Var(2, "="), 1),
      
      Notation("elem",15)(1, "∊", 2),
      Notation("equal",15)(1, "=", 2),
      Notation("compr",20)("{", "1∊_", "|", 2, "}"),
      Notation("repl",19)("{", 2, ":", "1∊_", "}"),
      Notation("set",18)("{", 1 by ",", "}"),
      Notation("()", 100)("(",1,")")
      
      )
      qnotations = notations.quotient(_.priority).sortBy(_._1)
   }

   
   def apply(): TokenList = apply("g|-λ x:nat×nat×nat→nat.x+y*q*z+x!:Π x:nat.nat")
   
   val t1 = "(∑ i=1^n.i*i)={f∊nat×nat×nat→nat|@(f,x,y,x+z)=x+y+x*y*z}" 
      
   val old = "a + [ - b / < fun c ! d , lam x : t t . t1 [ t2 + t2' ] t3 > ] |- e : f + g"
   val oldW = "W e1 e2 [ e3 + e4 ] W e5 e6 W e7 e8"
   init
   
   def apply(s: String): TokenList = {
      val tl = TokenList(s)
      val sc = new Scanner(tl)
      qnotations reverseMap {
         case (priority,nots) => sc.scan(nots)
      }
      sc.tl
   }
   
   def main(args: Array[String]) {
     apply()
   }
}