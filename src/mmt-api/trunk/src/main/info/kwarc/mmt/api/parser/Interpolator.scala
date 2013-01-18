package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import objects._

/*
works under Scala 2.10
to use, instantiate and import MMTContext

class MMTInterpolator(controller: frontend.Controller) {
   var theory : MPath = null
   implicit class MMTContext(sc: StringContext) {
      def mmt(ts: Term*): Term = {
         val strings = sc.parts.iterator
         val args = ts.iterator
         val buf = new StringBuffer(strings.next)
         var sub = Substitution()
         var i = 0
         while(strings.hasNext) {
            val name = LocalName("$_" + i.toString)
            sub = sub ++ Sub(name, args.next)
            buf.append(name)
            buf.append(strings.next)
            i += 1
        }
        val pu = ParsingUnit(null, OMMOD(theory), sub.asContext, buf.toString) 
        val t = controller.termParser(pu)
        t ^ sub
      }
   }
}
*/