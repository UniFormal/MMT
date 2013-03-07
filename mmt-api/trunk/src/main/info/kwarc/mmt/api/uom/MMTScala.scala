package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects.Term

trait TheoryScala {
   val _base : DPath
   val _path : MPath
}

trait AbstractTheoryScala {
   var _axioms: List[(String,Boolean)] = Nil
   def _assert(name: String, t: Boolean) {_axioms ::= (name,t)}
   def _test() {
      _axioms.foreach {
         case (n, a) => if (! a) println("test failed: " + n)
      }
   }
}

trait ConstantScala {
   val parent: MPath
   val name: String
   val path: GlobalName = parent ? name
   val term = objects.OMID(path)
}

trait ViewScala extends RuleSet {
   
}

object ConstantScala {
   implicit def constantToTerm(c: ConstantScala) = c.term
}