package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects.Term

trait TheoryScalaAux {
   val _base : DPath
   val _path : MPath
}

trait TheoryScala {
   var _axioms: List[(String, Unit => Boolean)] = Nil
   def _assert(name: String, t: => Boolean) {_axioms ::= ((name, _ => t))}
   def _test() {
      _axioms.foreach {
         case (n, a) =>
           val result = try {a().toString}
                        catch {case Unimplemented(f) => "unimplemented " + f}
           println(s"test $n: $result")
      }
   }
}

trait ConstantScala {
   val parent: MPath
   val name: String
   lazy val path: GlobalName = parent ? name
   lazy val term = objects.OMID(path)
}

trait ViewScala extends RuleSet with TheoryScala

object ConstantScala {
   implicit def constantToTerm(c: ConstantScala) = c.term
}

trait DocumentScala {
   private var theories: List[TheoryScalaAux] = Nil
   private var views: List[ViewScala] = Nil
   private var documents : List[DocumentScala] = Nil
   def addTheory(t: TheoryScalaAux) {
      theories = theories ::: List(t)
   }
   def addView(v: ViewScala) {
      views = views ::: List(v)
   }
   def addDocument(d: DocumentScala) {
      documents = documents ::: List(d)
   }
   def register(uom: UOM) {
      documents.foreach(_.register(uom))
      views.foreach(uom.register)
   }
   def test {
      documents.foreach {_.test}
      views.foreach {_._test}
   }
}