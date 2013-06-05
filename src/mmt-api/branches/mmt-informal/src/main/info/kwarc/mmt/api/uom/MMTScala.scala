package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects.Term

trait TheoryScalaAux {
   val _base : DPath
   val _path : MPath
}

trait TheoryScala {
   var _axioms: List[(String, Unit => Term, Term => Boolean)] = Nil
   def _assert(name: String, term: Unit => Term, assertion: Term => Boolean) {_axioms ::= ((name, term, assertion))}
   def _test(controller: frontend.Controller, log: String => Unit) {
      _axioms.foreach {
         case (n, tL, a) =>
           log("test case " + n)
           try {
             val t = tL()
             //log("term: " + controller.presenter.asString(t))
             val tS = controller.uom.simplify(t)
             //log("simplified: " + controller.presenter.asString(tS))
             val result = a(tS)
             log((if (result) "PASSED" else "FAILED") + "\n")
           } catch {
             case Unimplemented(f) => log("unimplemented " + f)
           }
           
      }
   }
}

trait ConstantScala {
   val parent: MPath
   val name: String
   lazy val path: GlobalName = parent ? name
   lazy val term = objects.OMID(path)
}

trait ViewScala extends objects.RuleSet with TheoryScala

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
   def register(rs: objects.RuleStore) {
      documents.foreach(_.register(rs))
      views.foreach {v =>
         rs.add(v)
      }
   }
   def test(controller: frontend.Controller, log: String => Unit) {
      documents.foreach {_.test(controller, log)}
      views.foreach {_._test(controller, log)}
   }
}