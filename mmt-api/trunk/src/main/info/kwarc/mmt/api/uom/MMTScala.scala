package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects.Term

trait TheoryScalaAux {
   val _base : DPath
   val _path : MPath
}

trait TheoryScala {
   var _axioms: List[(String, Term, Term => Boolean)] = Nil
   def _assert(name: String, term: Term, assertion: Term => Boolean) {_axioms ::= ((name, term, assertion))}
   def _test(controller: frontend.Controller) {
      _axioms.foreach {
         case (n, t, a) =>
           println("test case " + n)
           println("term: " + controller.presenter.asString(t))
           val tS = controller.uom.simplify(t)
           println("simplified: " + controller.presenter.asString(tS))
           val result = try {a(tS).toString}
                        catch {case Unimplemented(f) => "unimplemented " + f}
           println(s"result: $result\n")
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
      views.foreach(rs.add)
   }
   def test(controller: frontend.Controller) {
      documents.foreach {_.test(controller)}
      views.foreach {_._test(controller)}
   }
}