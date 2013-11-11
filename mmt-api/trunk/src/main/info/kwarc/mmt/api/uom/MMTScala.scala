package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._

trait RealizationInScala extends objects.RuleSet {
   val _domain: TheoryScala
   lazy val _path = _domain._path
   lazy val _name = _domain._name
   
   private[api] var _types : List[Unit => (GlobalName, RealizedType)] = Nil
   private[api] var _opers : List[Unit => RealizedOperator] = Nil
   
   /** add a realization of synType as rt (argument must be lazy because declares is called in initializer of trait) */
   def declares(synType: GlobalName)(rt: => RealizedType) {
      _types ::= (_ => (synType,rt))
   }
   /** add a realization of r.op as r (argument must be lazy because declares is called in initializer of trait) */
   def declares(r: => RealizedOperator) {_opers ::= (_ => r)}
   
   var _axioms: List[(String, Unit => Term, Term => Boolean)] = Nil
   def _assert(name: String, term: Unit => Term, assertion: Term => Boolean) {_axioms ::= ((name, term, assertion))}
   def _test(controller: frontend.Controller, log: String => Unit) {
      _axioms.foreach {
         case (n, tL, a) =>
           log("test case " + n)
           try {
             val t = tL()
             //log("term: " + controller.presenter.asString(t))
             val tS = controller.uom.simplify(t, objects.OMMOD(_path))
             //log("simplified: " + controller.presenter.asString(tS))
             val result = a(tS)
             log((if (result) "PASSED" else "FAILED") + "\n")
           } catch {
             case Unimplemented(f) => log("unimplemented " + f + "\n")
             case e: Error => log("error :" + e.toString + "\n")
           }
           
      }
   }
}

trait TheoryScala {
   val _base : DPath
   val _name : LocalName
   lazy val _path = _base ? _name
}

trait ConstantScala {
   val parent: MPath
   val name: String
   lazy val path: GlobalName = parent ? name
   lazy val term = objects.OMID(path)
}

trait ViewScala extends TheoryScala

object ConstantScala {
   implicit def constantToTerm(c: ConstantScala) = c.term
}

trait DocumentScala {
   private var realizations: List[RealizationInScala] = Nil
   private var documents : List[DocumentScala] = Nil
   def addRealization(r: RealizationInScala) {
      realizations ::= r
   }
   def addDocument(d: DocumentScala) {
      documents ::= d
   }
   def register(rs: objects.RuleStore) {
      documents.foreach(_.register(rs))
      realizations.foreach {v =>
         rs.add(v)
      }
   }
   def test(controller: frontend.Controller, log: String => Unit) {
      documents.foreach {_.test(controller, log)}
      realizations.foreach {_._test(controller, log)}
   }
}

