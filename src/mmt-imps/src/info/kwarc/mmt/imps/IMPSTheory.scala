package info.kwarc.mmt.imps

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.{Lambda, Apply, ApplySpine}
import utils._

object IMPSTheory
{
  val rootdpath = DPath(URI.http colon "latin.omdoc.org/foundations/lutins")
  val thname = "Lutins"

  val thpath = rootdpath ? thname

  class Sym(s: String)
  {
    val path : GlobalName = thpath ? s
    val term = OMS(path)
  }

  object tp extends Sym("tp")

  object exp extends Sym("exp")
  {
    def apply(t:Term) = Apply(this.term,t)
  }

  object Truth extends Sym("thetrue")
  object Falsehood extends Sym("thefalse")

  object Or extends Sym("or") {
    def apply(ls : List[Term]) : Term = {
      assert (ls.nonEmpty)
      ls.init.foldRight(ls.last)((t,r) => ApplySpine(this.term,t,r))
    }
  }

  object And extends Sym("and"){
    def apply(ls : List[Term]) : Term = {
      assert (ls.nonEmpty)
      ls.init.foldRight(ls.last)((t,r) => ApplySpine(this.term,t,r))
    }
  }

  object Equals extends Sym("equals") {
    def apply(p : Term, q : Term) : Term = {
      ApplySpine(this.term, p, q)
    }
  }

  object Iff extends Sym("iff") {
    def apply(p : Term, q : Term) : Term = {
      ApplySpine(this.term, p, q)
    }
  }

  object Implies extends Sym("implies") {
    def apply(p : Term, q : Term) : Term = {
      ApplySpine(this.term, p, q)
    }
  }

  object IMPSLambda extends Sym("lambda")
  {
    def apply(ls : List[(LocalName,Option[Term])], t : Term) = ls match
    {
      case Nil => ()
      case _ => ls.foldRight(t)((tm,p) => ApplySpine(this.term,Lambda(tm._1, tm._2.get, p)))
    }
  }

}