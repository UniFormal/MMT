package info.kwarc.mmt.imps

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.lf.{Lambda, Apply, ApplySpine}
import utils._
import objects.Conversions._

object IMPSTheory
{
  val rootdpath = DPath(URI.http colon "latin.omdoc.org/foundations/lutins")
  val thname = "Lutins"

  val thpath = rootdpath ? thname

  class Sym(s: String)
  {
    val path = thpath ? s
    val term = OMS(path)
  }

  object tp extends Sym("tp")

  object exp extends Sym("exp")
  {
    def apply(t:Term) = Apply(this.term,t)
  }
/*
  object impsLambda extends Sym("lambda")
  {
    def apply(typeA : Term, typeB : Term, sortA : Term, sortB : Term, funAB : Term) =
    {
       ApplySpine(this.term, typeA, typeB, sortA, sortB, funAB)
    }
  }
  */
  object IMPSLambda extends Sym("lambda")
  {
    def apply(ls : List[(LocalName,Option[Term])], t : Term) = ls match {
      case Nil => ()
      case _ =>
        val sort : Term = ??? /* p._2.getOrElse(doUnknown) */
        ls.foldRight(t)((tm,p) => ApplySpine(this.term,doUnknown,doUnknown,sort,doUnknown,
        Lambda()
      ))
    }
  }
}