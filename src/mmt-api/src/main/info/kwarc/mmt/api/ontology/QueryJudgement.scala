package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.xml

import scala.xml.Node

/**
  * A Judgement that MMT evaluates in a dynamic fashion at runtime.
  * Receives a single GlobalName as an argument
  *
  * @param varname
  */
sealed abstract class QueryJudgement(varname: LocalName) {
  def toJudgement(argument : GlobalName) : Judgement
}

object QueryJudgement {
  def parse(n: Node)(implicit queryFunctions: List[QueryFunctionExtension], relManager: RelationalManager): QueryJudgement = n match {
    case <Equals>{l}{r}</Equals> =>
      Equals(
        xml.attrL(n, "varname"),
        Obj.parseTerm(l, NamespaceMap.empty),
        Obj.parseTerm(r, NamespaceMap.empty)
      )
    case <Types>{l}{r}</Types> =>
      Types(
        xml.attrL(n, "varname"),
        Obj.parseTerm(l, NamespaceMap.empty),
        Obj.parseTerm(r, NamespaceMap.empty)
      )
  }

  def parse(t : Term)(implicit queryFunctions: List[QueryFunctionExtension], relManager: RelationalManager): QueryJudgement = t match {
    case OMBINDC(OMID(QMTJudgements.Equals),Context(VarDecl(vname,_,_,_,_)),List(tm1, tm2)) =>
      Equals(vname, tm1, tm2)
    case OMBINDC(OMID(QMTJudgements.Types),Context(VarDecl(vname,_,_,_,_)),List(tm1, tm2)) =>
      Types(vname, tm1, tm2)
  }
}

abstract class BinaryQueryJudgement(varname: LocalName, left : Term, right: Term) extends QueryJudgement(varname) {
  /** turns this queryJudgement into a proper Judgement to be evaluated by the controller */
  def toJudgement(argument : GlobalName) : Judgement = {
    val stack = Stack(Context(argument.module))
    val sub = varname / OMID(argument)

    toJudgementImpl(stack, left ^? sub, right ^? sub)
  }

  /** implementation of judgement conversion */
  protected def toJudgementImpl(stack: Stack, leftSub: Term, rightSub: Term) : Judgement
}


// BUG: judgments should not bind a variable

/** semantic equality between two terms */
case class Equals(varname : LocalName, left : Term, right: Term) extends BinaryQueryJudgement(varname, left, right) {
  def toJudgementImpl(stack: Stack, leftSub: Term, rightSub: Term) : Judgement = {
    Equality(stack, leftSub, rightSub, None)
  }
}

/** semantic type of a term */
case class Types(varname : LocalName, left : Term, right : Term) extends BinaryQueryJudgement(varname, left, right) {
  def toJudgementImpl(stack: Stack, leftSub: Term, rightSub: Term) : Judgement = {
    Typing(stack, leftSub, rightSub, None)
  }
}
