package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend.Controller
import info.kwarc.mmt.api.utils.xml
import objects._
import objects.Conversions._

import scala.xml.Node

/**
  * A Jugement that MMT evaluates in a dynamic fashion at runtime.
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
        Obj.parseTerm(l, NamespaceMap.empty)
      )
    case <Types>{l}{r}</Types> =>
      Types(
        xml.attrL(n, "varname"),
        Obj.parseTerm(l, NamespaceMap.empty),
        Obj.parseTerm(l, NamespaceMap.empty)
      )
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