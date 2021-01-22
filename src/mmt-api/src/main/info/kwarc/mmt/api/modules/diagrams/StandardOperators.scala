package info.kwarc.mmt.api.modules.diagrams

/**
  * Foundation-independent diagram operators.
  * Prime example is the pushout operator.
  */

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant

object CopyOperator extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(opSymbol), OMMOD(dom), OMMOD(cod)) => new CopyOperator(opSymbol, dom, cod)
    case _ => throw ParseError("invalid usage. correct usage: rule ...?CopyOperator <operator symbol " +
      "to tie with> <domain theory OMMOD> <codomain theory OMMOD>")
  }
}


class CopyOperator(override val head: GlobalName, dom: MPath, cod: MPath) extends SimpleLinearOperator with OperatorDSL {

  override val operatorDomain: Diagram = Diagram(List(dom))
  override val operatorCodomain: Diagram = Diagram(List(cod))
  override def applyMetaModule(m: Term): Term = ???

  override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_copy")

  val copy1: Renamer[LinearState] = getRenamerFor("1")
  val copy2: Renamer[LinearState] = getRenamerFor("2")

  override def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    List(
      const(copy1(c.path), copy1(tp), df.map(copy1.apply(_))),
      const(copy2(c.path), copy2(tp), df.map(copy2.apply(_)))
    )
  }
}

class CopyConnectorFirst(override val head: GlobalName, copyOp: CopyOperator) extends SimpleLinearConnector with OperatorDSL {

  override val in: LinearFunctorialTransformer = LinearFunctorialTransformer.identity(copyOp.operatorDomain)
  override val out: LinearFunctorialTransformer = copyOp

  override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_copy1")

  override def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {

    val copy1 = copyOp.copy1.coercedTo(state)

    List(assgn(c.path, copy1(c)))
  }
}
