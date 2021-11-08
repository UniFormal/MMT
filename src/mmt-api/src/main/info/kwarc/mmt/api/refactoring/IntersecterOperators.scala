package info.kwarc.mmt.api.refactoring

/**
  * Exposes the intersection refactoring tools from Intersecter.scala
  * as diagram operators.
  */

import info.kwarc.mmt.api.{GlobalName, InvalidElement, InvalidObject, Path}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.diagrams.{Diagram, DiagramInterpreter, NamedOperator}
import info.kwarc.mmt.api.modules.{Module, View}
import info.kwarc.mmt.api.objects.{OMA, OMMOD, OMS, Term}

private abstract class IntersecterOperator extends NamedOperator {
  protected def getIntersecter(ctrl: Controller): Intersecter

  override def apply(diagram: Term)(implicit interp: DiagramInterpreter): Option[Term] = diagram match {
    case OMA(OMS(`head`), List(OMMOD(intersectionViewPath))) =>
      val intersectionView = interp.ctrl.getO(intersectionViewPath) match {
        case Some(v: View) => v
        case Some(e) =>
          interp.errorCont(InvalidElement(e, s"Path given to Intersecter does not resolve to a View, but instead to `$e`"))
          return None
        case None =>
          interp.errorCont(InvalidObject(OMMOD(intersectionViewPath), "Path does not resolve"))
          return None
      }

      val outputModules: List[Module] = {
        // I don't know what intersecter returns, semantically, so I just choose dumb variable names
        val (a, b, c, d) = getIntersecter(interp.ctrl)(intersectionView)
        (a ::: b.flatMap(p => List(p._1, p._2)) ::: c ::: d.flatMap(p => List(p._1, p._2))).distinct
      }

      Some(Diagram(outputModules.map(_.path)).toTerm)

    case _ =>
      None
  }
}

/**
  * Exposes [[UnaryIntersecter]] as a [[NamedOperator]].
  *
  * Usage: `[notation for head symbol] ?intersectionView`.
  */
private object UnaryIntersecterOperator extends IntersecterOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?RefactoringOperators?unary_intersecter_operator")

  private var _intersecter: Intersecter = _
  override def getIntersecter(ctrl: Controller): Intersecter = {
    if (_intersecter == null) {
      _intersecter = new UnaryIntersecter
      ctrl.extman.addExtension(_intersecter)
    }
    _intersecter
  }
}

/**
  * Exposes [[BinaryIntersecter]] as a [[NamedOperator]].
  *
  * Usage: `[notation for head symbol] ?intersectionView`.
  */
private object BinaryIntersecterOperator extends IntersecterOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?RefactoringOperators?binary_intersecter_operator")

  private var _intersecter: Intersecter = _
  override def getIntersecter(ctrl: Controller): Intersecter = {
    if (_intersecter == null) {
      _intersecter = new BinaryIntersecter
      ctrl.extman.addExtension(_intersecter)
    }
    _intersecter
  }
}

