package info.kwarc.mmt.oeis
import info.kwarc.mmt.api._
import presentation._
import notations._
import info.kwarc.mmt.planetary._
class OEISObjectPresenter extends InformalMathMLPresenter {
  override def doOptionallyBracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      wrapBrackets(None, body)
  }
  override def getNotation(p: GlobalName): Option[TextNotation] = {
    super.getNotation(p) match {
      case Some(not) => Some(not)
      case None if (p.module.toMPath.last == "arithmetics") => Some(Arithmetics.getNotation(p.last))
      case None => None
    }
  }
}

