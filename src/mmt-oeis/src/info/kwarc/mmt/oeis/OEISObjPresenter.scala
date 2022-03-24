package info.kwarc.mmt.oeis
import info.kwarc.mmt.api._
import presentation._
import notations._
import info.kwarc.mmt.stex.InformalMathMLPresenter

class OEISObjectPresenter extends InformalMathMLPresenter {
  override def doOptionallyBracketedGroup(body: => Unit)(implicit pc: PresentationContext) {
      wrapBrackets(None, body)
  }
  override def getNotations(p: GlobalName): List[TextNotation] = {
    super.getNotations(p) match {
      case hd :: tl => hd :: tl
      case Nil if (p.module.last == "arithmetics") => List(Arithmetics.getNotation(p.last))
      case Nil => Nil
    }
  }
}

