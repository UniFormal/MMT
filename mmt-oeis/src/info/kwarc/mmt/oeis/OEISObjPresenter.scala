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
        case None if p.last == "power" => Some(TextNotation.parse("1 ^ 2 prec 100", NamespaceMap.empty))
        case None if p.last == "sqrt" => Some(TextNotation.parse("√ 1 prec 1000", NamespaceMap.empty))
        case None if p.last == "plus" => Some(TextNotation.parse("1+… prec 10", NamespaceMap.empty))
        case None if p.last == "minus" => Some(TextNotation.parse("1-… prec 10", NamespaceMap.empty))
        case None if p.last == "unary_minus" => Some(TextNotation.parse("- 1 prec 999", NamespaceMap.empty))
        case None if p.last == "divide" => Some(TextNotation.parse("1 / 2 prec 2", NamespaceMap.empty))
        case None if p.last == "equal" => Some(TextNotation.parse("1 = 2 prec 0", NamespaceMap.empty))
        case None if p.last == "=" => Some(TextNotation.parse("1 = 2 prec 0", NamespaceMap.empty))
        case None if p.last == ">" => Some(TextNotation.parse("1 > 2 prec 0", NamespaceMap.empty))
        case None if p.last == "<" => Some(TextNotation.parse("1 < 2 prec 0", NamespaceMap.empty))
        case None if p.last == ">=" => Some(TextNotation.parse("1 >= 2 prec 0", NamespaceMap.empty))
        case None if p.last == "<=" => Some(TextNotation.parse("1 <= 2 prec 0", NamespaceMap.empty))
        case None if p.last == "divisible" => Some(TextNotation.parse("1 | 2 prec 100", NamespaceMap.empty))
        case None if p.last == "times" => Some(TextNotation.parse("1×… prec 2", NamespaceMap.empty))
        case None if p.last == "interval" => Some(TextNotation.parse("[1,2] prec 2", NamespaceMap.empty))
        case None if p.last == "factorial" => Some(TextNotation.parse("1 ! prec 1002", NamespaceMap.empty))
         case None if p.last == "set" => Some(TextNotation.parse("⟮ 1,… ⟯  prec 2", NamespaceMap.empty))
         case _ => Some(TextNotation.parse(p.last + "⟮ 1,… ⟯", NamespaceMap.empty))
      }
   }
}

