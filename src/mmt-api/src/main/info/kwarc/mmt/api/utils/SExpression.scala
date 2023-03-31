package info.kwarc.mmt.api.utils
import info.kwarc.mmt.api._

/** straightforward implementation of S Expressions */
abstract class SExpression {
  def asList = this match {
    case SExpressionList(es) => es
    case _ => List(this)
  }
}

/** (e1 ... en) */
case class SExpressionList(value: List[SExpression]) extends SExpression {
  override def toString = "(" + value.mkString(" ") + ")"
}
/** (e1 . en) */
case class SExpressionPair(left: SExpression, right: SExpression) extends SExpression {
  override def toString = s"($left . $right)"
}
/** n or |n| */
case class SExpressionId(name: String) extends SExpression {
  override def toString = if (name.forall(SExpression.isIDChar)) name else  "|" + name + "|"
  /** (this args) */
  def apply(args: SExpression*) = SExpressionList(this::args.toList)
  /** matches (this args) */
  def unapplySeq(s: SExpression): Option[Seq[SExpression]] = s match {
    case SExpressionList(es) if es.headOption contains this => Some(es.tail)
    case _ => None
  }
}
/** "s" */
case class SExpressionString(value: String) extends SExpression {
  override def toString = {
    val valueE = JSON.quoteString(value)
    "\"" + valueE + "\""
  }
}
/** digits */
case class SExpressionNumber(value: Int) extends SExpression {
  override def toString = "\"" + value + "\""
}

object SExpression {
  private val idSyms = "!$%&*+-./:<=>?@^_~"
  def isIDChar(c: Char) = c.isUpper || c.isLower || (idSyms contains c)
  /** () */
  def NIL = SExpressionList(Nil)
  /** parses an S-expression */
  def parse(s: String, exactlyOne: Boolean): SExpression = {
    val u = new Unparsed(s,s => throw ParseError(s))

    /** reads one S-Expression off u */
    def parseAny: SExpression = {
      u.trim.head match {
        case '(' =>
          // (e1 ... en) or (e1 . e2)
          val p = u.getSourcePosition
          u.drop("(")
          var found: List[SExpression] = Nil
          while (!u.trim.empty && u.head != ')') {
            found ::= parseAny
          }
          if (u.empty) throw
            ParseError("unclosed ( at position " + p)
          u.drop(")")
          if (found.length == 3 && found(1) == SExpressionId(".")) {
            // special case for (a . b), a single . anywhere is a syntax error but we simply treat it as a normal id
            SExpressionPair(found.last,found.head)
          } else {
            SExpressionList(found.reverse)
          }
        case '|' =>
          u.drop("|")
          val n = u.takeUntilString("|",Nil)
          SExpressionId(n)
        case c if isIDChar(c) || (c == '\\' && u.left > 1) =>
          // idchar-only identifiers do not need to be |-wrapped
          // in that case lower-case is allowed but counts as uppercase, \c is c
          var chars : String = ""
          def readMore {
            if (u.empty) return
            val h = u.head
            if (isIDChar(h)) {
              u.drop(h)
              chars = chars ++ h.toUpper.toString
              readMore
            } else if (h == '\\' && u.left > 1) {
              u.drop(h)
              chars = chars ++ u.next().toString
              readMore
            }
          }
          readMore
          SExpressionId(chars)
        case c if c.isDigit =>
          // TODO - and decimals
          val ds = u.takeWhile(_.isDigit)
          SExpressionNumber(ds.toInt)
        case '"' =>
          val s = JSON.takeQuotedString(u, m => throw ParseError(m + " at position " + u.getSourcePosition))
          SExpressionString(s)
        case c =>
          throw ParseError("unexpected character at position " + u.getSourcePosition + ": " + c)
      }
    }

    var es: List[SExpression] = Nil
    while (!u.trim.empty) {
      es ::= parseAny
    }
    if (es.length == 1) es.head else {
      if (exactlyOne) {
        if (es.isEmpty) throw ParseError("no expression found")
        else throw ParseError("more than one expression found")
      } else {
        SExpressionList(es.reverse)
      }
    }
  }
}


