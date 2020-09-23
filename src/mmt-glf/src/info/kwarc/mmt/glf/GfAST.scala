package info.kwarc.mmt.glf

import info.kwarc.mmt.api.objects.{OMLIT, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.StandardNat
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.sequences.NatRules.NatLit

import scala.collection.mutable.ListBuffer

sealed abstract class GfAST {
  def toOMDocRec(th : Map[String, Constant]) : Term
}

case class GfFun(fun : String, args : List[GfAST]) extends GfAST {
  override def toString: String =
    fun + '(' + args.map(x => x.toString).mkString(", ") + ')'

  override def toOMDocRec(theorymap : Map[String, Constant]): Term = {
    if (args.isEmpty) {
      getTerm(fun, theorymap)
    } else {
      ApplySpine(getTerm(fun, theorymap), args.map(_.toOMDocRec(theorymap)):_*)
    }
  }

  private def getTerm(s : String, theorymap : Map[String, Constant]) : Term= {
    theorymap.get(s) match {
      case Some(c) => c.toTerm
      case None => throw LangTheoryIncomplete("Constant '" + s + "' not found in language theory")
    }
  }
}

case class GfInt(value: String) extends GfAST {
  override def toString: String = value
  override def toOMDocRec(theorymap : Map[String, Constant]): Term = {
    OMLIT(StandardNat(value.toInt), NatLit)
  }
}

object GfAST {
  def parseAST(str : String) : GfAST = {

    var head_end = 0
    var isint = true
    while (head_end < str.length && str.charAt(head_end) != ' ') {
      if (str.charAt(head_end) < '0' || str.charAt(head_end) > '9') isint = false
      head_end += 1
    }

    val head = str.take(head_end)
    if (isint) {
      return GfInt(head)
    }

    val args = ListBuffer[GfAST]()

    // parse args
    var i = head_end + 1
    while (i < str.length) {
      if (str.charAt(i) == '(') {   // argument in parenthesis
        i += 1
        val start = i
        var bracketcount = 1
        while (bracketcount != 0) {
          if (str.charAt(i) == '(') {
            bracketcount += 1
          } else if (str.charAt(i) == ')') {
            bracketcount -= 1
          }
          i += 1
        }
        args.append(parseAST(str.substring(start, i-1)))
        i += 1
      } else {    // argument not in parenthesis
        val start = i
        while (i < str.length && str.charAt(i) != ' ') {
          i += 1
        }
        args.append(parseAST(str.substring(start, i)))
        i += 1
      }
    }

    GfFun(head, args.toList)
  }
}
