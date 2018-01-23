package info.kwarc.mmt.tptp

import info.kwarc.mmt.api._
import archives._
import parser._
import documents._
import symbols._
import objects._
import checking._

import leo.modules.parsers._
import leo.modules.parsers.syntactical._
import leo.datastructures.tptp._
import Commons._

import scala.util._

class TPTPObjectParser extends ObjectParser {
  def isApplicable(s: String) = s == "tptp"

  def apply(pu: ParsingUnit)(implicit errorCont: ErrorHandler) = {
     val formula = TPTPParsers.fofFormula | TPTPParsers.tffFormula | TPTPParsers.thfFormula
     val form = TPTPParsers.parse(pu.term, formula) match {
       case TPTPParsers.Success(x,_) => x
       case noSu: TPTPParsers.NoSuccess => throw LocalError(noSu.msg)
     }
     val term = form match {
       case f: fof.Formula => translateFOF(f)
       case f: tff.Formula => translateTFF(f)
       case f: thf.Formula => translateTHF(f)
     }
     ???
  }

  def translateFOF(f: fof.Formula): objects.Term = {
    ???
  }
  def translateTFF(f: tff.Formula): objects.Term = {
    ???
  }
  def translateTHF(f: thf.Formula): objects.Term = {
    ???
  }
}

class TPTPParser extends Parser(new TPTPObjectParser) {
  val format = "tptp"

  def apply(ps: ParsingStream)(implicit cont: StructureParserContinuations) = {
    val decls= TPTP.parseFile(ps.fullString) match {
      case Left(s) => throw LocalError(s)
      case Right(t) => t
    }
    val declsT = decls.inputs.map {
      case Left(d) => translateDecl(d)
      case Right(i) => translateIncl(i)
    }
    ???
  }

  def translateDecl(d: AnnotatedFormula): Constant = {
    ???
  }

  def translateIncl(d: Include): Declaration = {
    ???
  }

}


class TPTPInterpreter extends OneStepInterpreter(new TPTPParser) {
  override def inExts = super.inExts ::: List("ax", "p")
}
