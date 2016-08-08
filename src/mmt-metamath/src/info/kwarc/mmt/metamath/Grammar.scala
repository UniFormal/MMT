package info.kwarc.mmt.metamath

import scala.collection.mutable.HashMap
import scala.util.parsing.combinator.Parsers
import scala.collection.mutable.HashSet
import scala.collection.mutable.MutableList
import scala.collection.immutable.Queue
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.GlobalName
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position


class Grammar(db: MMDatabase) extends Parsers {
  type Elem = Sym
  val parsers = new HashMap[Constant, Parser[Term]]
  val axiomByTC = new HashMap[Constant, MutableList[(Assert, List[Sym])]]
  db.statements.list foreach {
    case a: Assert if db.vartyps.contains(a.formula.typecode) => {
      a.frame.dv.foreach(_ => throw new MMError(s"Syntax axiom ${a.label} has a DV condition"))
      a.frame.hyps.foreach {
        case _: Essential => throw new MMError(s"Syntax axiom ${a.label} has a hypothesis")
        case _ =>
      }
      axiomByTC.getOrElseUpdate(a.formula.typecode, new MutableList) += ((a, a.formula.expr))
    }
    case _ =>
  }
  db.vartyps foreach { c =>
    var varP = acceptMatch(c.id+" var", { case vr: Variable
      if vr.activeFloat.typecode == c => OMV(vr.id) })
    parsers(c) = axiomByTC.get(c) match {
      case None => varP
      case Some(l) => varP | buildParser(c.id, l)
    }
  }


  def buildParser(name: String, axioms: Seq[(Assert, List[Sym])]): Parser[Term] = {
    val out = new MutableList[Parser[Term]]
    val constants = new HashMap[Constant, MutableList[(Assert, List[Sym])]]
    val variables = new HashMap[Constant, MutableList[(Assert, List[Sym])]]
    axioms foreach { case (a, l) =>
      l.headOption match {
        case None => out += success(OMS(a.label))
        case Some(s) => (s match {
          case c: Constant => constants.getOrElseUpdate(c, new MutableList)
          case v: Variable => variables.getOrElseUpdate(v.activeFloat.typecode, new MutableList)
        }) += ((a, l.tail))
      }
    }
    if (!constants.isEmpty) {
      val constantsLookup = new HashMap[Sym, Parser[Term]]
      constants foreach { case (c, l) => constantsLookup.put(c, buildParser(name + " " + c.id, l)) }
      out += Parser { in =>
        constantsLookup.get(in.first) match {
          case Some(p) => p(in.rest)
          case None => Failure("No match", in)
        }
      }
    }    
    variables foreach { case (c, l) =>
      val sub = buildParser(s"$name <${c.id}>", l)
      out += parsers(c) ~ sub ^^ { case v ~ OMAorOMID(t, args) => OMA(t, v :: args) }
    }
    out.tail.foldRight(out.head)((a,b) => a | b).named(name)
  }
  
  def asConst(s: Sym): Option[Constant] = s match {
    case c: Constant => Some(c)
    case _ => None
  }

  def parseAll {
    db.statements.list foreach { s => parse(s.label, s.formula) }
  }

  def parse(label: GlobalName, formula: Formula) {
    val c = formula.typecode
    val result = phrase(parsers.get(db.typecodes(c)).get).apply(new SeqReader(formula.expr))
    formula.parse = if (result.successful) result.get else throw new MMError(s"$result - while parsing $label: $formula")
  }
}

class SeqReader[T](seq: Seq[T], offset: Int = 1) extends Reader[T] {
  def atEnd = seq.isEmpty
  def first = seq.head
  def pos = new Position {
    def line = 1
    def column = offset
    override def toString = column.toString
    override def longString = toString
    def lineContents = ""
  }
  def rest = new SeqReader(seq.tail, offset + 1)
  override def toString = {
    var delim = ""
    var str = ""
    seq foreach { s =>
      str += delim + s
      delim = " "
    }
    str
  }
}
