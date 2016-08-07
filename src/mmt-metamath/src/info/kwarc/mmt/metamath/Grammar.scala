package info.kwarc.mmt.metamath

import scala.collection.mutable.HashMap
import scala.util.parsing.combinator.Parsers
import scala.collection.mutable.HashSet
import scala.collection.mutable.MutableList
import scala.collection.immutable.Queue
import scala.util.parsing.input.Reader
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Position

class Grammar(p: MMParser, syn: String => String, tc: String*) extends Parsers {
  type Elem = Sym
  val parsers = new HashMap[Constant, Parser[ParseNode]]
  val synMap = new HashMap[Constant, Constant]
  val varTypecodes = new HashSet[Constant]
  varTypecodes ++= tc.map { cstr => p.Syms(cstr).asInstanceOf[Constant] }
  val axiomByTC = new HashMap[Constant, MutableList[(Axiom, List[Sym])]]
  p.Statements.list foreach {
    case a: Axiom if varTypecodes.contains(a.formula.typecode) => {
      a.frame.dv.foreach(_ => throw new MMError(s"Syntax axiom ${a.label} has a DV condition"))
      a.frame.hyps.foreach {
        case _: Essential => throw new MMError(s"Syntax axiom ${a.label} has a hypothesis")
        case _ =>
      }
      axiomByTC.getOrElseUpdate(a.formula.typecode, new MutableList) += ((a, a.formula.expr))
    }
    case _ =>
  }
  varTypecodes foreach { c =>
    var varP = acceptMatch(c.id+" var", { case vr: Variable
      if vr.activeFloat.typecode == c => VarNode(vr.activeFloat) })
    parsers(c) = axiomByTC.get(c) match {
      case None => varP
      case Some(l) => varP | buildParser(c.id, l)
    }
  }


  def buildParser(label: String, axioms: Seq[(Axiom, List[Sym])]): Parser[AxiomNode] = {
    val out = new MutableList[Parser[AxiomNode]]
    val constants = new HashMap[Constant, MutableList[(Axiom, List[Sym])]]
    val variables = new HashMap[Constant, MutableList[(Axiom, List[Sym])]]
    axioms foreach { case (a, l) =>
      l.headOption match {
        case None => out += success(new AxiomNode(a, Nil))
        case Some(s) => s match {
          case c: Constant => constants.getOrElseUpdate(c, new MutableList) += ((a, l.tail))
          case v: Variable => variables.getOrElseUpdate(v.activeFloat.typecode, new MutableList) += ((a, l.tail))
        }
      }
    }
    if (!constants.isEmpty) {
      val constantsLookup = new HashMap[Sym, Parser[AxiomNode]]
      constants foreach { case (c, l) => constantsLookup.put(c, buildParser(label + " " + c.id, l)) }
      out += Parser { in =>
        constantsLookup.get(in.first) match {
          case Some(p) => p(in.rest)
          case None => Failure("No match", in)
        }
      }
    }    
    variables foreach { case (c, l) =>
      val sub = buildParser(s"$label <${c.id}>", l)
      out += parsers(c) ~ sub ^^ { case v ~ node => new AxiomNode(node.a, v :: node.child) }
    }
    out.tail.foldRight(out.head)((a,b) => a | b)
  }
  
  def asConst(s: Sym): Option[Constant] = s match {
    case c: Constant => Some(c)
    case _ => None
  }

  def parseAll {
    p.Statements.list foreach { s => parse(s.label, s.formula) }
  }

  def parse(label: String, formula: Formula) {
    val c = formula.typecode
    val result = phrase(parsers.get(synMap.getOrElseUpdate(c,
      if (varTypecodes.contains(c)) c
      else p.Syms(syn(c.id)).asInstanceOf[Constant]
    )).get).apply(new SeqReader(formula.expr))
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

class ParseNode(s: Statement, child: List[ParseNode]) {
  override def toString = if (child.isEmpty) s.label else {
    var str = "(" + s.label;
    child foreach { c => str += " " + c }
    str + ")"
  }
}
case class VarNode(f: Floating) extends ParseNode(f, Nil)
case class AxiomNode(a: Axiom, child: List[ParseNode]) extends ParseNode(a, child)
