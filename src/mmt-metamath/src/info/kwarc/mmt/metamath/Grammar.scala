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
  val parsersFirst = new HashMap[(Constant, Option[Constant]), MutableList[Parser[ParseNode]]]
  val synMap = new HashMap[Constant, Constant]
  val varTypecodes = new HashSet[Constant]
  tc foreach { cstr =>
    var c = p.Syms(cstr).asInstanceOf[Constant]
    varTypecodes += c
    var varP = acceptMatch(c.id+" var", { case vr: Variable
      if vr.activeFloat.typecode == c => VarNode(vr.activeFloat) })
    def inner(in: Input): ParseResult[ParseNode] = {
      val tryit = { h: Option[Constant] =>
        parsersFirst.getOrElseUpdate((c, h), new MutableList) foreach { p =>
          val res = p(in)
          if (res.successful) return res
        }
      }
      val k = asConst(in.first)
      if (k.isDefined) tryit(k)
      tryit(None)
      Failure("no match", in)
    }
    parsers(c) = varP | Parser { inner }.named("lookup")  
  }
    
  p.Statements.list foreach {
    case a: Axiom if varTypecodes.contains(a.formula.typecode) => {
      a.frame.dv.foreach(_ => throw new MMError(s"Syntax axiom ${a.label} has a DV condition"))
      a.frame.hyps.foreach {
        case _: Essential => throw new MMError(s"Syntax axiom ${a.label} has a hypothesis")
        case _ =>
      }
      def parserQ(i: Int): List[Sym] => Parser[List[ParseNode]] = {
        case Nil => success(Nil).named("${a.label}.$i")
        case s :: l => (s match {
          case c: Constant => c ~> parserQ(i+1)(l)
          case v: Variable => parsers.get(v.activeFloat.typecode).get ~ parserQ(i+1)(l) ^^ { case a ~ b => a :: b }
        }).named(s"${a.label}.$i")
      }
      // Assumes no empty syntax axioms
      parsersFirst.getOrElseUpdate((a.formula.typecode, asConst(a.formula.expr.head)), new MutableList) +=
        parserQ(0)(a.formula.expr).map(AxiomNode(a, _)).named(a.label)
    }
    case _ =>
  }

  def asConst(s: Sym): Option[Constant] = s match {
    case c: Constant => Some(c)
    case _ => None
  }

  def parseAll {
    var i = 0
    p.Statements.list foreach { s =>
      i+=1
      println(i + "/" + p.Statements.list.length + " " + s.formula)
      
      if (s.formula.toString == "|- ( { x | ph } = A <-> A. x ( ph <-> x e. A ) )") {
        i
      }
      parse(s.label, s.formula)
    }
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
