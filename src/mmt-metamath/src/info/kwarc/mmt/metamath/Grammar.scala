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
  val parsersFirst = new HashMap[Option[Constant], MutableList[Parser[ParseNode]]]
  val synMap = new HashMap[Constant, Constant]
  val varTypecodes = new HashSet[Constant]
  tc foreach { cstr =>
    var c = p.Syms(cstr).asInstanceOf[Constant]
    varTypecodes += c
    var varP = acceptMatch(c.id+" var", { case vr: Variable
      if vr.activeFloat.get.typecode == c => ParseNode(vr.activeFloat.get, Nil) })
    parsers(c) = varP | Parser { in =>
      parsersFirst.getOrElseUpdate(asConst(in.first), new MutableList) collectFirst
        { p => p(in) match { case i if i.successful => i } } getOrElse Failure("No match", in)
    }.named("lookup")
  }
  
  def asConst(s: Sym): Option[Constant] = s match {
    case c: Constant => Some(c)
    case _ => None
  }
  
  def parseAll {
    p.Statements.list foreach { s =>
      s match {
        case a: Axiom if varTypecodes.contains(a.formula.typecode) => {
          a.frame.dv.foreach(_ => throw new MMError(s"Syntax axiom ${a.label} has a DV condition"))
          a.frame.hyps.foreach {
            case _: Essential => throw new MMError(s"Syntax axiom ${a.label} has a hypothesis")
            case _ =>
          }
          var parserQ = success(Queue.empty[ParseNode])
          a.formula.expr foreach {
            case c: Constant => parserQ = parserQ <~ c
            case v: Variable => parserQ = parserQ ~
              parsers.get(v.activeFloat.get.typecode).get ^^ { case a ~ b => a.enqueue(b) }
          }
          // Assumes no empty syntax axioms
          addParser(a.formula.expr.head, parserQ.map(q => ParseNode(a, q.toList)), a.label)
        }
        case _ =>
      }
      try {
        parse(s.label, s.formula)
      } catch { case e: MMError =>
        parse(s.label, s.formula)
      }
    }
  }
  
  private def addParser(head: Sym, parser: Parser[ParseNode], name: String) {
    parsersFirst.getOrElseUpdate(asConst(head), new MutableList) += parser.named(name)
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
}

case class ParseNode(s: Statement, child: List[ParseNode]) {
  override def toString = if (child.isEmpty) s.label else {
    var str = "(" + s.label;
    child foreach { c => str += " " + c }
    str + ")"
  }
}
