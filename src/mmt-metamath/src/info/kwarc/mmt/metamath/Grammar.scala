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
import scala.util.parsing.combinator.PackratParsers

class Grammar(p: MMParser, syn: String => String, tc: String*) extends PackratParsers {
  type Elem = Sym
  val parsers = new HashMap[Axiom, PackratParser[ParseNode]]
  val axioms = new HashMap[(Constant, Option[Constant]), MutableList[Axiom]]
  val synMap = new HashMap[Constant, Constant]
  val varTypecodes = new HashSet[Constant]
  tc foreach { cstr => varTypecodes += p.Syms(cstr).asInstanceOf[Constant] }

  def key(s: Sym): Option[Constant] = s match {
    case c: Constant => Some(c)
    case _ => None
  }

  def parseAll {
    var i = 0
    p.Statements.list foreach { s =>
      s match {
        case a: Axiom if varTypecodes.contains(a.formula.typecode) => {
          a.frame.dv.foreach(_ => throw new MMError(s"Syntax axiom ${a.label} has a DV condition"))
          a.frame.hyps.foreach {
            case _: Essential => throw new MMError(s"Syntax axiom ${a.label} has a hypothesis")
            case _ =>
          }
          addAxiom(a)
        }
        case _ =>
      }
      i+=1
      println(i + "/" + p.Statements.list.length + " " + s.formula)
      parse(s.label, s.formula)
    }
  }
  
  private def addAxiom(a: Axiom) {
    axioms.getOrElseUpdate((a.formula.typecode, key(a.formula.expr.head)), new MutableList) += a
  }

  def parse(label: String, formula: Formula) {
    val c = formula.typecode
    val result = phrase(parser(synMap.getOrElseUpdate(c,
      if (varTypecodes.contains(c)) c
      else p.Syms(syn(c.id)).asInstanceOf[Constant]
    ))).apply(new SeqReader(formula.expr))
    formula.parse = if (result.successful) result.get else throw new MMError(s"$result - while parsing $label: $formula")
  }

  def parser(typecode: Constant): PackratParser[ParseNode] = {
    def inner(in: Input): ParseResult[ParseNode] = {
      in.first match {
        case v: Variable if v.activeFloat.typecode == typecode => return Success(VarNode(v.activeFloat), in.rest)
        case _ =>
      }
      val tryit = { a: Axiom =>
        val res = parsers.getOrElseUpdate(a, {
          var parserQ = success(Queue.empty[ParseNode])
          a.formula.expr foreach {
            case c: Constant => parserQ = parserQ <~ c
            case v: Variable => parserQ = parserQ ~
              parser(v.activeFloat.typecode) ^^ { case a ~ b => a.enqueue(b) }
          }
          // Assumes no empty syntax axioms
          parserQ.map(q => AxiomNode(a, q.toList))
        })(in)
        if (res.successful) return res
      }
      key(in.first) match {
        case s @ Some(_) => axioms.getOrElseUpdate((typecode, s), new MutableList) foreach tryit
        case None =>
      }
      axioms.getOrElseUpdate((typecode, None), new MutableList) foreach tryit
      Failure("no match", in)
    }
    Parser { inner }
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
  override def toString = seq.toString
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
