package info.kwarc.mmt.metamath

import info.kwarc.mmt.api._
import documents._
import archives._
import info.kwarc.mmt.api.parser.Reader
import info.kwarc.mmt.api.utils.{ File, Unparsed }
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList
import scala.collection.mutable.HashSet
import scala.util.parsing.combinator.Parsers

class Importer extends archives.Importer {
  val key = "mm-omdoc"
  def inExts = List("mm")
  override def inDim = RedirectableDimension("mm")

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
    log("Reading " + bf.inFile)
    val parser = new MMParser(bf.inFile)
    val e =  try {
      log("File parsing...")
      parser.parse
      log("Math parsing...")
      new Grammar(parser, {case "|-" => "wff"}, "wff", "set", "class").parseAll
    } catch {
      case MMError(s) => println(s)
      case e: Exception => e.printStackTrace()
    }
    //println(e)

    //val conv = new PVSImportTask(controller, bf, index)

    BuildResult.empty
  }

}

case class MMError(s: String) extends java.lang.Exception(s)

case class MMParser(f: File) {
  object Syms extends HashMap[String, Sym] {
    def +=(s: Sym): Unit = {
      s.seq = size
      put(s.id, s)
    }
    override def apply(s: String): Sym = super.get(s).getOrElse(throw new MMError(s"Symbol $s not found"))
  }

  object Statements extends HashMap[String, Statement] {
    val list = new MutableList[Statement]
    def +=(s: Statement): Unit = {
      s.seq = size
      if (put(s.label, s).isDefined) throw new MMError(s"Redefining statement $s")
      list += s
    }
    override def apply(s: String): Statement = super.get(s).getOrElse(throw new MMError(s"Statement $s not found"))
  }

  private val frames = Stack(new Frame())

  val up = new Unparsed(File.read(f), msg => throw MMError(msg))

  def parse {
    while (!up.empty) {
      var c = try {
        up.trim.getnext(2).toString
      } catch {
        case e: StringIndexOutOfBoundsException => up.remainder.toString
      }
      c match {
        case "$(" =>
          up.drop("$(")
          skipUntil("$)")
        case "$c" =>
          up.drop("$c")
          readUntil("$.").foreach(s => Syms += Constant(s))
        case "$v" =>
          up.drop("$v")
          readUntil("$.").foreach(s => Syms += Variable(s))
        case "$d" =>
          up.drop("$d")
          up.trim
          val vars = readUntil("$.").map(s => Syms(s).asInstanceOf[Variable])
          frames push Disjointness(vars: _*) :: frames.pop
        case "${" =>
          up.drop("${")
          frames push frames.head
        case "$}" =>
          up.drop("$}")
          frames.pop
        case _ =>
          val label = readUntil(" ").headOption.getOrElse(return)
          c = up.trim.getnext(2).toString
          c match {
            case "$f" =>
              up.drop("$f")
              val ls = readUntil("$.")
              if (ls.length != 2) throw new MMError("Incorrect $f statement")
              val const = Syms(ls(0)).asInstanceOf[Constant]
              val v = Syms(ls(1)).asInstanceOf[Variable]
              val stmt = Floating(label, const, v)
              Statements += stmt
              frames push stmt :: frames.pop
              v.activeFloat = Some(stmt)
            case "$e" =>
              up.drop("$e")
              up.trim
              val stmt = Essential(label, readFormula("$."))
              Statements += stmt
              frames push stmt :: frames.pop
            case "$a" =>
              up.drop("$a")
              up.trim
              val formula = readFormula("$.")
              Statements += Axiom(label, formula, Assert.trimFrame(formula, frames.head))
            case "$p" =>
              up.drop("$p")
              up.trim
              val formula = readFormula("$=")
              Statements += Provable(label, formula, Assert.trimFrame(formula, frames.head), readUntil("$."))
            case _ => throw new MMError("Not a valid metamath command: " + c + " in " + up.remainder.subSequence(0, 200))
          }
      }
    }
  }
  
  private def readUntil(s: String) = {
    var active = true
    var out = new MutableList[String]
    for (tok <- (if (s.length == 1) up.next(s.head, Reader.FS.toChar)._1 // FS, because there HAS to be a char
        else {
          var ret = new StringBuilder
          while (!up.remainder.startsWith(s)) {
            ret += up.next
          }
          up.drop(s)
          ret.toString
        }).split("\\s+")) {
      tok match {
        case "$(" => active = false
        case "$)" => active = true
        case t if active && !t.isEmpty => out += t
        case _ =>
      }
    }
    out.toList
  }

  private def skipUntil(s: String): Unit = if (s.length == 1) up.next(s.head, Reader.FS.toChar)._1 // FS, because there HAS to be a char
  else {
    while (!up.remainder.startsWith(s)) {
      up.next
    }
    up.drop(s)
  }

  private def readFormula(s: String) = {
    val ls = readUntil(s).map(Syms)
    Formula(ls.head.asInstanceOf[Constant], ls.tail)
  }
  
  def parseGrammar {
    // TODO
  }
}

class Sym(val id: String) {
  def str = id
  var seq: Int = _
}
case class Constant(override val id: String) extends Sym(id)
case class Variable(override val id: String) extends Sym(id) {
  var activeFloat: Option[Floating] = None
}

case class Formula(typecode: Constant, expr: List[Sym]) {
  var parse: ParseNode = _
  override def toString = {
    var s = typecode.id
    expr foreach { e => s += " " + e.id }
    s
  }
}

case class Disjointness(v: Variable*)

class Statement(val label: String, val formula: Formula) {
  var seq: Int = _
}

class Hypothesis(label: String, formula: Formula) extends Statement(label, formula)
case class Floating(override val label: String, typecode: Constant, v: Variable) extends Hypothesis(label, Formula(typecode, List(v)))
case class Essential(override val label: String, override val formula: Formula) extends Hypothesis(label, formula)

class Frame(val dv: List[Disjointness], val hyps: List[Hypothesis]) {
  def this() = this(Nil, Nil)
  def ::(d: Disjointness) = new Frame(d :: dv, hyps)
  def ::(h: Hypothesis) = new Frame(dv, h :: hyps)

  def disjoint = new Traversable[(Variable, Variable)] {
    def foreach[U](f: ((Variable, Variable)) => U) {
      dv.foreach(_ match {
        case Disjointness(v @ _*) => v.foreach(a => v.foreach(b => if (a != b) f(a,b)))
      })
    }
  }
}

class Assert(label: String, formula: Formula, val frame: Frame) extends Statement(label, formula)
object Assert {
  def trimFrame(formula: Formula, eframe: Frame) = {
    val usedVars = new HashSet[Variable]
    (formula :: eframe.hyps collect {
      case e: Essential => e.formula
    }).foreach(_.expr foreach {
      case v: Variable => usedVars += v
      case _ =>
    })
    new Frame(eframe.dv map {
      case d: Disjointness => d.v filter usedVars.contains
    } collect {
      case seq if seq.length > 1 => Disjointness(seq: _*)
    }, eframe.hyps filter {
      case Floating(_,_,v) => usedVars.contains(v)
      case _ => true
    })
  }
}

case class Axiom(override val label: String, override val formula: Formula, override val frame: Frame) extends Assert(label, formula, frame)
case class Provable(override val label: String, override val formula: Formula, override val frame: Frame, proof: List[String]) extends Assert(label, formula, frame)
