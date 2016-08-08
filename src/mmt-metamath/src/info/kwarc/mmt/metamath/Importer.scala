package info.kwarc.mmt.metamath

import info.kwarc.mmt.api._
import documents._
import archives._
import info.kwarc.mmt.api.parser.Reader
import info.kwarc.mmt.api.utils.{ File, Unparsed }
import scala.collection.mutable.{ HashMap, ListBuffer, Stack, MutableList, HashSet }
import scala.util.parsing.combinator.Parsers
import info.kwarc.mmt.api.objects._
import Metamath._
import info.kwarc.mmt.api.frontend.Controller

class Importer extends archives.Importer {
  val key = "mm-omdoc"
  def inExts = List("mm")
  override def inDim = RedirectableDimension("mm")

  def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
    log("Reading " + bf.inFile)
    val parser = new MMParser(bf.inFile)
    val e = try {
      log("File parsing...")
      val db = parser.parse
      log("Math parsing...")
      new Grammar(db, { case "|-" => "wff" }, "wff", "set", "class").parseAll
      log("Add to library...")
      val conv = new Translator(controller, bf, index)
      conv.addDatabase(db)
    } catch {
      case MMError(s) => println(s)
    }

    BuildResult.empty
  }

}

case class MMError(s: String) extends java.lang.Exception(s)

class MMDatabase(val mod: MPath) {
  object Syms extends HashMap[String, Sym] {
    def +=(s: Sym): Unit = {
      s.seq = size
      put(s.id, s)
    }
    override def apply(s: String): Sym = super.get(s).getOrElse(throw new MMError(s"Symbol $s not found"))
  }

  object Statements extends HashMap[LocalName, Statement] {
    val list = new MutableList[Statement]
    def +=(s: Statement): Unit = {
      s.seq = size
      if (put(s.label.name, s).isDefined) throw new MMError(s"Redefining statement $s")
      list += s
    }
    override def apply(s: LocalName): Statement = super.get(s).getOrElse(throw new MMError(s"Statement $s not found"))
  }
}

case class MMParser(f: File) {
  private val frames = Stack(new Frame())

  val up = new Unparsed(File.read(f), msg => throw MMError(msg))

  def parse: MMDatabase = {
    var db = new MMDatabase(Metamath._base ? f.name)
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
          readUntil("$.").foreach(s => db.Syms += Constant(s))
        case "$v" =>
          up.drop("$v")
          readUntil("$.").foreach(s => db.Syms += Variable(s))
        case "$d" =>
          up.drop("$d")
          up.trim
          val vars = readUntil("$.").map(s => db.Syms(s).asInstanceOf[Variable])
          frames push Disjointness(vars: _*) :: frames.pop
        case "${" =>
          up.drop("${")
          frames push frames.head
        case "$}" =>
          up.drop("$}")
          frames.pop
        case _ =>
          val label = db.mod ? readUntil(" ").headOption.getOrElse(return db)
          c = up.trim.getnext(2).toString
          c match {
            case "$f" =>
              up.drop("$f")
              readUntil("$.") match {
                case List(cs, vs) => {
                  val v = db.Syms(vs).asInstanceOf[Variable]
                  val stmt = Floating(label, db.Syms(cs).asInstanceOf[Constant], v)
                  v.activeFloat = stmt
                  db.Statements += stmt
                  frames push stmt :: frames.pop
                }
                case _ => throw new MMError("Incorrect $f statement")
              }
            case "$e" =>
              up.drop("$e")
              up.trim
              val stmt = Essential(label, readFormula("$."))
              db.Statements += stmt
              frames push stmt :: frames.pop
            case "$a" =>
              up.drop("$a")
              up.trim
              val formula = readFormula("$.")
              db.Statements += Assert(label, formula, Assert.trimFrame(formula, frames.head), None)
            case "$p" =>
              up.drop("$p")
              up.trim
              val formula = readFormula("$=")
              db.Statements += Assert(label, formula, Assert.trimFrame(formula, frames.head), Some(readUntil("$.")))
            case _ => throw new MMError("Not a valid metamath command: " + c + " in " + up.remainder.subSequence(0, 200))
          }
      }
    }
    
    def readFormula(s: String) = {
      val ls = readUntil(s).map(db.Syms)
      Formula(ls.head.asInstanceOf[Constant], ls.tail)
    }
    db
  }

  private def readUntil(s: String) = {
    var active = true
    var out = new MutableList[String]
    for (
      tok <- (if (s.length == 1) up.next(s.head, Reader.FS.toChar)._1 // FS, because there HAS to be a char
      else {
        var ret = new StringBuilder
        while (!up.remainder.startsWith(s)) {
          ret += up.next
        }
        up.drop(s)
        ret.toString
      }).split("\\s+")
    ) {
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
}

class Sym(val id: String) {
  def str = id
  var seq: Int = _
  override def toString = id
}
case class Constant(override val id: String) extends Sym(id)
case class Variable(override val id: String) extends Sym(id) {
  var activeFloat: Floating = _
}

case class Formula(typecode: Constant, expr: List[Sym]) {
  var parse: Term = _
  override def toString = {
    var s = typecode.id
    expr foreach { e => s += " " + e.id }
    s
  }
}

case class Disjointness(v: Variable*)

class Statement(val label: GlobalName, val formula: Formula) {
  var seq: Int = _
}

class Hypothesis(label: GlobalName, formula: Formula) extends Statement(label, formula)
case class Floating(override val label: GlobalName, typecode: Constant, v: Variable)
  extends Hypothesis(label, Formula(typecode, List(v)))
case class Essential(override val label: GlobalName, override val formula: Formula)
  extends Hypothesis(label, formula)

class Frame(val dv: List[Disjointness], val hyps: List[Hypothesis]) {
  def this() = this(Nil, Nil)
  def ::(d: Disjointness) = new Frame(d :: dv, hyps)
  def ::(h: Hypothesis) = new Frame(dv, h :: hyps)

  def disjoint = new Traversable[(Variable, Variable)] {
    def foreach[U](f: ((Variable, Variable)) => U) {
      dv.foreach(_ match {
        case Disjointness(v @ _*) => v.foreach(a => v.foreach(b => if (a != b) f(a, b)))
      })
    }
  }
}

case class Assert(override val label: GlobalName, override val formula: Formula, val frame: Frame,
  val proof: Option[List[String]]) extends Statement(label, formula)
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
    } reverse, eframe.hyps filter {
      case Floating(_, _, v) => usedVars.contains(v)
      case _ => true
    } reverse)
  }
}
