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
import scala.util.parsing.combinator.RegexParsers

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
      new Grammar(db).parseAll
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
  var grammatical = false
  var unambiguous = false

  object syms extends HashMap[String, Sym] {
    def +=(s: Sym): Unit = {
      s.seq = size
      put(s.id, s)
    }
    override def apply(s: String): Sym = super.get(s).getOrElse(throw MMError(s"Symbol $s not found"))
  }

  val vartyps = new HashSet[Constant]

  object typecodes extends HashMap[Constant, Constant] {
    def put(key: String, value: String) {
      val ckey = syms(key).asInstanceOf[Constant]
      val cval = syms(value).asInstanceOf[Constant]
      if (key == value)
        vartyps += cval
      else if (!vartyps.contains(cval))
        throw MMError(s"$value must be a previously declared variable typecode")
      if (super.put(ckey, cval).isDefined)
        throw new MMError(s"Redefining typecode $key")
      ckey.tc = true
    }
  }

  object statements extends HashMap[LocalName, Statement] {
    val list = new MutableList[Statement]
    def +=(s: Statement): Unit = {
      s.seq = size
      if (put(s.label.name, s).isDefined) throw new MMError(s"Redefining statement $s")
      list += s
    }
    override def apply(s: LocalName): Statement = super.get(s).getOrElse(throw MMError(s"Statement $s not found"))
  }

  val htmldefs = new HashMap[String, String]
  val althtmldefs = new HashMap[String, String]
  val latexdefs = new HashMap[String, String]
}

case class MMParser(f: File) {
  private val frames = Stack(new Frame)

  val up = new Unparsed(File.read(f), msg => throw MMError(msg))

  def parse: MMDatabase = {
    var db = new MMDatabase(Metamath._base ? f.name)
    var lastComment: Option[String] = None
    val cparser = new CommentParser(db)
    while (!up.empty) {
      var c = try {
        up.trim.getnext(2).toString
      } catch {
        case e: StringIndexOutOfBoundsException => up.remainder.toString
      }
      c match {
        case "$(" =>
          up.drop("$(")
          val newComment = readUntilRaw("$)")
          lastComment = if (cparser parseComment newComment) None else Some(newComment)
        case "$c" =>
          up.drop("$c")
          readUntil("$.").foreach(s => db.syms += Constant(s))
          lastComment = None
        case "$v" =>
          up.drop("$v")
          readUntil("$.").foreach(s => db.syms += Variable(s))
          lastComment = None
        case "$d" =>
          up.drop("$d")
          up.trim
          val vars = readUntil("$.").map(s => db.syms(s).asInstanceOf[Variable])
          frames push Disjointness(vars: _*) :: frames.pop
          lastComment = None
        case "${" =>
          up.drop("${")
          frames push frames.head
          lastComment = None
        case "$}" =>
          up.drop("$}")
          frames.pop
          lastComment = None
        case _ =>
          val label = db.mod ? readUntil(" ").headOption.getOrElse(return db)
          c = up.trim.getnext(2).toString
          c match {
            case "$f" =>
              up.drop("$f")
              readUntil("$.") match {
                case List(cs, vs) => {
                  val v = db.syms(vs).asInstanceOf[Variable]
                  val stmt = Floating(label, db.syms(cs).asInstanceOf[Constant], v)
                  if (!db.vartyps.contains(stmt.typecode))
                    throw MMError(s"variable $v should have a variable typecode in ${stmt.label}")
                  v.activeFloat = stmt
                  db.statements += stmt
                  frames push stmt :: frames.pop
                }
                case _ => throw MMError("Incorrect $f statement")
              }
            case "$e" =>
              up.drop("$e")
              up.trim
              val stmt = Essential(label, readFormula("$."))
              db.statements += stmt
              frames push stmt :: frames.pop
            case "$a" =>
              up.drop("$a")
              up.trim
              val formula = readFormula("$.")
              db.statements += Assert(label, formula, Assert.trimFrame(formula, frames.head), lastComment, None)
            case "$p" =>
              up.drop("$p")
              up.trim
              val formula = readFormula("$=")
              db.statements += Assert(label, formula, Assert.trimFrame(formula, frames.head),
                lastComment, Some(readUntil("$.")))
            case _ => throw MMError("Not a valid metamath command: " + c + " in " + up.remainder.subSequence(0, 200))
          }
          lastComment = None
      }
    }

    def readFormula(s: String) = {
      val ls = readUntil(s).map(db.syms)
      Formula(ls.head.asInstanceOf[Constant], ls.tail)
    }
    db
  }

  private def readUntilRaw(s: String) = {
    var ret = new StringBuilder
    while (!up.remainder.startsWith(s)) {
      ret += up.next
    }
    up.drop(s)
    ret.toString
  }

  private def readUntil(s: String) = {
    var active = true
    var out = new MutableList[String]
    for (
      tok <- (if (s.length == 1) up.next(s.head, Reader.FS.toChar)._1 // FS, because there HAS to be a char
      else readUntilRaw(s)).split("\\s+")
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
}

class CommentParser(db: MMDatabase) extends RegexParsers {
  var commentTyp: String = _
  lazy val specialCommentStart: Parser[String] = "$j" | "$t"
  lazy val specialComment = phrase((command*) <~ blockComment)
  lazy val command = keyword ~ (commandElement*) <~ ";"
  lazy val commandElement: Parser[CommandElement] = keyword | (stringExpr ^^ Arg)
  lazy val keyword = blockComment ~> ("[A-Za-z0-9_]+".r ^^ Keyword) <~ blockComment
  lazy val stringExpr = (literal ~ (("+" ~> literal)*)) ^^ { case a ~ l => a + l.fold("")(_ + _) }
  lazy val literal = blockComment ~>
    ("'([^']*)'|\"([^\"]*)\"".r ^^ { s => s.substring(1, s.length - 1) }) <~ blockComment
  lazy val blockComment = """/\*(?>(?:(?>[^*]+)|\*(?!/))*)\*/""".r*

  class CommandElement
  case class Keyword(s: String) extends CommandElement
  case class Arg(s: String) extends CommandElement

  def parseComment(str: String): Boolean = {
    parse(specialCommentStart, str) match {
      case Success(s, continuation) =>
        parse(specialComment, continuation) match {
          case Success(cs, _) =>
            cs foreach { case k ~ l => handleCommand(s, k, l) }
            true
          case NoSuccess(e, _) => throw MMError(e.toString)
        }
      case _ => false
    }
  }

  def handleCommand(s: String, k: Keyword, l: List[CommandElement]) {
    (s, k.s) match {
      case ("$j", "syntax") =>
        db.grammatical = true
        l match {
          case List(Arg(typ), Keyword("as"), Arg(vtyp)) => db.typecodes.put(typ, vtyp)
          case _ => l foreach {
            case Arg(typ) => db.typecodes.put(typ, typ)
            case _ => throw MMError("Invalid $j 'syntax' invocation")
          }
        }
      case ("$j", "grammar") => db.grammatical = true
      case ("$j", "unambiguous") =>
        db.grammatical = true
        db.unambiguous = true
        l match {
          case List() =>
          case List(Arg(parser)) =>
          case _ => throw MMError("Invalid $j 'unambiguous' invocation")
        }
      case ("$j", _) =>
      case ("$t", "htmldef") => l match {
        case List(Arg(sym), Keyword("as"), Arg(value)) => db.htmldefs.put(sym, value)
        case _ => throw MMError("Invalid $t 'latexdef' invocation")
      }
      case ("$t", "althtmldef") => l match {
        case List(Arg(sym), Keyword("as"), Arg(value)) => db.althtmldefs.put(sym, value)
        case _ => throw MMError("Invalid $t 'althtmldef' invocation")
      }
      case ("$t", "latexdef") => l match {
        case List(Arg(sym), Keyword("as"), Arg(value)) => db.latexdefs.put(sym, value)
        case _ => throw MMError("Invalid $t 'htmldef' invocation")
      }
      case ("$t", "htmlvarcolor") =>
      case ("$t", "htmltitle") =>
      case ("$t", "htmlhome") =>
      case ("$t", "exthtmltitle") =>
      case ("$t", "exthtmlhome") =>
      case ("$t", "exthtmllabel") =>
      case ("$t", "htmldir") =>
      case ("$t", "althtmldir") =>
      case ("$t", "htmlbibliography") =>
      case ("$t", "exthtmlbibliography") =>
      case ("$t", "htmlcss") =>
      case ("$t", "htmlfont") =>
      case _ => throw MMError(s"Unknown keyword '${k.s}'")
    }
  }
}

class Sym(val id: String) {
  def str = id
  var seq: Int = _
  override def toString = id
}
case class Constant(override val id: String) extends Sym(id) {
  var tc: Boolean = false
}
case class Variable(override val id: String) extends Sym(id) {
  var activeFloat: Floating = _
}

case class Formula(typecode: Constant, expr: List[Sym]) {
  require(typecode.tc)
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
        case Disjointness(v @ _*) => for (a <- v; b <- v) if (a != b) f(a, b)
      })
    }
  }
}

case class Assert(override val label: GlobalName, override val formula: Formula, val frame: Frame,
  val comment: Option[String], val proof: Option[List[String]]) extends Statement(label, formula)
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
