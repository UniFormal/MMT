package info.kwarc.mmt.metamath

import info.kwarc.mmt.api._
import documents._
import archives._
import info.kwarc.mmt.api.parser.Reader
import info.kwarc.mmt.api.utils.{File, Unparsed}

class Importer extends archives.Importer {
   val key = "mm-omdoc"
   def inExts = List("mm")
  override def inDim = RedirectableDimension("mm")


   def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
      log("Reading " + bf.inFile)
      val parser = new MMParser(bf.inFile)
      val e = {//try {
        log("Parsing...")
         parser.parse
      }// catch {
      //  case MMError(s) => println(s)
      //}
     //println(e)

      //val conv = new PVSImportTask(controller, bf, index)

      BuildResult.empty
   }

}

case class MMError(s: String) extends java.lang.Exception(s)

case class MMParser(f:File) {

  val statements = new Statementlist(None)
  var currentstatements = statements

  val up = new Unparsed(File.read(f),msg => throw MMError(msg))

  def parse: Unit = while (!up.empty) {
    var c = up.trim.getnext(2).toString
    print(".")
    c match {
      case "$(" =>
        up.drop("$(")
        currentstatements add Comment(readUntil("$)"))
      case "$c" =>
        up.drop("$c")
        currentstatements add Constant(readUntil("$.").trim)
      case "$v" =>
        up.drop("$v")
        currentstatements add Variable(readUntil("$.").trim)
      case "$d" =>
        up.drop("$d")
        up.trim
        val vars = readUntil("$.").split(" ").map(_.trim)
        currentstatements add Disjointness(vars:_*)
      case _ =>
        val label = readUntil(" ")
        c = up.trim.getnext(2).toString
        c match {
          case "$f" =>
            up.drop("$f")
            up.trim
            val const = readUntil(" ")
            up.trim
            val v = readUntil(" ")
            up.trim
            currentstatements add FFormula(label,c,v)
            up.drop("$.")
          case "$e" =>
            up.drop("$e")
            up.trim
            val const = readUntil(" ")
            up.trim
            currentstatements add EFormula(label,const,readUntil("$.").split(" ").map(_.trim).toList)
            up.drop("$.")
          case _ => throw new MMError("Not a valid metamath command: " + c + " in " + up.remainder.subSequence(0,200))
        }
    }
  }


  private def readUntil(s : String) = if (s.length == 1) up.next(s.head,Reader.FS.toChar)._1 // FS, because there HAS to be a char
  else {
    var ret = ""
    while (!up.remainder.startsWith(s)) {
      ret += up.next
    }
    up.drop(s)
    ret
  }
}

class MMStatement
class Statementlist(parentOpt : Option[Statementlist]) {
  private var list : List[MMStatement] = Nil
  def add(s : MMStatement) = list ::= s
  def get = list.reverse
  def parent = parentOpt.get
}
case class Comment(s : String) extends MMStatement
case class Constant(s : String) extends MMStatement
case class Variable(s : String) extends MMStatement
case class FFormula(label : String, c : String, v : String) extends MMStatement
case class EFormula(label : String, c : String, syms : List[String]) extends MMStatement
case class Disjointness(v : String*) extends MMStatement