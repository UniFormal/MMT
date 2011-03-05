/*

package info.kwarc.mmt.api.libraries
import jomdoc.modules._
import jomdoc.symbols._
import jomdoc.objects._
import jomdoc.utils._
import jomdoc.presentation._
import scala.xml.{Node,NodeSeq}
import scala.actors.{Actor}

class Input(buffer : String) {
   def next : String
   def lookahead(i: Int) : List[String]
   def shift(i: Int)
}


object lah3 {
  def unapply(i : Input) : Option[(String,String,String)] = {
      val l = i.lookahead(3)
      Some(l(0), l(1), l(2))
   }
}

object lah5 {
  def unapply(i : Input) : Option[(String,String,String,String,String)] = {
      val l = i.lookahead(5)
      Some(l(0), l(1), l(2), l(3), l(4))
   }
}

abstract class TextReader(controller : frontend.Controller, report : frontend.Report) {
   def log(s : String) = report("reader", s)
   def add(e : StructuralElement) {
      controller.add(e)
   }
   
   def readModules(path : DPath, i : Input) {
      var more = true
      var input = i
      while (more) {
         input match {
            case lah3("theory", n, "{") =>
               input.shift(3)
               val name = Path.parseLocal(n).toLocalPath
               val thy = new Theory(path, name, None)
               add(thy)
               val tpath = path ? name
               readSymbols(tpath, input)
            case lah5("theory", n, "meta", m, "{") =>
               input.shift(5)
               val name = Path.parseLocal(n).toLocalPath
               val mt = Path.parse(m, path)
               val thy = new Theory(path, name, Some(mt))
               add(thy)
               val tpath = path ? name
               readSymbols(tpath, input)
            case "view" :: n :: ":" :: f :: "->" :: t :: "{" :: rest =>
               val name = Path.parseLocal(n).toLocalPath
               val from = Path.parseM(f,path)
               val to = Path.parseM(t,path)
               val view = new DeclaredView(path, name, from, to, None)
               add(view)
               val vpath = path ? name
               input = readAssignments(vpath, to, rest)
            case Nil => more = false
            case s => throw errors.Parse("expected: ...; found: " + s)
         }
      }
   }
   def readSymbols(path : MPath, i : Input) : Input = {
      var more = true
      var input = i
      while (more) {
         input match {
            case "constant" :: n :: ":" :: rest =>
               val name = Path.parseLocal(n).toLocalPath
               var tp : Option[Term] = None
               var df : Option[Term] = None
               val (t, i) = parseTerm(rest); tp = Some(t); input = i
               input match {
                  case "=" :: rest =>
                     val (d, i) = parseTerm(rest); df = Some(d); input = i
                  case rest =>
               }
               val con = new Constant(path, name, tp, df, RoleConstant)
               add(con)
            case "constant" :: n :: "=" :: t :: rest =>
               input = rest
            case "constant" :: n :: rest =>
               input = rest
            case "structure" :: n :: ":" :: f :: "{" :: rest =>
               val name = Path.parseLocal(n).toLocalPath
               val from = Path.parse(f, path)
               val str = new DeclaredStructure(path, name, from, None)
               add(str)
               val spath = path / name
               input = readAssignments(spath, path, rest)
            case "}" :: rest =>
               more = false
               input = rest
            case s => throw errors.Parse("expected: ...; found: " + s)
         }
      }
      input
   }
   def readAssignments(from : MPath, to : MPath, i : Input) : Input = {
      var more = true
      var input = i
      while (more) {
         input match {
            case "conass" :: n :: ":=" :: rest =>
               val (tm, i) = parseTerm(rest)
               input = i
//               val ass = new ConstantAssignment(n, tm)
            case "strass" :: n :: ":=" :: rest =>
               val (mor, i) = parseMorphism(rest)
               input = i
//             val ass = new StructureAssignment(n, mor)
            case "}" :: rest =>
               more = false
               input = rest
            case s => throw errors.Parse("expected: ...; found: " + s)
         }
      }
      input
   }
   def parseTerm(i : Input) : (Term,Input)
   def parseMorphism(i : Input) : (Morph,Input)
}

*/