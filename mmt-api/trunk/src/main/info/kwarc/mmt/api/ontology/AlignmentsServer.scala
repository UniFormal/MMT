package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import web._

import scala.collection.mutable
import QueryTypeConversion._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.{DeclaredLink, DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.refactoring.ArchiveStore
import info.kwarc.mmt.api.symbols.{DeclaredStructure, DefinedStructure, FinalConstant}
import info.kwarc.mmt.api.utils._

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

sealed abstract class Reference

case class LogicalReference(mmturi: ContentPath) extends Reference {
  override def toString = mmturi.toPath
}

case class PhysicalReference(url: URI) extends Reference {
  override def toString = url.toString
}

sealed abstract class Alignment {
  val from: Reference
  val to: Reference
  var props: List[(String, String)] = Nil

  def ->(that: Alignment): Alignment

  // def toJSON: (JSONString, JSONObject)

  var isGenerated = false

  def reverse: Alignment
}

sealed abstract class FormalAlignment extends Alignment {
  val from: LogicalReference
  val to: LogicalReference
  val invertible: Boolean

  def toTerm : Term = to match {
    case LogicalReference(t: GlobalName) ⇒ OMS(t)
    case LogicalReference(t: MPath)      ⇒ OMMOD(t)
  }
}

case class SimpleAlignment(from: LogicalReference, to: LogicalReference, invertible: Boolean) extends FormalAlignment {

  def toJSON = (JSONString("Simple"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString))
  )))

  def reverse = if (invertible) SimpleAlignment(to, from, true) else InformalAlignment(to, from)

  def ->(that: Alignment): Alignment = {
    require(to == that.from)
    val ret = that match {
      case SimpleAlignment(tfrom, tto, inv) =>
        SimpleAlignment(from, tto, invertible && inv)
      case InformalAlignment(tfrom, tto) =>
        InformalAlignment(from,tto)
      case ArgumentAlignment(tfrom,tto,inv,arguments) =>
        ArgumentAlignment(from,tto,invertible && inv,arguments)
    }
    ret.isGenerated = true
    ret
  }

  override def toString = from.toString + " " + to.toString +
    " direction=" + (if (invertible) """"both"""" else """"forward"""") +
    props.filter(x ⇒ x._1 != "direction").map(p ⇒ " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")
}

case class ArgumentAlignment(from: LogicalReference, to: LogicalReference, invertible: Boolean,
                             arguments: List[(Int, Int)]) extends FormalAlignment {
  def toJSON = (JSONString("Argument"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString)),
    (JSONString("args"), JSONArray(arguments.map(p ⇒ JSONArray.fromList(List(JSONInt(p._1), JSONInt(p._2))))))
  )))

  def reverse = if (invertible) ArgumentAlignment(to, from, true, arguments.map(p ⇒ (p._2, p._1))) else
    InformalAlignment(to, from)

  override def toString = from.toString + " " + to.toString +
    " direction=" + (if (invertible) """"both"""" else """"forward"""") +
    " " + """arguments="""" + arguments.map(p ⇒ "(" + p._1 + "," + p._2 + ")").mkString("") +
    """"""" +
    props.filter(x ⇒ !(List("direction", "arguments") contains x._1)).map(p ⇒ " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")

  private def combine(args: List[(Int,Int)]) : List[(Int,Int)] = arguments.map(p => {
    val p2 = args.find(q => p._2==q._1)
    p2.map(q => (p._1,q._2))
  }) collect {
    case Some((a,b)) => (a,b)
  }

  def ->(that: Alignment): Alignment = {
    require(to == that.from)
    val ret = that match {
      case SimpleAlignment(tfrom, tto, inv) =>
        ArgumentAlignment(from, tto, invertible && inv,arguments)
      case InformalAlignment(tfrom, tto) =>
        InformalAlignment(from,tto)
      case ArgumentAlignment(tfrom,tto,inv,args2) =>
        ArgumentAlignment(from,tto,invertible && inv,combine(args2))
    }
    ret.isGenerated = true
    ret
  }
}

case class InformalAlignment(from: Reference, to: Reference) extends Alignment {
  def toJSON = (JSONString("Informal"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString))
  )))

  def reverse = InformalAlignment(to, from)

  override def toString = from.toString + " " + to.toString +
    props.map(p ⇒ " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")

  def ->(that: Alignment): Alignment = {
    require(to == that.from)
    val ret = InformalAlignment(from,that.to)
    ret.isGenerated = true
    ret
  }
}

object SimpleAlignment {
  def apply(from: ContentPath, to: ContentPath, invertible: Boolean, props: List[(String, String)] = Nil): SimpleAlignment = {
    val ret = SimpleAlignment(LogicalReference(from), LogicalReference(to), invertible)
    ret.props = props
    ret
  }
}
object ArgumentAlignment {
  def apply(from: ContentPath, to: ContentPath, invertible: Boolean, args: List[(Int, Int)], props: List[(String, String)] = Nil): ArgumentAlignment = {
    val ret = ArgumentAlignment(LogicalReference(from), LogicalReference(to), invertible, args)
    ret.props = props
    ret
  }
}
object InformalAlignment {
  def apply(from: ContentPath, to: URI, props: List[(String, String)] = Nil): InformalAlignment = {
    val ret = InformalAlignment(LogicalReference(from), PhysicalReference(to))
    ret.props = props
    ret
  }
}

class AlignmentsServer extends ServerExtension("align") {

  override val logPrefix = "align"

  private val alignments = mutable.HashSet[Alignment]()

  private lazy val archives : ArchiveStore = controller.extman.get(classOf[ArchiveStore]).headOption.getOrElse {
    val a = new ArchiveStore
    controller.extman.addExtension(a)
    a
  }


  override def start(args: List[String]) {
    controller.extman.addExtension(new AlignQuery)
    args.foreach(a ⇒ try {
      val file = File(a)
      val fs = FilePath.getall(file).filter(_.getExtension.contains("align"))
      log("Files: " + fs)
      fs.foreach(readFile)
    } catch {
      case e: Exception ⇒ println(e.getMessage)
    })
  }
  override def destroy {
    controller.extman.get(classOf[AlignQuery]) foreach { a ⇒
      a.destroy
      controller.extman.removeExtension(a)
    }
  }

  private var nsMap = NamespaceMap.empty


  def apply(path: List[String], query: String, body: Body) = {
    path match {
      case "from" :: _ ⇒
        val path = Path.parseS(query, nsMap)
        val toS = getAlignments(LogicalReference(path)).map(_.to.toString)
        log("Alignment query: " + query)
        log("Alignments from " + path + ":\n" + toS.map(" - " + _).mkString("\n"))
        Server.TextResponse(toS.mkString("\n"))
      case "add" :: _ ⇒
        val str = Try(body.asString).getOrElse("")
        val formData : JSONObject = Try(JSON.parse(str).asInstanceOf[JSONObject]).getOrElse(JSONObject(List()))
        log(formData.toString)
        val regex = """\\/""".r
        val addedAlignments = if (formData.nonEmpty) {
          val fromRaw = formData("from").getOrElse("").toString
          val toRaw = formData("to").getOrElse("").toString
          val from = regex.replaceAllIn(fromRaw,  "/")
          val to = regex.replaceAllIn(toRaw, "/")
          log("Adding alignment from " + from + " to " + to)
          processString(from + " " + to)
        } else {
          0
        }
        Server.TextResponse("Added " + addedAlignments + " alignments")
      case _ ⇒
        log(path.toString) // List(from)
        log(query.toString) // an actual symbol path
        log(body.toString) //whatever
        Server.TextResponse("")
    }
  }

  def getAll = alignments.toList

  def getAlignments(from: Reference): List[Alignment] = {
    var res: List[Reference] = Nil
    def recurse(c: Reference): List[Alignment] = {
      //if (res.contains(c)) return Nil
      res ::= c
      val ret = (alignments.filter(a ⇒ a.from == c).toList :::
        alignments.collect {
          case a if a.to == c ⇒ a.reverse
        }.toList).filter(a ⇒ !res.contains(a.to))
      ret ::: ret.flatMap(a ⇒ recurse(a.to).map(a -> _)) //a => recurse(a.from).map(a -> _))
    }
    recurse(from).distinct
  }

  def getAlignments(from: ContentPath): List[Alignment] = getAlignments(LogicalReference(from))

  def getFormalAlignments(from: ContentPath) = getAlignments(LogicalReference(from)).collect {
    case a: FormalAlignment ⇒ a
  }

  def getAlignmentsTo(from: ContentPath, in: DPath) = getAlignments(LogicalReference(from)).filter(a ⇒
    a.to.toString.startsWith(in.toString))

  def getFormalAlignmentsTo(from: ContentPath, in: DPath) = getFormalAlignments(from).filter(a ⇒
    a.to.toString.startsWith(in.toString))

  def translate(t: Term, to: DPath) = ???

  def CanTranslateTo(t: Term): List[String] = archives.getArchives

  private def makeAlignment(p1: String, p2: String, pars: List[(String, String)]): Unit = {
    val argls = """\((\d+),(\d+)\)(.*)""".r
    val direction = pars.find(p ⇒ p._1 == "direction")
    if (direction.isDefined) {
      if (pars.exists(_._1 == "arguments")) {
        var args: List[(Int, Int)] = Nil
        val item = pars.find(_._1 == "arguments").get
        var read = item._2.trim
        while (read != "") read match {
          case argls(i, j, r) ⇒
            args ::= (i.toInt, j.toInt)
            read = r.trim
          case _ ⇒ throw new Exception("Malformed argument pair list: " + item._2)
        }
        val ret = if (direction.get._2 == "forward")
          ArgumentAlignment(Path.parseMS(p1, nsMap), Path.parseMS(p2, nsMap), false, args, pars)
        else if (direction.get._1 == "backward")
          ArgumentAlignment(Path.parseMS(p2, nsMap), Path.parseMS(p1, nsMap), false, args, pars)
        else
          ArgumentAlignment(Path.parseMS(p1, nsMap), Path.parseMS(p2, nsMap), true, args, pars)
        alignments += ret
      } else {
        val ret = if (direction.get._2 == "forward")
          SimpleAlignment(Path.parseMS(p1, nsMap), Path.parseMS(p2, nsMap), false, pars)
        else if (direction.get._1 == "backward")
          SimpleAlignment(Path.parseMS(p2, nsMap), Path.parseMS(p1, nsMap), false, pars)
        else if (direction.get._2 == "both")
          SimpleAlignment(Path.parseMS(p1, nsMap), Path.parseMS(p2, nsMap), true, pars)
        else throw new Exception("unknown alignment direction: " + direction.get._2)
        alignments += ret
      }
    } else {
      val from: Reference = Try(LogicalReference(Path.parseMS(p1, nsMap))).getOrElse(PhysicalReference(URI(p1)))
      val to: Reference = Try(LogicalReference(Path.parseMS(p2, nsMap))).getOrElse(PhysicalReference(URI(p2)))
      val ret = InformalAlignment(from, to)
      ret.props = pars
      alignments += ret
    }
  }

  private def processString(s: String): Int = {
    val param = """(.+)\s*=\s*\"(.+)\"\s*(.*)""".r
    var alignmentsCount: Int = 0
    var rest = s
    if (rest.startsWith("namespace")) {
      val (abbr, path) = {
        val s = rest.substring(10).split("""\s""").map(_.trim).filter(_.nonEmpty)
        (s.head, s(1))
      }
      nsMap = nsMap.add(abbr, URI(path))
      //println("added namespace: " + abbr +" -> " + path)
      //println(nsMap)
    } else if (s.nonEmpty && !s.startsWith("//")) {
      val (p1, p2) = {
        val s = rest.split("""\s""").map(_.trim).filter(_.nonEmpty)
        //println(s.head)
        //println(s(1))
        rest = rest.substring(s.head.length).trim.substring(s(1).length).trim
        (s.head, s(1))
      }
      var pars: List[(String, String)] = Nil
      while (rest != "") rest match {
        case param(key, value, r) ⇒
          pars ::= (key, value)
          rest = r.trim
        case _ ⇒ throw new Exception("Malformed alignment: " + s)
      }
      makeAlignment(p1, p2, pars)
      alignmentsCount += 1
    }
    alignmentsCount
  }

  /* 
   * Read alignments from a file
   * @param file
   * @returns the number of alignments read
   */
  private def readFile(file: File) {
    val cmds = File.read(file).split("\n").map(_.trim).filter(_.nonEmpty)
    val tmp = cmds map processString
    val alignmentsCount = tmp.sum
    log(alignmentsCount + " alignments read from " + file.toString)
  }

  private def writeToFile(file: File) = File.write(file, alignments.map(_.toString).mkString("\n"))

  // TODO needs reworking
  private def readJSON(file: File) {
    /*
    val json = JSON.parse(File.read(file))
    json match {
      case obj: JSONObject => obj.map foreach {
         case (JSONString("View"), alignmentList:JSONArray) =>
            alignmentList.values foreach {
               case o: JSONObject =>
                 alignments += SimpleAlignment(
                   Path.parseS(o("from") match {
                     case Some(JSONString(s)) => s
                     case _ => ???
                   },nsMap),
                   Path.parseS(o("to") match {
                     case Some(JSONString(s)) => s
                     case _ => ???
                   },nsMap)
                 )
            }
         case (JSONString("Informal"),o:JSONObject) =>
          alignments += InformalAlignment(
            Path.parseS(o("from") match {
              case Some(JSONString(s)) => s
              case _ => ???
            },nsMap),
            o("to") match {
              case Some(JSONString(s)) => URI(s)
              case _ => ???
            })
         case (JSONString("Simple"),o:JSONObject) =>
           alignments += SimpleAlignment(
             Path.parseS(o("from") match {
               case Some(JSONString(s)) => s
               case _ => ???
             },nsMap),
             Path.parseS(o("to") match {
               case Some(JSONString(s)) => s
               case _ => ???
             },nsMap)
           )
         case (JSONString("Partial"),o:JSONObject) =>
           alignments += PartialAlignment(
             Path.parseS(o("from") match {
               case Some(JSONString(s)) => s
               case _ => ???
             },nsMap),
             Path.parseS(o("to") match {
               case Some(JSONString(s)) => s
               case _ => ???
             },nsMap)
           )
         case _ =>
      }
      case _ =>
    }
    */
  }

  /** translation along alignments */
  private class AlignQuery extends QueryExtension("align", PathType, ESet(StringType)) {
    def evaluate(argument: BaseType, params: List[String]) = {
      log("Evaluating align query")
      log(argument.toString)
      params.foreach(p => log(p.toString))
      val o = argument match {
        case p : ContentPath => p
        case _       ⇒ throw ImplementationError("evaluation of ill-typed query")
      }
      // controller.extman.get(classOf[AlignmentsServer])
      ???
    }
  }

}