package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import web._

import scala.collection.mutable
import QueryTypeConversion._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.DeclaredTheory
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

abstract class Alignment {
  val from: Reference
  val to: Reference
  var props: List[(String, String)] = Nil

  def ->(that: Alignment): Alignment

  // def toJSON: (JSONString, JSONObject)

  var isGenerated = false

  def reverse: Alignment
}

abstract class FormalAlignment extends Alignment {
  val from: LogicalReference
  val to: LogicalReference
  val invertible: Boolean

  def toTerm : Term = to match {
    case LogicalReference(t: GlobalName) ⇒ OMS(t)
    case LogicalReference(t: MPath)      ⇒ OMMOD(t)
  }

  def applicable(t: Term): Boolean = t match {
    case OMS(f) if f == from.mmturi ⇒ true
    case _                          ⇒ altapplicable(t)
  }
  protected def altapplicable(t: Term): Boolean

  def apply(t: Term)(implicit cont: Option[StatelessTraverser] = None): Term = {
    assert(applicable(t))
    t match {
      case OMS(f) if f == from.mmturi ⇒ toTerm
      case _                          ⇒ translate(t, cont)
    }
  }
  protected def translate(t: Term, cont: Option[StatelessTraverser] = None): Term

  private val top = this

  def ->(that: Alignment) = {
    require(top.to == that.from)
    that match {
      case a : FormalAlignment =>
        val ret = new FormalAlignment {
          val from = top.from
          val to = a.to
          val invertible = top.invertible && a.invertible
          override def applicable(t : Term) = top.applicable(t) && a.applicable(top.apply(t))
          protected def altapplicable(t : Term) = top.altapplicable(t) && a.altapplicable(top.apply(t))
          protected def translate(t: Term, cont: Option[StatelessTraverser] = None) =
            a.apply(top.translate(t))
          def reverse = a.reverse -> top.reverse
        }
        ret.isGenerated = true
        ret
      case i : InformalAlignment =>
        val ret = InformalAlignment(top.from, i.to)
        ret.isGenerated = true
        ret
    }
  }
}

class ComplexAlignment(src : GlobalName, target : MPath,targetterm : Term) extends FormalAlignment {
  val from = LogicalReference(src)
  val to = LogicalReference(target)

  val invertible = false

  override def toTerm = targetterm

  protected def altapplicable(t: Term) = false

  // should never be called
  protected def translate(t: Term, cont: Option[StatelessTraverser] = None): Term = toTerm
  def reverse = InformalAlignment(to,from)
}

case class SimpleAlignment(from: LogicalReference, to: LogicalReference, invertible: Boolean) extends FormalAlignment {

  def altapplicable(t: Term) = false
  def translate(t: Term, cont: Option[StatelessTraverser]) = t // cannot ever occur

  def toJSON = (JSONString("Simple"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString))
  )))

  def reverse = if (invertible) SimpleAlignment(to, from, true) else InformalAlignment(to, from)

  override def toString = from.toString + " " + to.toString +
    " direction=" + (if (invertible) """"both"""" else """"forward"""") +
    props.filter(x ⇒ x._1 != "direction").map(p ⇒ " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")
}

case class ArgumentAlignment(from: LogicalReference, to: LogicalReference, invertible: Boolean,
                             arguments: List[(Int, Int)]) extends FormalAlignment {

  def altapplicable(t: Term) = t match {
    case OMA(OMS(f), args) if f == from.mmturi        ⇒ true
    case ApplyMatch(OMS(f), args) if f == from.mmturi => true
    case _                                            ⇒ false
  }
  private def reorder(args: List[Term]) = {
    var nargs = args.map(t => {
      val apos = arguments.find(p => p._1==(args.indexOf(t) + 1))
      if (apos.isDefined) (apos.get._2,t) else (0,t)
    }).filter(_._1!=0).sortBy(_._1)
    val max = nargs.map(_._1).max
    (1 to max).map(i => nargs.find(p => p._1==i)).collect{
      case Some(p) => p._2
      case _ => ??? // TODO insert variables
    }.toList
    //val max = arguments.sortBy(_._2).last._2
    //(1 to max).map(i ⇒ args(arguments.find(p ⇒ p._2 == i).map(_._1).getOrElse(???))).toList // TODO insert variables
  }
  object ApplyMatch {
    //TODO foundation independence
    val path = DPath(utils.URI("http", "cds.omdoc.org") / "urtheories") ? "LF" ? "apply"
    val applyterm = OMS(path)
    def apply(f: Term, a: Term*) = OMA(applyterm, f :: a.toList)
    def unapply(t: Term) : Option[(Term,List[Term])] = t match {
      case OMA(app, f :: args) if app == applyterm =>
        unapply(f) match {
          case None => Some((f, args))
          case Some((c, args0)) => Some((c, args0 ::: args))
        }
      case _ => None
    }
  }
  def translate(t: Term, cont: Option[StatelessTraverser]) = t match {
    case OMA(OMS(f), args) if f == from.mmturi ⇒
      OMA(toTerm, reorder(args).map(a => cont.map(_.apply(a, Context.empty)).getOrElse(a))) // TODO insert variables
    case ApplyMatch(OMS(f),args) if f == from.mmturi =>
      ApplyMatch(toTerm, reorder(args).map(a => cont.map(_.apply(a, Context.empty)).getOrElse(a)):_*)
  }

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
}

case class InformalAlignment(from: Reference, to: Reference) extends Alignment {
  def toJSON = (JSONString("Informal"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString))
  )))

  def reverse = InformalAlignment(to, from)

  def ->(that: Alignment) = that match {
    case f: FormalAlignment   ⇒ InformalAlignment(from, f.to)
    case f: InformalAlignment ⇒ InformalAlignment(from, f.to)
  }

  override def toString = from.toString + " " + to.toString +
    props.map(p ⇒ " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")
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
/*
case class Alignment(kind: String, from: GlobalName, to: GlobalName, args: Option[List[(Int,Int)]]) {
  override def toString = s"$kind $from $to"
}
*/

class AlignmentsServer extends ServerExtension("align") {

  override val logPrefix = "align"

  private val alignments = mutable.HashSet[Alignment]()

  private var archives : ArchiveStore = null


  override def start(args: List[String]) {
    controller.extman.addExtension(new AlignQuery)
    archives = new ArchiveStore
    controller.extman.addExtension(archives)
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

  // val dones : mutable.HashMap[ContentPath,List[Alignment]] = mutable.HashMap.empty

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

  def translate(t: Term, to: DPath) = try { Some(Translator(to)(t)) } catch {
    case CanNotTranslate ⇒ None
    case e: Exception    ⇒ throw e
  }

  def CanTranslateTo(t: Term): List[String] = archives.getArchives

  private object CanNotTranslate extends Exception

  private case class Translator(target: DPath) extends StatelessTraverser {

    def apply(t: Term): Term = apply(t, Context.empty)

    implicit val cont = Some(this)

    def traverse(t: Term)(implicit con: Context, init: Unit): Term = t match {
      // TODO this completely ignores all but the first alignment that matches
      case s @ OMS(p)                  ⇒ getFormalAlignmentsTo(p, target).find(_.applicable(s)).map(_.apply(s)).getOrElse(throw CanNotTranslate)
      case s @ OMA(f @ OMS(fun), args) ⇒ getFormalAlignmentsTo(fun, target).find(_.applicable(s)).map(_.apply(s)).getOrElse(Traverser(this, t))
      case _                           ⇒ Traverser(this, t)
    }
  }

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
  private class AlignQuery extends QueryExtension("align", ObjType, ObjType) {
    def evaluate(argument: BaseType, params: List[String]) = {
      log("Evaluating align query")
      log(argument.toString)
      params.foreach(p => log(p.toString))
      val dpath = params match {
        case p :: Nil ⇒ Path.parseD(p, NamespaceMap.empty)
        case Nil      ⇒ throw ParseError("parameter expected")
        case _        ⇒ throw ParseError("exactly one parameter expected")
      }
      val o = argument match {
        case t: Term ⇒ t
        case o: Obj  ⇒ ??? // TODO
        case _       ⇒ throw ImplementationError("evaluation of ill-typed query")
      }
      // controller.extman.get(classOf[AlignmentsServer])
      translate(o, dpath).toList
    }
  }

}

abstract class FullArchive {
  val archive : Archive
  def foundations : List[DeclaredTheory]
  require(archive.ns.isDefined)
  val path = archive.ns.get
  val name = archive.id

  def declares(p : Path) : Boolean
}

class ArchiveStore extends Extension {

  case class InternalFullArchive(archive : Archive, foundations : List[DeclaredTheory]) extends FullArchive {
    private var contains : List[MPath] = Nil
    private var isread = false

    private def read = logGroup {
      log("Read relational " + name + "...")
      archive.readRelational(FilePath(""), controller, "rel")
      log("Loading Theories...")
      val theories = controller.evaluator.evaluate(Paths(IsTheory)).asInstanceOf[ESetResult].h.view.flatten.toList.map(s =>
        Path.parse(s.toString))
      logGroup {
        log("Namespace: " + path)
        contains = theories.filter(path <= _).collect {
          case t:MPath => t
        }.distinct
        log("Theories: ")
        logGroup {
          contains.foreach(t => log(" - " + t.toString.drop(archive.ns.get.toString.length)))
        }
      }
      isread = true
    }

    def declares(p : Path) : Boolean = p match {
      case d:DPath =>
        path <= d
      case m : MPath =>
        path <= m || foundations.exists(t => t.path == m)
      case s : GlobalName =>
        declares(s.module)
    }
  }


  private val stored : mutable.HashMap[String,FullArchive] =  mutable.HashMap()

  // partially mirrors alignmentfinder; TODO clean up

  private def flattened(th : DeclaredTheory) : List[DeclaredTheory] = {
    Try(controller.simplifier.flatten(th))
    val theories = //th ::
      th.getIncludes.map(p => Try(controller.get(p).asInstanceOf[DeclaredTheory]))
    val flats = theories.map({
      case Success(t) => Try(flattened(t))
      case Failure(e) => Failure(e)
    })
    val successes = flats.collect {
      case Success(t) => t
    }.flatten
    th :: successes
  }

  override def logPrefix = "ArchiveStore"

  override def start(args: List[String]) = Try {
    val archs = controller.backend.getArchives
    log("Archives found: " + archs.map(_.id).mkString(","))
    log("Reading archives:")
    logGroup {
      archs foreach { a =>
        log(a.id + "...")
        logGroup {
          if (a.ns.isDefined) {
            log("Namespace: " + a.ns.get)
            /*
            val fnd = Try(controller.get(a.foundation.get).asInstanceOf[DeclaredTheory]).map(Some(_)).getOrElse(None)
            log("Foundation: " + fnd.map(_.path.toString).getOrElse("None"))
            val found = fnd.map(flattened).getOrElse(Nil).distinct
            if (fnd.isDefined) log("Implied: " + found.tail.map(_.name.toString).mkString(", "))
            */
            stored += ((a.id, InternalFullArchive(a, Nil)))//((a.id, InternalFullArchive(a, found)))
          } else {
            log("No namespace given")
          }
        }
      }
    }
  }

  def getArchive(a : Archive) : Option[FullArchive] = stored.get(a.id)
  def getArchive(s : String) : Option[FullArchive] = stored.get(s)
  def getArchives : List[String] = stored.keys.toList
}