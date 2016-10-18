package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import web._

import scala.collection.mutable
import QueryTypeConversion._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.refactoring.{ArchiveStore, FullArchive}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.{URI, _}

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.List
import scala.util.{Success, Try}

class AlignmentsServer extends ServerExtension("align") {

  override val logPrefix = "align"

  private object alignments {
    private val set = mutable.HashMap[(Reference,Reference),Alignment]()

    def toList = set.values.toList

    def get(r : Reference, closure : Option[Alignment => Boolean] = None) : List[Alignment] = {
      /*
      var res: List[Reference] = List(r)
      def recurse(c: Reference): List[Alignment] = {
        // if (res.contains(c)) return Nil
        val neighb = filter(a ⇒ a.from == c && !res.contains(a.to))
        res = res ::: neighb.map(_.to)
        def recstep(a : Alignment) : List[Reference] = {
          val first = recurse(a.to)
          val second = first.map(b => {
            val comp = a -> b
            +=(comp)
            comp.to
          })
          second
        }
        val recs = neighb.flatMap(recstep)
        val trans = neighb.map(_.to) ::: recs
        trans.map(p => set(c, p)).distinct
      }
      recurse(r)
      */
      var res : List[Reference] = List(r)
      def recurse(start : Alignment) : List[Alignment] = {
        res ::= start.to
        val allneighbs = filter(a => a.from == start.to) // && !res.contains(a.to) && closure.get(a))
        val conts = allneighbs.filter(closure.get).map(a => add(start -> a)).filter(a => !res.contains(a.to))
        start :: conts.flatMap(recurse)
      }
      if (closure.isEmpty) filter(a => a.from == r) else {
        val all = filter(a => a.from == r).flatMap(recurse)
        all.map(a => addToList(a,all)).distinct
      }
    }

    def collect[B, That](pf: PartialFunction[Alignment, B])(implicit bf: CanBuildFrom[List[Alignment], B, That]) : That = toList.collect[B,That](pf)(bf)

    def filter(f : Alignment => Boolean) = toList.filter(f)
    def map[B](f : Alignment => B) = toList.map(f)

    def +=(a : Alignment) = {
      val newA = add(a)
      val newAr = add(newA.reverse)
      set((newA.from,newA.to)) = newA
      set((newAr.from,newAr.to)) = newAr
    }

    private def addToList(a : Alignment, b : List[Alignment]) = {
      val ls = b.filter(al => al.from == a.from && al.to == a.to && al != a) ::: set.get((a.from,a.to)).toList
      ls.foldLeft(a)((x,y) => add(x, Some(y)))
    }

    private def add(a : Alignment, bOpt : Option[Alignment] = None) : Alignment = {
      val b = bOpt.getOrElse(set.getOrElse((a.from,a.to),return a))
      (a,b) match {
        case (p1 : ConceptPair, p2 : ConceptPair) => p1
        case (p1 : ConceptAlignment, p2 : ConceptAlignment) => p1
        case (p1 : InformalAlignment, p2 : InformalAlignment) =>
          p1.props = (p1.props ::: p2.props).distinct
          p1
        case (p1, p2 : SimpleAlignment) =>
          val invertible = p1 match {
            case f: FormalAlignment => f.invertible || p2.invertible
            case _ => p2.invertible
          }
          val ret = SimpleAlignment(p2.from,p2.to, invertible)
          ret.props = (p1.props ::: p2.props).distinct
          ret
        case (p1 : SimpleAlignment, p2) =>
          val invertible = p2 match {
            case f: FormalAlignment => f.invertible || p1.invertible
            case _ => p1.invertible
          }
          val ret = SimpleAlignment(p1.from,p1.to, invertible)
          ret.props = p1.props ::: p2.props
          ret
        case (p1 : ArgumentAlignment, p2 : ArgumentAlignment) if p1.arguments == p2.arguments =>
          val ret = ArgumentAlignment(p1.from,p1.to,p1.invertible || p2.invertible, p1.arguments)
          ret.props = p1.props ::: p2.props
          ret
        case (p1 : ArgumentAlignment, p2) =>
          val invertible = p2 match {
            case f: FormalAlignment => f.invertible || p1.invertible
            case _ => p1.invertible
          }
          val ret = ArgumentAlignment(p1.from,p1.to, invertible, p1.arguments)
          ret.props = p1.props ::: p2.props
          ret
        case (p1, p2 : ArgumentAlignment) =>
          val invertible = p1 match {
            case f: FormalAlignment => f.invertible || p2.invertible
            case _ => p2.invertible
          }
          val ret = ArgumentAlignment(p2.from,p2.to, invertible, p2.arguments)
          ret.props = p1.props ::: p2.props
          ret
        case _ => throw new Exception("Alignments not compatible: " + a.getClass + " and " + b.getClass)
      }
    }

  }

  private lazy val archives : ArchiveStore = controller.extman.get(classOf[ArchiveStore]).headOption.getOrElse {
    val a = new ArchiveStore
    controller.extman.addExtension(a)
    a
  }

  private var filebase : File = null
  override def start(args: List[String]) {
    controller.extman.addExtension(new AlignQuery)
    args.headOption.foreach(a ⇒ try {
      filebase = File(a)
      val fs = FilePath.getall(filebase)
      val afiles = fs.filter(f => f.getExtension.contains("align"))
      log("Files: " + afiles)
      afiles foreach readFile
      log(alignments.toList.filterNot(_.isGenerated).length + " Alignments read")
    } catch {
      case e: Exception ⇒ throw e // println(e.getMessage)
    })
  }
  override def destroy {
    controller.extman.get(classOf[AlignQuery]) foreach { a ⇒
      a.destroy
      controller.extman.removeExtension(a)
    }
  }

  private var nsMap = NamespaceMap.empty

  private def save(a : Alignment): Unit = if (filebase != null) {
    val savefile = filebase / "save.align"
    File.append(savefile,"\n" + a.toString)
  }

  def addNew(a : Alignment) = {
    alignments += a
    save(a)
  }

  def apply(path: List[String], query: String, body: Body, session: Session) = {
    path match {
      case "from" :: _ ⇒
        val path = Path.parseS(query, nsMap)
        val toS = if (query.contains("transitive=\"true\"")) alignments.get(LogicalReference(path), Some(_ => true)).map(_.to.toString)
        else alignments.get(LogicalReference(path)).map(_.to.toString)
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

  def getAllPrimitive = alignments.toList

  def getAlignments(s : String) = alignments.get(ConceptReference(s),Some(_ => true))

  def getAlignments(cp : ContentPath) = alignments.get(LogicalReference(cp),Some(_ => true))

  def getFormalAlignments(cp : ContentPath) = alignments.get(LogicalReference(cp), Some(_.isInstanceOf[FormalAlignment])) collect {
    case fa : FormalAlignment => fa
  }

  def getConcepts = (alignments collect {
    case ca: ConceptAlignment => ConceptReference(ca.concept)
  }).distinct.map(_.con).sortWith((p,q) => p < q)

  def getConceptAlignments(con : String) = {
    val direct = alignments.get(ConceptReference(con))
    (direct ::: direct.flatMap(a => alignments.get(a.to,Some(_ => true))).collect{
      case ca : ConceptAlignment if ConceptReference(ca.concept) != ConceptReference(con) => ConceptPair(con,ca.concept)
      case ConceptPair(f,_) if f != ConceptReference(con) => ConceptPair(con,f.con)
      case ConceptPair(_,t) if t != ConceptReference(con) => ConceptPair(con,t.con)
    }).distinct
  }

  def makeAlignment(p1: String, p2: String, allpars: List[(String, String)]): Alignment = {
    val argls = """\((\d+),(\d+)\)(.*)""".r
    val direction = allpars.find(p ⇒ p._1 == "direction")
    val pars = allpars.filterNot(p => p._1 == "direction" || p._1 == "arguments")
    if (direction.isDefined) {
      if (allpars.exists(_._1 == "arguments")) {
        var args: List[(Int, Int)] = Nil
        val item = allpars.find(_._1 == "arguments").get
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
        ret
      } else {
        val ret = if (direction.get._2 == "forward")
          SimpleAlignment(Path.parseMS(p1, nsMap), Path.parseMS(p2, nsMap), false, pars)
        else if (direction.get._1 == "backward")
          SimpleAlignment(Path.parseMS(p2, nsMap), Path.parseMS(p1, nsMap), false, pars)
        else if (direction.get._2 == "both")
          SimpleAlignment(Path.parseMS(p1, nsMap), Path.parseMS(p2, nsMap), true, pars)
        else throw new Exception("unknown alignment direction: " + direction.get._2)
        ret
      }
    } else {
      val from: URIReference = Try(LogicalReference(Path.parseMS(p1, nsMap))).getOrElse(PhysicalReference(URI(p1)))
      val to: URIReference = Try(LogicalReference(Path.parseMS(p2, nsMap))).getOrElse(PhysicalReference(URI(p2)))
      val ret = InformalAlignment(from, to)
      ret.props = pars
      ret
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
    } else if (s.nonEmpty && s.startsWith("|")) {
      val p = s.tail.split('|').map(_.trim)
      if (p.length == 2) {
        val Array(con, uri) = p
        val ref = Try(LogicalReference(Path.parseMS(uri, nsMap))).getOrElse(PhysicalReference(URI(uri)))
        alignments += ConceptAlignment(ref,con)
        alignmentsCount += 1
      }
    } else if (s.nonEmpty && s.startsWith("<")) {
      val p = s.trim.drop(1).dropRight(1).split('|').map(_.trim)
      alignments += ConceptPair(p.head,p.tail.head)
      alignmentsCount += 1
    } else if (s.nonEmpty && !s.startsWith("//")) {
      val (p1, p2) = {
        val s = rest.split("""\s""").map(_.trim).filter(_.nonEmpty)
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
      alignments += makeAlignment(p1, p2, pars)
      alignmentsCount += 1
    }
    alignmentsCount
  }

  private def readFile(file: File) {
    val cmds = File.read(file).split("\n").map(_.trim).filter(_.nonEmpty)
    val tmp = cmds map processString
    val alignmentsCount = tmp.sum
    log(alignmentsCount + " alignments read from " + file.toString)
  }

  def writeToFile(file: File) = File.write(file, getAsString)

  def getAsString = alignments.filter(!_.isGenerated).map(_.toString).mkString("\n")

  // TODO needs reworking
  private def readJSON(file: File) {
    ???
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