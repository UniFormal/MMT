package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import web._
import scala.collection.mutable
import QueryTypeConversion._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.refactoring.{ArchiveStore, FullArchive}
import info.kwarc.mmt.api.utils.{URI, _}

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.List
import scala.util.{Success, Try}

class AlignmentsServer extends ServerExtension("align") {

  override val logPrefix = "align"

  object alignments {
    private val set = mutable.HashMap[(Reference,Reference),Alignment]()
    private val trcls = mutable.HashMap[Reference,List[Alignment]]()

    def toList = set.values.toList

    def get(r : Reference) : List[Alignment] = {
      var res: List[Reference] = List(r)
      def recurse(c: Reference): List[Alignment] = {
        // if (res.contains(c)) return Nil
        val neighb = filter(a ⇒ a.from == c && !res.contains(a.to))
        res = res ::: neighb.map(_.to)
        def recstep(a : Alignment) = {
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
    }

    def collect[B, That](pf: PartialFunction[Alignment, B])(implicit bf: CanBuildFrom[List[Alignment], B, That]) : That = toList.collect[B,That](pf)(bf)

    def filter(f : Alignment => Boolean) = toList.filter(f)
    def map[B](f : Alignment => B) = toList.map(f)

    def +=(a : Alignment) = {
      add(a)
      add(a.reverse)
    }

    private def add(a : Alignment) = if (!set.isDefinedAt((a.from,a.to))) {
      set((a.from,a.to)) = a
    } else {
      val old = set((a.from,a.to))
      val newa = (a,old) match {
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
        case _ => throw new Exception("Alignments not compatible: " + old.getClass + " and " + a.getClass)
      }
      set.remove((a.from,a.to))
      set((a.from,a.to)) = newa
    }
  }

  private lazy val archives : ArchiveStore = controller.extman.get(classOf[ArchiveStore]).headOption.getOrElse {
    val a = new ArchiveStore
    controller.extman.addExtension(a)
    a
  }


  override def start(args: List[String]) {
    controller.extman.addExtension(new AlignQuery)
    args.foreach(a ⇒ try {
      val file = File(a)
      val fs = FilePath.getall(file)
      val afiles = fs.filter(f => f.getExtension.contains("align"))
      val sqlites = fs.filter(f => f.getExtension.contains("sqlite"))
      log("Files: " + (afiles ::: sqlites))
      afiles foreach readFile
      sqlites foreach readNnexus
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


  def apply(path: List[String], query: String, body: Body, session: Session) = {
    path match {
      case "from" :: _ ⇒
        val path = Path.parseS(query, nsMap)
        val toS = alignments.get(LogicalReference(path)).map(_.to.toString)
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

  def getConcepts = (alignments collect {
    case ca: ConceptAlignment => ca.concept
  }).distinct.sortWith((p,q) => p < q)
  def getConceptAlignments(s : String) : List[Alignment] = alignments.get(ConceptReference(s.toLowerCase))

  def getAllConceptAlignments = getConcepts map (s => (s,getConceptAlignments(s).map(_.to).distinct))

  def getFromArchive(from : FullArchive,to : Option[FullArchive]) = {
    val thpaths = from.theories
    val ths = thpaths.map (t => {
      Try(controller.get(t).asInstanceOf[DeclaredTheory])
    }).collect{
      case Success(th) =>
        controller.simplifier(th)
        th
    }
    val consts = ths.map(t => Try(t.getConstants)).collect{case Success(c) => c}.flatten
    val als = (consts.map(_.path) ::: thpaths).flatMap(getFormalAlignments)
    if (to.isDefined) als.filter(a => to.get.declares(a.to.mmturi)) else als

  }

  def getAlignments(from: ContentPath): List[Alignment] = alignments.get(LogicalReference(from))

  def getFormalAlignments(from: ContentPath) = alignments.get(LogicalReference(from)).collect {
    case a: FormalAlignment ⇒ a
  }

  def getAlignmentsTo(from: ContentPath, in: DPath) = alignments.get(LogicalReference(from)).filter(a ⇒
    a.to.toString.startsWith(in.toString))

  def getFormalAlignmentsTo(from: ContentPath, in: DPath) = getFormalAlignments(from).filter(a ⇒
    a.to.toString.startsWith(in.toString))

  def translate(t: Term, to: DPath) = ???

  def CanTranslateTo(t: Term): List[String] = archives.getArchives.map(_.name)

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
      val from: Reference = Try(LogicalReference(Path.parseMS(p1, nsMap))).getOrElse(PhysicalReference(URI(p1.replace("https://","http://"))))
      val to: Reference = Try(LogicalReference(Path.parseMS(p2, nsMap))).getOrElse(PhysicalReference(URI(p2.replace("https://","http://"))))
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
    } else if (s == "| Concept | URI |" || s == "| ---- | ---- |") {

    } else if (s.nonEmpty && s.startsWith("|")) {
      val p = s.tail.split('|').map(_.trim)
      if (p.length == 2) {
        val Array(con, uri) = p
        val ref = Try(LogicalReference(Path.parseMS(uri, nsMap))).getOrElse(PhysicalReference(URI(uri.replace("https://","http://"))))
        alignments += ConceptAlignment(ref,con.toLowerCase)
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

  private def readNnexus(file: File): Unit = {
    val cmds = File.read(file).split("\n").map(_.trim).filter(_.nonEmpty)
    val tmp = cmds map nnexusString
    val alignmentsCount = tmp.sum
    log(alignmentsCount + " alignments read from " + file.toString)
    log(getConcepts.length + " Concepts")
  }
  private val nnstr = """INSERT INTO "concepts" VALUES("""
  private def nnexusString(s : String) : Int = try {
    if (s.length > nnstr.length && s.startsWith(nnstr)) {
      val csv = s.drop(nnstr.length).dropRight(2).split(',').tail.init.map(_.tail.init)
      val con = (csv.head + " " + csv(1)).trim.replace("''","'")
      val uri = if (csv(5).startsWith("http://") || csv(5).startsWith("https://")) csv(5).replace("https://","http://") else "http://" + csv(5)
      val ref = Try(LogicalReference(Path.parseMS(uri, nsMap))).getOrElse(PhysicalReference(URI(uri)))
      alignments += ConceptAlignment(ref, con)
      1
    } else 0
  } catch {
    case e: Exception => 0
  }

  private def writeToFile(file: File) = File.write(file, alignments.map(_.toString).mkString("\n"))

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