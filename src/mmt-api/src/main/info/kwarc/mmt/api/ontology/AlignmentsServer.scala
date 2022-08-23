package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import web._

import scala.collection.mutable
import info.kwarc.mmt.api.refactoring.ArchiveStore
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.{File, FilePath, JSON, JSONArray, JSONObject, JSONString, URI}

import scala.collection.immutable.List
import scala.util.{Success, Try}

class AddAlignments extends Extension {
  override def logPrefix: String = "checkalign"
  lazy val align = controller.extman.get(classOf[AlignmentsServer]).headOption.getOrElse {
    val a = new AlignmentsServer
    controller.extman.addExtension(a)
    a
  }

  override def start(args: List[String]): Unit = {
    var errors : List[String] = Nil
    def wrap(f : File) : List[(Alignment,String)] = try {
      align.readFile(f).map((_,f.toString))
    } catch {
      case e : Exception =>
        errors::=e.getMessage + " (in File: " + f.toString + ")"
        Nil
    }
    AlignmentsServer.findAlignmentsFolder(controller, args.headOption).foreach(filebase => {
      val fs = FilePath.getall(filebase)
      val afiles = fs.filter(f => f.getExtension.contains("align"))
      log("Files: " + afiles)
      val als = afiles flatMap wrap
      log(s"${als.filterNot(_._1.isGenerated).length} Alignments read")
      log(if (errors.nonEmpty) "Errors:\n" + errors.mkString("\n") else "No errors :)")
      log("Checking for missing symbols...")
      val syms = als.collect{
        case (fa : FormalAlignment,s) => List((fa.from.mmturi,s),(fa.to.mmturi,s))
      }.flatten
      errors = Nil
      syms foreach {s =>
        try {
          if(controller.getO(s._1).isEmpty) errors ::= s._1.toString + " (in File: " + s._2 + ")"
        } catch {
          case e : Exception => errors ::= s._1.toString + " (in File: " + s._2 + ")"
        }
      }
      log(if (errors.nonEmpty) "Missing symbols:\n" + errors.mkString("\n") else "No missing symbols :)")
    })
    controller.extman.removeExtension(this)
  }

}

class AlignmentsServer extends ServerExtension("align") {

  override val logPrefix = "align"

  private object alignments {
    private val set = mutable.HashMap[(Reference,Reference),Alignment]()

    def toList = set.values.toList
    
    def get(r : Reference, closure : Option[Alignment => Boolean] = None) : List[Alignment] = {
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

    def collect[That](pf: PartialFunction[Alignment, That]) : List[That] = toList.collect(pf)

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
  override def start(args: List[String]): Unit = {
    controller.extman.addExtension(new AlignQuery)
    AlignmentsServer.findAlignmentsFolder(controller, args.headOption).foreach(filebase => try {
      val fs = FilePath.getall(filebase)
      val afiles = fs.filter(f => f.getExtension.contains("align"))
      log("Files: " + afiles)
      afiles foreach readFile
      log(s"${alignments.toList.filterNot(_.isGenerated).length} Alignments read")
    } catch {
      case e: Exception => throw e // println(e.getMessage)
    })
  }
  override def destroy: Unit = {
    controller.extman.get(classOf[AlignQuery]) foreach { a =>
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

  def json(from : Reference => Boolean, to :Reference => Boolean) = {
    val starts = alignments.toList.collect {
      case a if from(a.from) => a.from
    }
    val all = starts.flatMap(s => alignments.get(s,Some(_=>true))).filter(a => to(a.to))
    val ret = all.map { align =>
      val fr = ("from",JSONString(align.from.toString))
      val to = ("to",JSONString(align.to.toString))
      val kvpairs = JSONArray(align.props.map {case (k,v) =>
        JSONObject((k,JSONString(v)))
      }:_*)
      val arr : List[(String,JSON)] = List(fr,to,("properties",kvpairs))
      JSONObject(arr:_*)
    }
    JSONArray(ret:_*)
  }

  def apply(request: ServerRequest): ServerResponse = {
    request.pathForExtension match {
      case "from" :: _ =>
        val path = Path.parseS(request.query, nsMap)
        val toS = if (request.query.contains("transitive=\"true\"")) alignments.get(LogicalReference(path), Some(_ => true)).map(_.to.toString)
        else alignments.get(LogicalReference(path)).map(_.to.toString)
        log("Alignment query: " + request.query)
        log("Alignments from " + path + ":\n" + toS.map(" - " + _).mkString("\n"))
        ServerResponse.TextResponse(toS.mkString("\n"))
      case "add" :: _ =>
        val str = Try(request.body.asString).getOrElse("")
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
        ServerResponse.TextResponse("Added " + addedAlignments + " alignments")
      case _ =>
        log(request.pathForExtension.toString) // List(from)
        log(request.query) // an actual symbol path
        log(request.body.toString) //whatever
        ServerResponse.TextResponse("")
    }
  }

  def getAll = alignments.toList

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
    val direction = allpars.find(p => p._1 == "direction")
    val pars = allpars.filterNot(p => p._1 == "direction" || p._1 == "arguments")
    lazy val p1P = Path.parseMS(p1, nsMap)
    lazy val p2P = Path.parseMS(p2, nsMap)
    if (direction.isDefined) {
      if (allpars.exists(_._1 == "arguments")) {
        var args: List[(Int, Int)] = Nil
        val item = allpars.find(_._1 == "arguments").get
        var read = item._2.trim
        while (read != "") read match {
          case argls(i, j, r) =>
            args ::= (i.toInt, j.toInt)
            read = r.trim
          case _ => throw new Exception("Malformed argument pair list: " + item._2)
        }
        val ret = if (direction.get._2 == "forward")
          ArgumentAlignment(p1P, p2P, false, args, pars)
        else if (direction.get._1 == "backward")
          ArgumentAlignment(p2P, p1P, false, args, pars)
        else
          ArgumentAlignment(p1P, p2P, true, args, pars)
        ret
      } else if (allpars.exists(_._1 == "dotoperator")) {
        val dot = utils.listmap(allpars, "dotoperator").get
        val dotP = Path.parseS(dot, nsMap)
        DereferenceAlignment(p1P, p2P, dotP)
      } else {
        val ret = if (direction.get._2 == "forward")
          SimpleAlignment(p1P, p2P, false, pars)
        else if (direction.get._2 == "backward")
          SimpleAlignment(p2P, p1P, false, pars)
        else if (direction.get._2 == "both")
          SimpleAlignment(p1P, p2P, true, pars)
        else throw new Exception("unknown alignment direction: " + direction.get._2)
        ret
      }
    } else {
      val from: URIReference = Try(LogicalReference(p1P)).getOrElse(PhysicalReference(URI(p1)))
      val to: URIReference = Try(LogicalReference(p2P)).getOrElse(PhysicalReference(URI(p2)))
      val ret = InformalAlignment(from, to)
      ret.props = pars
      ret
    }
  }

  private def processString(s: String): List[Alignment] = {
    // val param = """(.+)\s*=\s*\"(.+)\"\s*(.*)""".r
    var dones : List[Alignment] = Nil
    object param {
      def unapply(s : String) : Option[(String,String,String)] = {
        var rest = s.trim
        var eqindex = rest.indexOf("=\"")
        if (eqindex == -1) return None
        val key = rest.substring(0,eqindex)
        rest = rest.substring(eqindex + 2)

        eqindex = rest.indexOf("\"")
        if (eqindex == -1) return None
        val value = rest.substring(0,eqindex)
        rest = rest.substring(eqindex + 1)
        Some((key,value,rest))
      }
    }
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
        dones ::= ConceptAlignment(ref,con)
      }
    } else if (s.nonEmpty && s.startsWith("<")) {
      val p = s.trim.drop(1).dropRight(1).split('|').map(_.trim)
      dones ::= ConceptPair(p.head,p.tail.head)
    } else if (s.nonEmpty && !s.startsWith("//")) {
      val (p1, p2) = {
        val s = rest.split("""\s""").map(_.trim).filter(_.nonEmpty)
        rest = rest.substring(s.head.length).trim.substring(s(1).length).trim
        (s.head, s(1))
      }
      var pars: List[(String, String)] = Nil
      while (rest != "") rest match {
        case param(key, value, r) =>
          pars ::= (key, value)
          rest = r.trim
        case _ => throw new Exception("Malformed alignment: " + s)
      }
      dones ::= makeAlignment(p1, p2, pars)
    }
    dones foreach alignments.+=
    dones
  }

  def readFile(file: File) = {
    val cmds = File.read(file).split("\n").map(_.trim).filter(_.nonEmpty)
    val tmp = cmds flatMap processString
    val alignmentsCount = tmp.length
    log(s"${alignmentsCount} alignments read from ${file.toString}")
    tmp.toList
  }

  def writeToFile(file: File) = File.write(file, getAsString)

  def getAsString = alignments.filter(!_.isGenerated).map(_.toString).mkString("\n")

  // TODO needs reworking
  private def readJSON(file: File): Unit = {
    ???
  }

  /** translation along alignments */
  private class AlignQuery extends QueryFunctionExtension("align", ElementQuery(PathType), new SetQuery(List(StringType))) {
    def evaluate(argument: BaseType, params: List[String]) = {
      log("Evaluating align query")
      log(argument.toString)
      params.foreach(p => log(p.toString))
      val o = argument match {
        case p : ContentPath => p
        case _       => throw ImplementationError("evaluation of ill-typed query")
      }
      // controller.extman.get(classOf[AlignmentsServer])
      ???
    }
  }

}

object AlignmentsServer {
  /** finds the alignments folder given an optional path */
  def findAlignmentsFolder(controller: Controller, path: Option[String]): Option[File] = {
    if(path.isDefined){ return Some(File(path.get)) }
    controller.getMathHub.flatMap {mh => mh.getEntry("alignments/Public").map(_.root)}
  }
}