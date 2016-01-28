package info.kwarc.mmt.api.archives

import java.text.SimpleDateFormat
import java.util.Date

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Node

object Table {
  val columns: List[String] = List(
    "errLevel",
    "errType",
    "archive",
    "group",
    "repo",
    "fileName",
    "fileLink",
    "fileDate",
    "target",
    "sourceRef",
    "shortMsg")
}

case class ErrorContent(tp: String, level: Level, sourceRef: Option[parser.SourceRef], shortMsg: String)

/** an [[Error]] as reconstructed from an error file */
case class BuildError(archive: Archive, target: String, path: FilePath, data: ErrorContent) {
  def toStrList: List[String] = {
    val f = (archive / errors / target / path).addExtension("err")
    List(data.level.toString,
      data.tp,
      archive.id,
      archive.root.up.getName,
      archive.root.getName,
      path.toString,
      f.toString,
      new SimpleDateFormat("yyyy-MM-dd HH:mm").format(new Date(f.toJava.lastModified)),
      target,
      data.sourceRef.fold("")(_.toString),
      data.shortMsg
    )
  }
}

object ErrorReader {
  /** only get errors equal or above errorLevel */
  def getBuildErrors(f: File, errorLevel: Level, log: Option[String => Unit]): List[ErrorContent] = {
    val emptyErr = f.length == 0
    val node = if (emptyErr) <errors></errors> else xml.readFile(f)
    var bes: List[ErrorContent] = Nil
    node.child.foreach { x =>
      def getAttrs(attrs: List[String], x: Node): List[String] = {
        val as = x.attributes
        attrs map (a => as.get(a).getOrElse("").toString)
      }
      val List(tgt, srcRef, errType, shortMsg, level) =
        getAttrs(List("target", "sref", "type", "shortMsg", "level"), x)
      def infoMessage(msg: String) =
        log.map(f => f(msg + "\nFile: " + f + "\nNode: " + shortMsg))
      var lvl: Level = Level.Error
      if (level.isEmpty) infoMessage("empty error level")
      else
        try {
          lvl = Level.parse(level)
        } catch {
          case e: ParseError => infoMessage(e.getMessage)
        }
      var srcR: Option[SourceRef] = None
      try {
        srcR = if (srcRef.isEmpty) None else Some(SourceRef.fromURI(URI(srcRef)))
      }
      catch {
        case e: ParseError => infoMessage(e.getMessage)
      }
      if (lvl >= errorLevel)
        bes ::= ErrorContent(errType, lvl, srcR, shortMsg)
    }
    if (node.child.isEmpty && (emptyErr || errorLevel <= Level.Force)) {
      bes ::= (if (emptyErr) ErrorContent("", 0, None, "corrupt error file")
      else ErrorContent("", 0, None, "no error"))
    }
    bes.reverse
  }
}

/**
 * maintains the errors of an archive, mapping (target,path) to error list
 */
class ErrorMap(val archive: Archive) extends mutable.HashMap[(String, List[String]), List[BuildError]]

/**
 * maintains all errors produced while running [[BuildTarget]]s on [[Archive]]s
 */
class ErrorManager extends Extension with Logger {
  self =>
  override val logPrefix = "errormanager"
  /** the mutable data: one ErrorMap per open Archive */
  private var errorMaps: List[ErrorMap] = Nil

  /** the error level to pick */
  private var level: Level = Level.Error

  /**
   * @param id the archive
   * @return the [[ErrorMap]] of the archive
   */
  def apply(id: String): ErrorMap =
    errorMaps.find(_.archive.id == id).getOrElse(throw GeneralError("archive does not exist: " + id))

  /**
   * @param a the archive
   *          load all errors of this archive
   */
  def loadAllErrors(a: Archive): Unit = {
    errorMaps ::= new ErrorMap(a)
    a.traverse(errors, EmptyPath, TraverseMode(_ => true, _ => true, parallel = false)) {
      case Current(_, FilePath(target :: path)) =>
        loadErrors(a, target, FilePath(path))
    }
  }

  /** load all errors of a build target applied to a file
    *
    * @param a      the archive
    * @param target the build target
    * @param fpath  the file
    */
  def loadErrors(a: Archive, target: String, fpath: FilePath): Unit = {
    val f = a / errors / target / fpath
    val bes = ErrorReader.getBuildErrors(f, level, Some((s: String) => log(s))).
      map(BuildError(a, target, fpath.toFile.stripExtension.toFilePath, _))
    val em = apply(a.id)
    em.put((target, fpath.segments), bes)
  }

  /** iterator over all errors given as (archive, target, path, error) */
  def iterator: Iterator[BuildError] = {
    errorMaps.iterator.flatMap { em =>
      em.values.iterator.flatten
    }
  }

  /** registers a [[ChangeListener]] and a [[ServerExtension]] */
  override def start(args: List[String]): Unit = {
    val (m, _) = AnaArgs(List(OptionDescr("level", "", IntArg, "error level to pick (0 means all)")), args)
    m.get("level").foreach { v => level = v.getIntVal - 1 }
    controller.extman.addExtension(cl)
    controller.extman.addExtension(serve)
    controller.backend.getArchives.foreach { a => loadAllErrors(a) }
  }

  override def destroy(): Unit = {
    //TODO remove cl and serve
  }

  /** adds/deletes [[ErrorMap]]s for each opened/closed [[Archive]] */
  private val cl = new ChangeListener {
    /** creates an [[ErrorMap]] for the archive and asynchronously loads its errors */
    override def onArchiveOpen(a: Archive): Unit = {
      Future {
        loadAllErrors(a)
      }
    }

    /** deletes the [[ErrorMap]] */
    override def onArchiveClose(a: Archive): Unit = {
      errorMaps = errorMaps.filter(_.archive != a)
    }

    /** reloads the errors */
    override def onFileBuilt(a: Archive, t: TraversingBuildTarget, p: FilePath): Unit = {
      Future {
        loadErrors(a, t.key, p.toFile.addExtension("err").toFilePath)
      }
    }
  }
  private val defaultLimit: Int = 100

  def serveJson(path: List[String], query: String): JSON = {
    val wq = WebQuery.parse(query)
    val ps = wq.pairs map (_._1) map (_.filter(_.isDigit)) filter (_.nonEmpty) map (_.toInt)
    val mx = if (ps.isEmpty) -1 else ps.max
    var hideQueries: List[List[String]] = Nil
    for (i <- 0 to mx) {
      var as = Table.columns map (c => wq.string(c + i))
      hideQueries ::= as
    }
    val args = Table.columns map (wq.string(_))
    val limit = wq.int("limit", defaultLimit)
    val hide = wq.boolean("hide")
    val result: Iterator[BuildError] = iterator.filter { be2 =>
      val be = be2.toStrList map (_.replace('+', ' ')) // '+' is turned to ' ' in query
      //noinspection SideEffectsInMonadicTransformation
      assert(args.length == be.length)
      args.zip(be).forall { case (a, b) => b.indexOf(a) > -1 } &&
        hideQueries.forall { hq => hq.zip(be).exists { case (a, b) => b.indexOf(a) == -1 } }
    }
    path match {
      case List("count2") => JSONArray(JSONObject("count" -> JSONInt(result.length)))
      case List("group", field) =>
        val idx = Table.columns.indexOf(field)
        assert(idx >= 0)
        val l: List[String] = result.map(be => be.toStrList(idx)).toList.sorted
        val g = l.groupBy(identity).mapValues(_.length).toList.sortBy(_._2).reverse
        JSONArray(g.take(limit).map { case (s, i) =>
          JSONObject("count" -> JSONInt(i), "content" -> JSONString(s))
        }: _*)
      case _ =>
        if (hide) JSONObject(Table.columns.zip(args map JSONString): _*)
        else JSONArray(result.toList.take(limit).map(l =>
          JSONObject(Table.columns.zip(l.toStrList map JSONString): _*)): _*)
    }
  }

  /** serves lists of [[Error]]s */
  private val serve = new ServerExtension("errors") {
    override def logPrefix = self.logPrefix

    def apply(path: List[String], query: String, body: Body) = path match {
      case List("file") =>
        val source = scala.io.Source.fromFile(query)
        val lines = try source.mkString finally source.close()
        Server.XmlResponse(lines)
      case _ => Server.JsonResponse(serveJson(path, query))
    }
  }
}
