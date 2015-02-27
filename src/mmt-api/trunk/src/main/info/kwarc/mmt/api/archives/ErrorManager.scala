package info.kwarc.mmt.api.archives

import java.text.SimpleDateFormat
import java.util.Date

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.{Elem, Node}

/** an [[Error]] as reconstructed from an error file */
case class BuildError(archive: Archive, target: String, path: List[String],
                      tp: String, level: Level.Level, sourceRef: parser.SourceRef,
                      shortMsg: String, longMsg: String,
                      stackTrace: List[List[String]]) {
  def toJSON: JSON = {
    val File(f) = archive / errors / target / path
    import info.kwarc.mmt.api.utils.JSONConversions._
    JSONObject(
      "id" -> "0", // to be removed later
      "errLevel" -> level.toString,
      "errType" -> tp,
      "fileName" -> f.getPath,
      "fileDate" -> new SimpleDateFormat("yyyy-MM-dd HH:mm").format(new Date(f.lastModified)),
      "target" -> target,
      "sourceRef" -> sourceRef.toString,
      "shortMsg" -> shortMsg,
      "longMsg" -> longMsg,
      "stackTrace" -> stackTrace.flatten.mkString("\n")
    )
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
  override val logPrefix = this.getClass.toString
  /** the mutable data: one ErrorMap per open Archive */
  private var errorMaps: List[ErrorMap] = Nil

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
  def loadAllErrors(a: Archive) {
    a.traverse(errors, Nil, _ => true, parallel = false) { case Current(_, target :: path) =>
      loadErrors(a, target, path)
    }
  }

  /**
   * @param a the archive
   * @param target the build target
   * @param path the file
   *             load all errors of a build target applied to a file
   */
  def loadErrors(a: Archive, target: String, path: List[String]) {
    val f = a / errors / target / path
    val node = xml.readFile(f)
    var bes: List[BuildError] = Nil
    node.child.foreach { x =>
      val (stacks, others) = x.child partition (_.label == "stacktrace")
      def getAttrs(attrs: List[String], x: Node): List[String] = {
        val as = x.attributes
        attrs map (a => as.get(a).getOrElse("").toString)
      }
      val List(tgt, srcRef, errType, shortMsg, level) = getAttrs(List("target", "sref", "type", "shortMsg", "level"), x)
      def infoMessage(msg: String) =
        log(msg + "\nFile: " + f + "\nNode: " + shortMsg)
      var lvl: Level.Level = Level.Error
      if (level.isEmpty) infoMessage("empty error level")
      else
        try {
          lvl = Level.parse(level)
        } catch {
          case e: ParseError => infoMessage(e.getMessage)
        }
      val trace: List[List[String]] = stacks.toList map (e => e.child.toList map (_.text))
      val longMsg: String = (others map (_.text)).mkString
      val elems = others filter (_.isInstanceOf[Elem])
      if (elems.nonEmpty)
        infoMessage("ignored sub-elements")
      val be = BuildError(a, target, path, errType, lvl, SourceRef.fromURI(URI(srcRef)), shortMsg, longMsg, trace)
      bes ::= be
    }
    val em = apply(a.id)
    em((target, path)) = bes.reverse
  }

  /** iterator over all errors given as (archive, target, path, error) */
  def iterator: Iterator[BuildError] = {
    errorMaps.iterator.flatMap { em =>
      em.values.iterator.flatten
    }
  }

  /** registers a [[ChangeListener]] and a [[ServerExtension]] */
  override def start(args: List[String]) {
    controller.extman.addExtension(cl)
    controller.extman.addExtension(serve)
  }

  /** adds/deletes [[ErrorMap]]s for each opened/closed [[Archive]] */
  private val cl = new ChangeListener {
    /** creates an [[ErrorMap]] for the archive and asynchronously loads its errors */
    override def onArchiveOpen(a: Archive) {
      errorMaps ::= new ErrorMap(a)
      Future {
        loadAllErrors(a)
      }
    }

    /** deletes the [[ErrorMap]] */
    override def onArchiveClose(a: Archive) {
      errorMaps = errorMaps.filter(_.archive != a)
    }
  }

  /** serves lists of [[Error]]s */
  private val serve = new ServerExtension("errors") {
    def apply(path: List[String], query: String, body: Body) = {
      val wq = WebQuery.parse(query)
      val result = iterator.filter { be =>
        true // TODO select BuildErrors according to query
      }
      val json = JSONArray(result.toList.map(_.toJSON): _*)
      Server.JsonResponse(json)
    }
  }
}
