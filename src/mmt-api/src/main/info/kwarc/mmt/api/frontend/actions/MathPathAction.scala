package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.{ErrorLogger, GeneralError}
import info.kwarc.mmt.api.backend.{LocalCopy, LocalSystem}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.ParsingStream
import info.kwarc.mmt.api.utils.{File, FilePath, URI}

/** Shared base class for Actions updating the mathpath */
sealed abstract class MathPathAction extends ActionImpl {}

/** add a catalog entry for the local file system
  *
  * All URIs of the form file:///SUFFIX are mapped to SUFFIX
  *
  * concrete syntax: mathpath local
  */
case object Local extends MathPathAction {
  def apply(implicit controller: Controller): Unit = {
    val currentDir = new java.io.File(".").getCanonicalFile
    val b = URI.fromJava(currentDir.toURI)
    controller.backend.addStore(LocalSystem(b))
  }
  def toParseString = "mathpath local"
}
object LocalCompanion extends ActionObjectCompanionImpl[Local.type]("add a catalog entry for the local file system", "mathpath local")

/** add catalog entries for a set of local copies, based on a file in Locutor registry syntax */
case class AddArchive(folder: java.io.File) extends MathPathAction {
  def apply(implicit controller: Controller): Unit = controller.addArchive(folder)
  def toParseString = s"mathpath archive $folder"
}
object AddArchiveCompanion extends ActionCompanionImpl[AddArchive]("add catalog entries for a set of local copies", "mathpath archive") {
  import Action._
  override val addKeywords = false
  def parserActual(implicit state: ActionState) = ("mathpath archive" | "archive add") ~> file ^^ { f => AddArchive(f) }
}

case class AddMathPathFS(uri: URI, file: File) extends MathPathAction {
  def apply(implicit controller: Controller) : Unit = {
    val lc = new LocalCopy(uri.schemeNull, uri.authorityNull, uri.pathAsString, file)
    controller.backend.addStore(lc)
  }
  def toParseString = s"mathpath fs $uri $file"
}
object AddMathPathFSCompanion extends ActionCompanionImpl[AddMathPathFS]("add catalog entry for a local directory", "mathpath fs") {
  import Action._
  def parserActual(implicit state: ActionState) = uri ~ file ^^ { case u ~ f => AddMathPathFS(u, f) }
}

case class AddMathPathJava(javapath: File) extends MathPathAction {
  def apply(implicit controller: Controller) = controller.backend.openRealizationArchive(javapath)
  def toParseString = "mathpath java " + javapath
}
object AddMathPathJavaCompanion extends ActionCompanionImpl[AddMathPathJava]("add catalog entry for realizations in Java", "mathpath java") {
  import Action._
  def parserActual(implicit state: ActionState) = file ^^ { f => AddMathPathJava(f) }
}

case class Read(file: File, interpret: Boolean) extends MathPathAction {
  def apply(implicit controller: Controller): Unit = {
    import controller._
    if (!file.isFile)
      throw GeneralError("file not found: " + file)
    val ps = backend.resolvePhysical(file) match {
      case Some((arch, p)) => ParsingStream.fromSourceFile(arch, FilePath(p))
      case None => ParsingStream.fromFile(file)
    }
    read(ps, interpret, mayImport = true)(new ErrorLogger(controller.report))
    ps.stream.close
  }
  def toParseString = s"${if (interpret) "interpret" else "read"} $file"
}
object ReadCompanion extends ActionCompanionImpl[Read]("read a file containing MMT in OMDoc syntax", "read", "interpret") {
  import Action._
  override val addKeywords = false
  def parserActual(implicit state: ActionState) = read | interpret
  private def read(implicit state: ActionState) = "read" ~> file ^^ { f => Read(f, interpret=false)}
  private def interpret(implicit state: ActionState) = "interpret" ~> file ^^ { f => Read(f, interpret=true)}
}

/** implements helper functions of [[MathPathAction]]s */
trait MathPathActionHandling {
  self: Controller =>

  /** add an archive plus its optional classpath and notify listeners, handling [[AddArchive]] */
  def addArchive(root: File): List[Archive] =  {
    val archs = backend.openArchive(root)
    archs.foreach { a =>
      a.properties.get("classpath").foreach { cp =>
        backend.openRealizationArchive(a.root / cp)
      }
      notifyListeners.onArchiveOpen(a)
    }
    archs
  }
}