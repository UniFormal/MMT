package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller, ShellArguments}
import info.kwarc.mmt.api.utils.AnaArgs.OptionDescrs
import info.kwarc.mmt.api.utils._

/** Shared base class for Actions that are related to Archives */
sealed abstract class ArchiveAction extends Action {}

case class ArchiveBuild(ids: List[String], dim: String, modifier: BuildTargetModifier, in: FilePath = EmptyPath) extends ArchiveAction with ActionWithErrorRecovery {
  def apply(errorCont: Option[ErrorHandler]) = {
    controller.buildArchive(ids, dim, modifier, in, errorCont)
  }
  def toParseString = s"build ${MyList(ids).mkString("[", ",", "]")} ${modifier.toString(dim)}" +
    (if (in.segments.isEmpty) "" else " " + in)
}
object ArchiveBuildCompanion extends ActionCompanion("builds a dimension in a previously opened archive", "build", "archive"){
  import Action._

  override val addKeywords = false
  def parserActual(implicit state: ActionState) = archbuild
  private def archbuild(implicit state: ActionState) =
    "build" ~> stringList ~ keyMod ~ optFilePath ^^ {
      case ids ~ km ~ in =>
        ArchiveBuild(ids, km._1, km._2, in)
    }
  private def optFilePath(implicit state: ActionState) = (str ?) ^^ { case in => FilePath(stringToList(in.getOrElse(""), "/")) }
}

case class ArchiveMar(id: String, file: File) extends ArchiveAction {
  def apply() {
    val arch = controller.backend.getArchive(id).getOrElse(throw ArchiveError(id, "archive not found"))
    arch.toMar(file)
  }
  def toParseString = s"archive $id mar $file"
}
object ArchiveMarCompanion extends  ActionCompanion("builds a dimension in a previously opened archive", "archive") {
  import Action._
  def parserActual(implicit state: ActionState) = str ~ ("mar" ~> file) ^^ { case id ~ trg => ArchiveMar(id, trg) }
}

/** helper functions of [[ArchiveAction]]s */
trait ArchiveActionHandling {self: Controller =>
  /**
    * Builds a given target from an Archive, handling the [[ArchiveBuild]] Action
    *
    */
  def buildArchive(ids: List[String], key: String, mod: BuildTargetModifier, inRaw: FilePath, errorCont: Option[ErrorHandler]) {
    ids.foreach { id =>
      val arch = backend.getArchive(id) getOrElse (throw ArchiveError(id, "archive not found"))
      // if the current directory is the archive's source directory, an initial "." refers to the current directory
      val in = if (inRaw.startsWith(".")) {
        (state.home - arch/source) match {
          case Some(p) => p / inRaw.tail
          case None => inRaw
        }
      } else inRaw
      key match {
        case "check" => arch.check(in, this)
        case "validate" => arch.validate(in, this)
        case "relational" =>
          arch.readRelational(in, this, "rel")
          arch.readRelational(in, this, "occ")
          log("done reading relational index")
        case "close" =>
          val arch = backend.getArchive(id).getOrElse(throw ArchiveError(id, "archive not found"))
          backend.closeArchive(id)
          notifyListeners.onArchiveClose(arch)
        case _ =>
          val bt = extman.getOrAddExtension(classOf[BuildTarget], key) getOrElse {
            throw RegistrationError("build target not found: " + key)
          }
          bt(mod, arch, in, errorCont)
      }
    }
    waitForBuild
  }

  def waitForBuild {
    log("waiting for build to finish")
    val pollingInterval = 1000
    while (true) {
      // if the q is empty, break the loop
      if (buildManager.doneBuilding) {
        return
      }
      // wait for sleep
      Thread.sleep(pollingInterval)
    }
  }
}
