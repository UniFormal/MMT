package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller, ShellArguments}
import info.kwarc.mmt.api.utils.AnaArgs.OptionDescrs
import info.kwarc.mmt.api.utils._

/** Shared base class for Actions that are related to Archives */
sealed abstract class ArchiveAction extends Action {}

case class ArchiveBuild(ids: List[String], dim: String, modifier: BuildTargetModifier, in: FilePath = EmptyPath) extends ArchiveAction {
  def apply() = controller.buildArchive(ids, dim, modifier, in)
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
  private def dimension(implicit state: ActionState) = "check" | "validate" | "relational" | "close"
  private def optFilePath(implicit state: ActionState) = (str ?) ^^ { case in => FilePath(stringToList(in.getOrElse(""), "/")) }
}

case object FinishBuild extends ArchiveAction with ResponsiveAction {
  override def toParseString: String = "finish build"
  private val pollingInterval = 1000
  def apply() {
    respond("Waiting for build to finish")
    while(true) {

      // if the q is empty, break the loop
      if(controller.extman.get(classOf[BuildQueue]).headOption.forall(_.isEmpty)){
        return
      }

      // wait for sleep
      Thread.sleep(pollingInterval)
    }
  }
}
object FinishBuildCompanion extends ObjectActionCompanion(FinishBuild, "wait for build process to finish", "finish build")

case class ConfBuild(mod : String, targets : List[String], profile : String) extends ArchiveAction {
  def apply() = controller.configBuild(mod, targets, profile)
  def toParseString = "cbuild " + mod + " " + MyList(targets).mkString("[", ",", "]") + " " + profile
}
object ConfBuildCompanion extends ActionCompanion("handle building relative to a configuration file", "cbuild"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ stringList ~ str ^^ {
    case mod ~ comps ~ profile =>
      ConfBuild(mod, comps, profile)
  }
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
  def buildArchive(ids: List[String], key: String, mod: BuildTargetModifier, in: FilePath) {
    ids.foreach { id =>
      val arch = backend.getArchive(id) getOrElse (throw ArchiveError(id, "archive not found"))
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
          bt(mod, arch, in)
      }
    }
  }

  /** auxiliary function of [[configBuild]]: guess which files/folders the users wants to build
    *
    * @return archive root and relative path in it
    */
  private def collectInputs(f: File): List[(File, FilePath)] = {
    backend.resolveAnyPhysical(f) match {
      case Some(ff) =>
        // f is a file in an archive
        List(ff)
      case None =>
        // not in archive, treat f as directory containing archives
        if (f.isDirectory) f.subdirs.flatMap(collectInputs)
        else {
          logError("not a file within an archive: " + f)
          Nil
        }
    }
  }

  /** auxiliary method of [[make]] */
  private def usageOption: OptionDescrs = List(
    OptionDescr("usage", "", NoArg, "display usage message"),
    OptionDescr("help-command", "", NoArg, "help about the build target"))

  /** Runs a given profile from a config file, handling the [[ConfBuild]] target */
  def configBuild(modS : String, targets : List[String], profile : String): Unit = {
    val config = getConfig
    val mod  = modS match {
      case "Build" => BuildAll
      case "Clean" => Clean
      case _ => BuildAll
    }
    val archives = try {
      config.getProfile(profile).archives.map(aid => config.getArchive(aid))
    } catch {
      case e : Exception => config.getWritableArchives
    }
    archives foreach {a =>
      config.getArchive(a.id).formats foreach {f =>
        val imps = config.getImporters(f)
        val exps = config.getExporters(f)
        var foundChanged = false
        imps foreach { imp =>
          if (targets.contains(imp) || targets.isEmpty) foundChanged = true
          if (foundChanged) buildArchive(List(a.id), imp, mod, EmptyPath)
        }
        exps foreach { exp =>
          if (targets.contains(exp) || foundChanged || targets.isEmpty) buildArchive(List(a.id), exp, mod, EmptyPath)
        }
      }
    }
  }
}
