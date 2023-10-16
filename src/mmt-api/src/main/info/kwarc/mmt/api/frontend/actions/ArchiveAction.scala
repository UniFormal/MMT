package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller, ShellArguments}
import info.kwarc.mmt.api.utils.AnaArgs.OptionDescrs
import info.kwarc.mmt.api.utils._

/** Shared base class for Actions that are related to Archives */
sealed abstract class ArchiveAction extends Action {}

case class ArchiveNew(folder: File) extends ArchiveAction {
  def apply() = {
    controller.newArchive(folder)
  }
  def toParseString = s"archive new $folder"
}
object ArchiveNewCompanion extends ActionCompanion("creates an empty archive", "archive new") {
  import Action._
  def parserActual(implicit state: ActionState) = file ^^ {case f => ArchiveNew(f)}
}

case class ArchiveBuild(ids: List[String], dim: String, modifier: BuildTargetModifier, in: FilePath = EmptyPath) extends ArchiveAction with ActionWithErrorRecovery {
  def apply(errorCont: Option[ErrorHandler]) = {
    controller.buildArchive(ids, dim, modifier, in, errorCont)
  }
  def toParseString = {
    val archives = if (ids.length == 1) ids.head else MyList(ids).mkString("[", ",", "]")
    s"build $archives ${modifier.toString(dim)}" +
      (if (in.segments.isEmpty) "" else " " + in)
  }
}
object ArchiveBuildCompanion extends ActionCompanion("builds a dimension in a previously opened archive", "build", "archive"){
  import Action._
  override val addKeywords = false
  def parserActual(implicit state: ActionState) = archbuild
  private def archbuild(implicit state: ActionState) = {
    "build" ~> stringList ~ keyMod ~ optFilePath ^^ {
      case ids ~ km ~ in => ArchiveBuild(ids, km._1, km._2, in)
    }
  }
  private def optFilePath(implicit state: ActionState) = (str ?) ^^ { case in => FilePath(stringToList(in.getOrElse(""), "/")) }
}

case class ArchiveMar(id: String, file: File) extends ArchiveAction {
  def apply(): Unit = {
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
  /** creates a new empty archive */
  def newArchive(folder: File) = {
    log("creating archive at " + folder)
    val manifestLocation = folder / "MANIFEST.MF"
    if (manifestLocation.exists) log("overriding existing archive")
    val id = folder.getName
    val manifest = List(
      "// a unique identifier for this archive",
      s"id: $id",
      "// a unique URI to be used as the root of all documents in this archive",
      s"narration-base: http://mmt.omdoc.org/documents/$id",
      "// the URI that is used as the default namespace for every file in this archive",
      s"ns: http://mmt.omdoc.org/modules/$id",
      "dependencies: MMT/urtheories"
    )
    File.WriteLineWise(manifestLocation, manifest)
    val hw = MMTSystem.getResourceAsString("newArchive/helloworld.mmt")
    File.write(folder / "source" / "helloworld.mmt", hw)
    val bld = MMTSystem.getResourceAsString("newArchive/build.msl")
    File.write(folder / "build.msl", bld)
  }

  /**
    * Builds a given target from an Archive, handling the [[ArchiveBuild]] Action
    *
    */
  def buildArchive(ids: List[String], key: String, mod: BuildTargetModifier, inRaw: FilePath, errorCont: Option[ErrorHandler]): Unit = {
    val archs = ids.flatMap {s =>
      if (s == ".") {
        // '.' refers to the containing archive (state.home is set to containing file while processing msl files)
        backend.resolveAnyPhysical(state.home) map {
          case (archRoot,_) => backend.openArchive(archRoot).head
        } match {
          case None => throw ArchiveError(s, "archive not found")
          case Some(a) => List(a)
        }
      } else {
        backend.getArchive(s) match {
          case None =>
            backend.getArchives.filter(_.id.startsWith(s + "/")) match {
              case Nil =>
                throw ArchiveError(s, "archive not found")
              case o => o
            }
          case o => o.toList
        }
      }
    }.distinct
    archs.foreach { arch =>
      // if the current directory is in the archive's source directory, an initial "." refers to the current directory
      val in = if (inRaw.segments.headOption contains ".") {
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
          val ar = backend.getArchive(arch.id).getOrElse(throw ArchiveError(arch.id, "archive not found"))
          // ^ is it intentional that the archive is retrieved from backend a second time?
          backend.closeArchive(ar.id)
          notifyListeners.onArchiveClose(ar)
        case _ =>
          val bt = extman.getOrAddExtension(classOf[BuildTarget], key) getOrElse {
            throw RegistrationError("build target not found: " + key)
          }
          bt(mod, arch, in, errorCont)
      }
    }
    waitForBuild
  }

  def waitForBuild: Unit = {
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
