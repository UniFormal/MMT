package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._
import utils._

/** dependency of a [[QueuedTask]] */
sealed abstract class Dependency {
  /** convert to a string for toJson */
  def toJString: String
  def toJson: JSONString = JSONString(toJString)
}

sealed abstract class BuildDependency extends Dependency {
  def key: String
  def archive: Archive
  def inPath: FilePath
  def getTarget(controller: Controller): TraversingBuildTarget = {
    controller.extman.getOrAddExtension(classOf[TraversingBuildTarget], key).getOrElse {
      throw RegistrationError("build target not found: " + key)
    }
  }
  def getErrorFile(controller: Controller): File
}

/** dependency on another [[BuildTask]]
  *
  * @param inPath path to file (without inDim)
  */
case class FileBuildDependency(key: String, archive: Archive, inPath: FilePath) extends BuildDependency {
  def toJString: String = inPath.toString + " (" + key + ")"
  def getErrorFile(controller: Controller): File = (archive / errors / key / inPath).addExtension("err")
}

/** like [[FileBuildDependency]] but for a directory
  *
  * @param inPath path to file (without inDim)
  */
case class DirBuildDependency(key: String, archive: Archive, inPath: FilePath, children: List[BuildTask])
  extends BuildDependency {
  def toJString: String = archive.id + "/" + inPath.toString +
    " (" + key + ") " + children.map(bt => bt.inPath).mkString("[", ", ", "]")
  def getErrorFile(controller: Controller): File = getTarget(controller).getFolderErrorFile(archive, inPath)
}

sealed abstract class ResourceDependency extends Dependency

/** a dependency on a physical resource */
case class PhysicalDependency(file: File) extends ResourceDependency {
  def toJString: String = file.toString
}

/** a dependency on an MMT module that must be provided by building some other [[BuildTask]]
  *
  * providing the dependency typically requires some catalog to determine the appropriate [[BuildTask]]
  */
case class LogicalDependency(mpath: MPath) extends ResourceDependency {
  def toJString: String = mpath.toString
}
