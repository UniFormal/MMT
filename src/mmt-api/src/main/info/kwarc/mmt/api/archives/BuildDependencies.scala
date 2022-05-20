package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.api.utils.JSONArray.toList
import utils._

object Dependency {
  def parse(js : JSONArray) : Dependency = toList(js) match {
    case List(JSONString("file"),JSONString(b)) => PhysicalDependency(File(b))
    case List(JSONString("module"),JSONString(b)) => LogicalDependency(Path.parseM(b))
    case List(JSONString("doc"),JSONString(b)) => DocumentDependency(Path.parseD(b,NamespaceMap.empty))
  }
}

/** dependency of a [[QueuedTask]] */
sealed abstract class Dependency {
  /** convert to a string for toJson */
  def depkey: String
  def toJString: String
  def toJson: JSONArray = JSONArray(JSONString(depkey),JSONString(toJString))
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
  val depkey = "filebuild"
  def toJString: String = inPath.toString + " (" + key + ")"
  def getErrorFile(controller: Controller): File = (archive / errors / key / inPath).addExtension(ext ="err")
}

/** like [[FileBuildDependency]] but for a directory
  *
  * @param inPath path to file (without inDim)
  */
case class DirBuildDependency(key: String, archive: Archive, inPath: FilePath, children: List[BuildTask])
  extends BuildDependency {
  val depkey = "dirbuild"
  def toJString: String = archive.id + "/" + inPath.toString +
    " (" + key + ") " + children.map(bt => bt.inPath).mkString("[", ", ", "]")
  def getErrorFile(controller: Controller): File = getTarget(controller).getFolderErrorFile(archive, inPath)
}

sealed abstract class ResourceDependency extends Dependency

/** a dependency on a physical resource */
case class PhysicalDependency(file: File) extends ResourceDependency {
  val depkey = "file"
  def toJString: String = file.toString
}

/** a dependency on an MMT module that must be provided by building some other [[BuildTask]]
  *
  * providing the dependency typically requires some catalog to determine the appropriate [[BuildTask]]
  */
case class LogicalDependency(mpath: MPath) extends ResourceDependency {
  val depkey = "module"
  def toJString: String = mpath.toString
}

case class DocumentDependency(dpath:DPath) extends ResourceDependency {
  val depkey = "doc"
  def toJString: String = dpath.toString
}