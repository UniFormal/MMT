package info.kwarc.mmt.api.flexiformal

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.Position

/**
 * A path to a fragment of a MMT content element
 * @param path the path to the containing content element
 * @param fragment the position of the fragment inside the syntax tree of the containing element
 */
case class FragPath(path : Path, fragment : Position) {
  override def toString = path.toPath + "#" + fragment.toString 
  //if this is a simple path return it, otherwise None 
  def toPathO : Option[Path] = fragment.indices match {
    case Nil => Some(path)
    case _ => None
  }
  def isPath = fragment.indices.isEmpty
}

object FragPath {
  def apply(path : Path) : FragPath = FragPath(path, Position(Nil))
}