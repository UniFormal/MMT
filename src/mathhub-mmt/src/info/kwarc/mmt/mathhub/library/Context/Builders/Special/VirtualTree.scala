package info.kwarc.mmt.mathhub.library.Context.Builders.Special

import info.kwarc.mmt.api.archives.MathHub
import info.kwarc.mmt.api.frontend.{Controller}

/**
  * Represents a "virtual" tree inserted directly into the root of archives.
  *
  * Nodes are identified by a (archive, path) pair, where archive is the ID of the current archive
  * and path is a list of strings.
  *
  * Each node is either an "inner node" or a "leaf node".
  *
  * An inner node may have children, a leaf node may not.
  * Paths to child nodes consist of the path to the parent node and the name of the child node.
  *
  * Leaf Nodes should return opaque content, typically of html format.
  */
abstract class VirtualTree(
                            protected val controller: Controller,
                            protected val mathhub: MathHub) {
  // internal identifier used in virtual URLs
  val key: String

  // used in the interface
  val displayName: String

  /**
    * Checks if this VirtualTree is applicable to the provided archive.
    * Applicable means that it is displayed on the archive page.
    */
  def applicable(archive: String): Boolean

  /** checks if a node exists */
  def exists(archive: String, path: List[String]): Boolean

  /** Return the children of the given node */
  def children(archive: String, path: List[String]): List[String]

  /** get the displayed name for a node */
  def getName(archive: String, path: List[String]): String = path.lastOption.getOrElse(displayName)

  /** Check if a node is an inner node */
  def isLeaf(archive: String, path: List[String]): Boolean

  /**
    * getLeafContent returns opaque content of the provided leaf node.
    * It should return a pair of (format, content).
    *
    * Format is typically "html".
    */
  def getLeafContent(archive: String, path: List[String]): (String, String)
}
