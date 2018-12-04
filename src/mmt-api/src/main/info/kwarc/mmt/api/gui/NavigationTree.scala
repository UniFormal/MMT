package info.kwarc.mmt.api.gui

import info.kwarc.mmt.api._
import parser._
import utils._

/** abstraction for rendering trees in different UIs */
abstract class NavigationTreeImplementation[T <: NavigationTree, N <: NavigationTreeElement] {
  /** makes a UI-tree from the MMT-info about it
   *  must satisfy makeTree(i).getInfo == i
   */
  def makeTree(info: NavigationTreeInfo): T
  /** makes a UI-node from the MMT-info about it
   *  must satisfy makeNode(i).getInfo == i
   */
  def makeNode(info: NavigationTreeElementInfo): N
  
  /** retrieves the lowest node of the tree whose region covers an offset */
  def getElementAtOffset(tree: T, offset: Int): N
  /** retrieves the lowest node of the tree whose region covers a range */
  def getElementAtRange(tree: T, from: Int, to: Int): N
}

/** an abstraction for the classes used by UI-specific tree implementations */
trait NavigationTree {
  def getInfo: NavigationTreeInfo
} 

/** an abstraction for the classes used by UI-specific tree implementations */
trait NavigationTreeElement {
  def getInfo: NavigationTreeElementInfo
} 

/** global information about a [[NavigationTree]] */
case class NavigationTreeInfo(src: File)

/** the semantic information to be stored in the nodes of a [[NavigationTreeElement]] */
/* TODO all (currently jEdit-specific) MMTAsset classes should become subclasses of this class
 * jEdit only needs one wrapper class around this one that extends SourceAsset
 * the makeNode method of `object SidekickTree extends NavigationTreeImplementation[SidekickParsedData,SourceAsset]` would apply the wrapper.
 * The parse method of MMTSidekick can be implemented generically in terms of an arbitrary NavigationTreeImplementation.
 */
abstract class NavigationTreeElementInfo {
  def region: SourceRegion
}

