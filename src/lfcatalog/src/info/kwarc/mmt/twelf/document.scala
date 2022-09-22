package info.kwarc.mmt.twelf

import java.io.{File, FileWriter, BufferedWriter}
import scala.xml._
import scala.collection.mutable.{HashSet, LinkedHashSet, LinkedHashMap, ListBuffer}


// ------------------------------- document-level -------------------------------

/** All information extracted from a file
  * @param url the local file address
  * @param associatedComment the optional semantic comment associated with the document
  * @param modules theories and views declared in the document
  * @param prefixes mapping from namespace prefixes to their URI
  * @param declaredNamespaces list of current namespaces declared in the document
  * @param The errors that occurred during parsing */
class Document(val url: URI, val associatedComment: Option[SemanticCommentBlock], val modules: ListBuffer[ModuleBlock],
                val prefixes: LinkedHashMap[String,URI], val declaredNamespaces: LinkedHashSet[URI], var errors: List[ParseError]) {

  /** Time, in miliseconds, when the file was last modified */
  var lastModified : Long = -1

  def toOmdoc : Elem =
    <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath" base={declaredNamespaces.head.toString}>
      {associatedComment.map(_.toOmdoc).getOrElse(Seq.empty)}
      {modules.map(_.toOmdoc)}
    </omdoc>
}


// ------------------------------- general -------------------------------


/** Start and end two-dimensional coordinates of a block
  * @param a start line
  * @param b start column
  * @param c end line
  * @param d end column */
class Position(a: Int, b: Int, c: Int, d: Int) {

  /** @param x start position (line and column)
      @param y end position (line and column) */
  def this(x: (Int,Int), y: (Int,Int)) = this(x._1, x._2, y._1, y._2)

  /** begin */
  def _1 = (a,b)
  /** end */
  def _2 = (c,d)

  /** Format: startline.startcol-endline.endcol */
  override def toString = s"${a}.${b}-${c}.${d}"
}


/** A block of information within a document
  * @param pos start and end two-dimensional coordinates */
abstract class Block(pos: Position) {
  def toOmdoc : Elem
}


/** A semantic comment
  * The "short" property is the text on the first line of the comment, if it is not preceded by a '@'
  * The "long" property is the text starting on the second line of the comment, until the first line that starts with '@'
  * Each subsequent lines must start with '@'. The first word after '@' is the key, the rest of the line is the value. */
case class SemanticCommentBlock(val comment: String, val properties: LinkedHashMap[String, String], val pos: Position) extends Block(pos) {
  def toOmdoc : Elem =
    <metadata>
      {properties.map(x => <metadatum key={x._1}>{x._2}</metadatum>)}
    </metadata>
}


/** A block with name and URI */
abstract class NamedBlock(val uri: URI, val url: URI, val name: String, val pos: Position) extends Block(pos) {
  /** the optional semantic comment associated with the block */
  var associatedComment: Option[SemanticCommentBlock] = None
}


// ------------------------------- module-level -------------------------------


/** A theory or view */
abstract class ModuleBlock(override val uri: URI, override val url: URI, override val name: String, val deps: LinkedHashSet[URI], override val pos: Position)
  extends NamedBlock(uri, url, name, pos)


/** A theory */
case class SigBlock(override val uri: URI, override val url: URI, override val name: String, val children: ListBuffer[DeclBlock], override val deps: LinkedHashSet[URI], override val pos: Position)
  extends ModuleBlock(uri, url, name, deps, pos) {
  override def toOmdoc : Elem =
    <theory name={name} uri={uri.toString}>
      {associatedComment.map(_.toOmdoc).getOrElse(Seq.empty)}
      {children.map(_.toOmdoc)}
    </theory>
}


/** A view */
case class ViewBlock(override val uri: URI, override val url: URI, override val name: String, val children: ListBuffer[AssignmentBlock], override val deps: LinkedHashSet[URI], val domain: URI, val codomain: LinkedHashSet[URI], override val pos: Position)
  extends ModuleBlock(uri, url, name, deps, pos) {
  override def toOmdoc : Elem =
    <view name={name} uri={uri.toString}>
      {associatedComment.map(_.toOmdoc).getOrElse(Seq.empty)}
      {children.map(_.toOmdoc)}
    </view>
}


// ------------------------------- symbol-level -------------------------------


/** A constant or structure assignment */
abstract class AssignmentBlock(override val uri: URI, override val url: URI, override val name: String, override val pos: Position)
  extends NamedBlock(uri, url, name, pos)

/** A constant assignment */
case class CstAssignmentBlock(override val uri: URI, override val url: URI, override val name: String, override val pos: Position)
  extends AssignmentBlock(uri, url, name, pos) {
  override def toOmdoc : Elem =
    <conass name={name} uri={uri.toString}>
      {associatedComment.map(_.toOmdoc).getOrElse(Seq.empty)}
    </conass>
}

/** A structure assignment */
case class StrAssignmentBlock(override val uri: URI, override val url: URI, override val name: String, override val pos: Position)
  extends AssignmentBlock(uri, url, name, pos) {
  override def toOmdoc : Elem =
    <strass name={name} uri={uri.toString}>
      {associatedComment.map(_.toOmdoc).getOrElse(Seq.empty)}
    </strass>
}



/** A constant or structure declaration */
abstract class DeclBlock(override val uri: URI, override val url: URI, override val name: String, override val pos: Position)
  extends NamedBlock(uri, url, name, pos)


/** A constant declaration */
case class CstDeclBlock(override val uri: URI, override val url: URI, override val name: String, override val pos: Position)
  extends DeclBlock(uri, url, name, pos) {
  override def toOmdoc : Elem =
    <constant name={name} uri={uri.toString}>
      {associatedComment.map(_.toOmdoc).getOrElse(Seq.empty)}
    </constant>
}


/** A structure declaration */
case class StrDeclBlock(override val uri: URI, override val url: URI, override val name: String, val children: ListBuffer[AssignmentBlock], val domain: Option[URI], override val pos: Position)
  extends DeclBlock(uri, url, name, pos) {
  override def toOmdoc : Elem =
    <structure name={name} uri={uri.toString}>
      {associatedComment.map(_.toOmdoc).getOrElse(Seq.empty)}
      {children.map(_.toOmdoc)}
    </structure>
}
