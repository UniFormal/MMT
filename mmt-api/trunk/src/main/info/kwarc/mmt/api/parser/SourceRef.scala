package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import utils._

/** region in a source block
 * @param start inclusive start position
 * @param end inclusive end position
 */
case class SourceRegion(start: SourcePosition, end: SourcePosition) {
  /** inverse of SourceRegion.parse */
  override def toString = start + "-" + end
  /** l.c-l.c*/
  def twoDimString = start.twoDimString + "-" + end.twoDimString
}

/** helper object */
object SourceRegion {
  /** parses the format start-end */
  def parse(s: String) : SourceRegion = {
      val parts = s.split("-").toList
      if (parts.length == 2) SourceRegion(SourcePosition.parse(parts(0)), SourcePosition.parse(parts(1)))
      else throw ParseError("illegal source region: " + s)
   }
}

/** position in a source block; both one and two-dimensional coordinates are maintained
 * @param offset one-dimensional coordinate, -1 if omitted
 * @param line vertical two-dimensional coordinate
 * @param column horizontal two-dimensional coordinate
 * all coordinates start from 0
 */
case class SourcePosition(offset: Int, line: Int, column: Int) {
  /** inverse of SourcePosition.parse */
  override def toString = offset + "." + line + "." + column
  def twoDimString = line + "." + column
  /** a position that is i places later in the same line */
  def +(i: Int) = SourcePosition(offset + i, line, column + i)
  /** the SourceRegion of lenght 1 at this SourcePosition */
  def toRegion = SourceRegion(this, this)
}

/** helper object */
object SourcePosition {
   /** parses the format offset.line.column */
   def parse(s: String) : SourcePosition = {
      val parts = s.split("\\.").toList
      if (parts.length == 3) {
         val (o,l,c) = try {
           (parts(0).toInt, parts(1).toInt, parts(2).toInt)
         } catch {case e => throw ParseError("non-integer in source position " + s)}
         SourcePosition(o,l,c)
      }
      else throw ParseError("illegal source position: " + s)
   }
}

/** region in an identified source block
 * @param container URI of the source document
 * @param region in that document
 */
case class SourceRef(container: URI, region: SourceRegion) {
   def toURI = container ## region.toString
   override def toString = toURI.toString 
}

object SourceRef {
   def fromURI(u: URI) = {
      val container = u.copy(fragment = None)
      val reg = SourceRegion.parse(u.fragment.getOrElse(""))
      SourceRef(container, reg)
   }
   /** the theory in which the parsed metadata keys reside */
   //def metadataBase : MPath = MetaDatum.keyBase

   /** the "sourceRef" metadata key */
   def metaDataKey : GlobalName = mmt.mmtbase ? "metadata" ? "sourceRef"
}