package info.kwarc.mmt.api
import utils._

/** Start and end coordinates of a block
 */
case class SourceRegion(start: SourcePosition, end: SourcePosition) {
  /** inverse of SourceRegion.parse */
  override def toString = start + "-" + end
  /** l.c-l.c*/
  def twoDimString = start.twoDimString + "-" + end.twoDimString
}

object SourceRegion {
  /** parses the format start-end */
  def parse(s: String) : SourceRegion = {
      val parts = s.split("-").toList
      if (parts.length == 2) SourceRegion(SourcePosition.parse(parts(0)), SourcePosition.parse(parts(1)))
      else throw ParseError("illegal source region: " + s)
   }
}

/** Start and end two-dimensional coordinates of a block
 */
case class SourcePosition(offset: Int, line: Int, column: Int) {
  /** inverse of SourcePosition.parse */
  override def toString = offset + "." + line + "." + column
  def twoDimString = line + "." + column
}

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