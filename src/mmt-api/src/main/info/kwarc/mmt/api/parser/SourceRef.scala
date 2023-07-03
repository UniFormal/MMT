package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._



/** region in a source block
 * @param start inclusive start position
 * @param end inclusive end position
 */
case class SourceRegion(start: SourcePosition, end: SourcePosition) {
  /** inverse of SourceRegion.parse */
  override def toString = start.toString + ":" + end.toString
  /** l.c-l.c*/
  def twoDimString = start.twoDimString + ":" + end.twoDimString
  /** number of characters in this region */
  def length = end.offset - start.offset + 1
  /* whether that a subregion of this */
  def contains(that: SourceRegion) = start <= that.start && that.end <= end
  /* whether that is a point in this */
  def contains(that: SourcePosition) = start <= that && that <= end

  /**
    * Subtracts child regions from the current region.
    *
    * @param children must fulfill `children.forall(this.contains)` and must be ascendingly ordered by the ordering
    *                 on [[SourcePosition]]
    * @return An ascendingly ordered list of remaining fragments of the current region (i.e., `this`).
    */
  def subtractChildRegions(children: List[SourceRegion]): List[SourceRegion] = children match {
    case range :: xs =>
      if (range.start <= this.start) {
        if (range.end < this.end) {
          SourceRegion(range.end + 1, this.end).subtractChildRegions(xs)
        } else {
          // parent got fully subsumed by range
          Nil
        }
      } else {
        if (range.start <= this.end) {
          SourceRegion(this.start, range.start - 1) :: (if (range.end < this.end) {
            SourceRegion(range.end + 1, this.end).subtractChildRegions(xs)
          } else {
            Nil
          })
        } else {
          // by ordering pre-condition on children, nothing to do anymore
          List(this)
        }
      }
    case Nil => List(this)
  }
}

/** helper object */
object SourceRegion {
  /** parses the format start-end */
  def parse(s: String) : SourceRegion = {
      val parts = if (s.contains(':')) s.split(":").toList else s.split("-").toList
      if (parts.length == 2) SourceRegion(SourcePosition.parse(parts(0)), SourcePosition.parse(parts(1)))
      else throw ParseError("illegal source region: " + s)
   }
  def ofString(s: String) = SourceRegion(SourcePosition(0,0,0),SourcePosition(s.length-1,0,s.length-1))
  val none = SourceRegion(SourcePosition.none,SourcePosition.none)
}

/**
  * Position in a source block; both one and two-dimensional coordinates are maintained.
  *
  * By convention, throughout MMT offsets in strings are counted wrt. UTF-16 encoding (matching
  * [[https://devdocs.io/openjdk~19/java.base/java/lang/string Java's encoding of strings]]).
  * Thus, all Unicode characters with code points above FFFF are counted as 2 source positions.
  * For example, in the string "a𐐀b" the grapheme "a", "𐐀", and "b" have source positions 0, 1, and 3, respectively
  * since "𐐀" is represented by a surrogate pair in UTF-16.
  *
  * Consumers of source references (e.g., MMT plugins for jEdit and VSCode) need to watch out for this.
  *
  * @param offset one-dimensional coordinate, -1 if omitted
  * @param line vertical two-dimensional coordinate
  * @param column horizontal two-dimensional coordinate
  *
  * All coordinates are zero-based.
  *
  */
case class SourcePosition(offset: Int, line: Int, column: Int) extends Ordered[SourcePosition] {
  /** inverse of SourcePosition.parse */
  override def toString = s"$offset.$line.$column"
  def twoDimString = s"$line.$column"
  /** same as twoDimString but columns and rows are counted from 1, not 0 */
  def twoDimStringFromOne = s"${line+1}.${column+1}"
  /** the position that is i places later in the same line */
  def +(i: Int) = SourcePosition(offset + i, line, column + i)
  /** the position after the string s, which starts at the current position (s may contain newlines) */
  def after(s: String) = {
     var sp = this
     s foreach {c =>
        if (c == '\n') sp = sp.nl
        else sp += 1
     }
     sp
  }
  /**
   * the position that is i places earlier in the same line
   *
   * pre: i >= column
   */
  def -(i: Int) = SourcePosition(offset - i, line, column - i)
  /** the difference between two position */
  def -(that: SourcePosition) = this.offset - that.offset
  /** the position that is 1 places later at the beginning of the the next line */
  def nl = SourcePosition(offset + 1, line + 1, 0)
  /** the SourceRegion of lenght 1 at this SourcePosition */
  def toRegion = SourceRegion(this, this)

  override def compare(that: SourcePosition): Int = {
    import scala.math.Ordered.orderingToOrdered

    if (offset != -1 && that.offset != -1) {
      offset - that.offset
    } else {
      (line, that.line) compare (column, that.column)
    }
  }
}

/** helper object */
object SourcePosition {
   /** parses the format offset.line.column */
   def parse(s: String) : SourcePosition = {
      val parts = s.split("\\.").toList
      if (parts.length == 3) {
         val (o,l,c) = try {
           (parts(0).toInt, parts(1).toInt, parts(2).toInt)
         } catch {case e : Throwable => throw ParseError("non-integer in source position " + s)}
         SourcePosition(o,l,c)
      }
      else throw ParseError("illegal source position: " + s)
   }
   val none = SourcePosition(-1,-1,-1)
}

/** region in an identified source block
 * @param container URI of the source document
 * @param region in that document
 */
case class SourceRef(container: URI, region: SourceRegion) {
   def toURI = container ## region.toString
   override def toString = toURI.toString
   /** whether that is a subregion of this */
   def contains(that: SourceRef) = container == that.container && (region contains that.region)
}

object SourceRef extends metadata.Linker[SourceRef](mmt.mmtbase ? "metadata" ? "sourceRef") {
   def fromURI(u: URI) = {
      val container = u.copy(fragment = None)
      val reg = SourceRegion.parse(u.fragment.getOrElse(""))
      SourceRef(container, reg)
   }
   def toURI(s: SourceRef) = s.toURI
   def fromString(s: String) = SourceRef.fromURI(URI(s))
   def anonymous(s: String) = {
      val reg = SourceRegion.ofString(s)
      SourceRef(URI.empty, reg)
   }
}
