package info.kwarc.mmt.api.documents
import info.kwarc.mmt.api._
import objects._

/* A Narration instance represents unstructured narrative content
 * such as sentences and paragraphs.
 * The tokens it contains are words, sentences, or mathematical objects. 
 */
class Narration(val tokens: List[SFDocElem]) extends DocumentItem with SFContentElemList {
   def governingPath = None
   def role = Role_Narration
   def toNode = <omtext>{tokens map (_.toNode)}</omtext>
   override def toString = tokens.map(_.toString).mkString(""," ", "")
}