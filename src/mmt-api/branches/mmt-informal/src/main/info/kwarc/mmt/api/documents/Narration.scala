package info.kwarc.mmt.api.documents
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.presentation.{XMLLiteral}
import objects._

trait SemiFormalXML {
  def toNode : scala.xml.Node 
}

class ObjectNode(val tokens : List[SemiFormalObject]) extends SemiFormalXML with SemiFormalObjectList {
  def toNode = <omtext>{tokens map (_.toNode)}</omtext> 
  override def toString = tokens.map(_.toString).mkString(""," ", "")
}

class XMLNode(val node : scala.xml.Node) extends SemiFormalXML {
  def toNode = node
}

class SemiFormalNode(val node : scala.xml.Node,val child : List[SemiFormalNode]) extends SemiFormalXML {
  def toNode = new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, child.map(_.toNode) :_ *)
}

/* A Narration instance represents unstructured narrative content
 * such as sentences and paragraphs.
 * The tokens it contains are words, sentences, or mathematical objects. 
 */
abstract class Narration(val text : SemiFormalXML) extends DocumentItem {
  def role = Role_Narration
  def governingPath = None
}


class Definition(val targets : List[GlobalName], text : SemiFormalXML) extends Narration(text) {
  def toNode = <definition for="target"> {text.toNode} </definition>  
  def components =  XMLLiteral(text.toNode) :: Nil //TODO  
}

class Example(val targets : List[GlobalName], text : SemiFormalXML) extends Narration(text) {
  def toNode = <example for="target"> {text.toNode} </example>  
  def components =  XMLLiteral(text.toNode) :: Nil //TODO      
}

class Assertion(val targets : List[GlobalName], text : SemiFormalXML) extends Narration(text) {
  def toNode = <assertion for="target"> {text.toNode} </assertion>
  def components =  XMLLiteral(text.toNode) :: Nil //TODO      
}