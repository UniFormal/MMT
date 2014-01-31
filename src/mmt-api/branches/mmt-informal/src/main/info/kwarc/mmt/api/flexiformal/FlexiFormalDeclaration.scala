package info.kwarc.mmt.api.flexiformal

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.presentation.{XMLLiteral}
import objects._
import collection._
import symbols._
import modules._
import frontend._
import presentation._
import xml.{Node}

abstract class FlexiformalRelation

object Formalizes extends FlexiformalRelation
object isVerbalizedBy extends FlexiformalRelation


object FlexiformalDeclaration {
  def parseNarrativeObject(n : scala.xml.Node)(implicit base : Path) : NarrativeObject = n.label match {
    case "#PCDATA" => new NarrativeText(n.toString)
    case "OMOBJ" => new NarrativeTerm(Obj.parseTerm(n, base))
    case "ref" if n.prefix == "omdoc" => 
      val targetS = (n \ "@target").text
      val target = Path.parse(targetS, base)
      val text = n.child.mkString(" ")
      val self = (n \ "@self").text match {
        case "true" => true 
        case _ => false
      }
      new NarrativeRef(target, text, self)
    case _ => 
      val child = n.child.map(parseNarrativeObject)
      new NarrativeNode(n, child.toList)
  } 
  
  def parseDeclaration(n : scala.xml.Node, mpath : MPath, base : Path) : FlexiformalDeclaration = (n \ "@role").text match {
    case "plain" =>
      val r = parseNarrativeObject(n.child.head)(base)
      new PlainNarration(OMMOD(mpath), r)
    case "definition" =>
      val targetsS = (n \ "for").text.split(" ")
      val targets = targetsS.map(st => Path.parseS(st, base))
      val contentXML = n.child.find(_.label == "CMP").getOrElse(throw ParseError("Expected CMP inside definition"))
      val content = parseNarrativeObject(contentXML)(base)
      new Definition(OMMOD(mpath), targets.toList, content)    
  }
}

sealed trait NarrativeObject extends Content with ComponentContainer {
  def toNode : scala.xml.Node 
  def role = Role_NarrativeObject
  def governingPath = None
  def delete: Unit = throw ImplementationError("Cannot delete narrative object")
  def isDefined: Boolean = true
  def update(nw: ComponentContainer) = throw ImplementationError("cannot update : " + nw)
  
}

class NarrativeText(val text : String) extends NarrativeObject {
  def toNode = scala.xml.Text(text)
  def components = presentation.StringLiteral(text) :: Nil
}

class NarrativeTerm(val term : Term) extends NarrativeObject {
  def toNode = new scala.xml.Elem("om", "OMOBJ", new scala.xml.PrefixedAttribute("xmlns","om", "http://www.openmath.org/OpenMath", scala.xml.Null), scala.xml.TopScope, false, term.toNode)
  def components = term :: Nil
 
}

class NarrativeRef(val target : Path, val text : String, val self : Boolean = false) extends NarrativeObject { //self is true e.g. for subjects of definitions "A 'prime number' p is ..."
  def toNode = <omdoc:ref target={target.toPath} self={self.toString}> {text} </omdoc:ref>
  
  def components = presentation.StringLiteral(text) :: Nil
}

class NarrativeNode(val node : scala.xml.Node,val child : List[NarrativeObject]) extends NarrativeObject {
  def toNode = new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, child.map(_.toNode) :_ *)
  def components = child    
}


/* A Narration instance represents unstructured narrative content
 * such as sentences and paragraphs.
 * The tokens it contains are words, sentences, or mathematical objects. 
 */
abstract class FlexiformalDeclaration(val home : Term, val content : NarrativeObject) extends Declaration {
  def role : Role
  def getDeclarations = Nil
}

class PlainNarration(home : Term, content : NarrativeObject) extends FlexiformalDeclaration(home, content) {
  val name: info.kwarc.mmt.api.LocalName = LocalName.anonName
  def role = Role_Narration
  def toNode = 
    <flexiformal role="plain"> 
      {getMetaDataNode}
      {content.toNode}
    </flexiformal>
  def components = content :: Nil //TODO
  def getComponents = (DefComponent -> content) :: Nil 
}

class Definition(home : Term, val targets : List[GlobalName], content : NarrativeObject) extends FlexiformalDeclaration(home, content) {
  val name: info.kwarc.mmt.api.LocalName = LocalName.anonName
  def role = Role_Narrative_Def
  def toNode = 
    <flexiformal role="definition" for={targets.mkString(" ")}> 
      {getMetaDataNode}
      {content.toNode} 
    </flexiformal>  
  def components =  content :: Nil //TODO  
  def getComponents = (DefComponent -> content) :: Nil
}

class Example(home : Term, val targets : List[GlobalName], content : NarrativeObject) extends FlexiformalDeclaration(home, content) {
  val name: info.kwarc.mmt.api.LocalName = LocalName.anonName
  def role = Role_Narration
  def toNode = 
    <flexiformal role="example" for="target"> 
      {getMetaDataNode}
      {content.toNode} 
    </flexiformal>  
  def components =  XMLLiteral(content.toNode) :: Nil //TODO
  def getComponents = (DefComponent -> content) :: Nil
}

class Assertion(home : Term, val targets : List[GlobalName], content : NarrativeObject) extends FlexiformalDeclaration(home, content) {
  val name: info.kwarc.mmt.api.LocalName = LocalName.anonName
  def role = Role_Narration
  def toNode = 
    <flexiformal role="assertion" for="target"> 
      {getMetaDataNode}
      {content.toNode} 
    </flexiformal>
  def components =  XMLLiteral(content.toNode) :: Nil //TODO      
  def getComponents = (DefComponent -> content) :: Nil
}