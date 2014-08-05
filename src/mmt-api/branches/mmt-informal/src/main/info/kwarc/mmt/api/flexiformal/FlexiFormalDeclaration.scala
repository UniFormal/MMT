package info.kwarc.mmt.api.flexiformal

import info.kwarc.mmt.api._
import objects._
import collection._
import symbols._
import modules._
import frontend._
import presentation._
import xml.{Node, NamespaceBinding, Elem}
import info.kwarc.mmt.api.metadata.MetaData

object FlexiformalDeclaration {
  def parseNarrativeObject(n : scala.xml.Node)(implicit base : Path) : NarrativeObject = n.label match {
    case _ if ((n \ "@type").text == "XML") => new NarrativeXML(n)
    case "OMOBJ" => new NarrativeTerm(Obj.parseTerm(scala.xml.Utility.trim(n), base))
    case "ref" if n.prefix == "omdoc" => 
      val targetS = (n \ "@target").text
      val target = Path.parse(targetS, base)
      val objects = n.child.map(parseNarrativeObject)
      val self = (n \ "@self").text match {
        case "true" => true 
        case _ => false
      }
      new NarrativeRef(target, objects.toList, self)
    case "#PCDATA" => new NarrativeXML(n)
    case _ => 
      val child = n.child.map(parseNarrativeObject)
      new NarrativeNode(n, child.toList)
  } 
  
  def parseDeclaration(n : scala.xml.Node, name : LocalName, mpath : MPath, base : Path) : FlexiformalDeclaration = (n \ "@role").text match {
    case "plain" =>
      val r = parseNarrativeObject(n.child.head)(base)
      new PlainNarration(OMMOD(mpath), name, r)
    case "definition" =>
      val targetsS = (n \ "@for").text.split(" ")
      val targets = targetsS.map(st => Path.parseS(st, base))
      val metadataXML = n.child.find(_.label == "metadata")
      val contentXML = n.child.find(c => (c.label != "metadata") && (c.label != "#PCDATA")).get
      val metadata = metadataXML.map(MetaData.parse(_, base))
      val content = parseNarrativeObject(contentXML)(base)
      val d = new Definition(OMMOD(mpath), name, targets.toList, content)
      metadata.map(d.metadata = _)
      d
  }
  
  def cleanNamespaces(node : scala.xml.Node) : Node = node match {
    case el : Elem => 
      val scope = _cleanNamespaces(el.scope, Nil)
      new scala.xml.Elem(el.prefix, el.label, el.attributes, scope, el.minimizeEmpty, el.child : _*)
    case _ => node
  }
  
  private def _cleanNamespaces(scope : NamespaceBinding, prefixes : List[String] = Nil) : NamespaceBinding = {
    if (scope == scala.xml.TopScope) {
      scope
    } else {
      if (prefixes.contains(scope.prefix)) {
        _cleanNamespaces(scope.parent, prefixes)
      } else {
        NamespaceBinding(scope.prefix, scope.uri, _cleanNamespaces(scope.parent, scope.prefix :: prefixes))
      }
    }
  }
}

sealed trait NarrativeObject extends Content with ComponentContainer {
  def toNode : scala.xml.Node 
  def governingPath = None
  def delete: Unit = throw ImplementationError("Cannot delete narrative object")
  def isDefined: Boolean = true
  def update(nw: ComponentContainer) = throw ImplementationError("cannot update : " + nw)
  
}

class NarrativeXML(val node : scala.xml.Node) extends NarrativeObject {
  def toNode = node
  def components = Nil
  def children = Nil
}

class NarrativeTerm(val term : Term) extends NarrativeObject {
  def toNode = new scala.xml.Elem("om", "OMOBJ", new scala.xml.PrefixedAttribute("xmlns","om", "http://www.openmath.org/OpenMath", scala.xml.Null), scala.xml.TopScope, false, term.toNode)
  def components = term :: Nil
  def children = List(term)
}

class NarrativeRef(val target : Path, val objects : List[NarrativeObject], val self : Boolean = false) extends NarrativeObject { //self is true e.g. for subjects of definitions "A 'prime number' p is ..."
  def toNode = <omdoc:ref target={target.toPath} self={self.toString}> {objects.map(_.toNode)} </omdoc:ref>
  def components = objects
  def children = Nil
}

class NarrativeNode(dirtyNode : scala.xml.Node,val child : List[NarrativeObject]) extends NarrativeObject {
  val node = FlexiformalDeclaration.cleanNamespaces(dirtyNode)
  def toNode = new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, child.map(_.toNode) :_ *)
  def components = child
  def children = child
}

/* A Narration instance represents unstructured narrative content
 * such as sentences and paragraphs.
 * The tokens it contains are words, sentences, or mathematical objects. 
 */
abstract class FlexiformalDeclaration(val home : Term, val name : LocalName, val content : NarrativeObject) extends Declaration {
  def getDeclarations = Nil
  def children = List(content)
}

class PlainNarration(home : Term, name : LocalName,content : NarrativeObject) 
	extends FlexiformalDeclaration(home, name, content) {
  def toNode = 
    <flexiformal role="plain" name={name.toPath}> 
      {getMetaDataNode}
      {content.toNode}
    </flexiformal>
  def getComponents = (DefComponent -> content) :: Nil 
}

class Definition(home : Term, name : LocalName, val targets : List[GlobalName], content : NarrativeObject) 
	extends FlexiformalDeclaration(home, name, content) {
  def toNode = 
    <flexiformal role="definition" name={name.toPath} for={targets.mkString(" ")}> 
      {getMetaDataNode}
      {content.toNode} 
    </flexiformal>  
  def getComponents = (DefComponent -> content) :: Nil
}

class Example(home : Term, name : LocalName, val targets : List[GlobalName], content : NarrativeObject) 
	extends FlexiformalDeclaration(home, name, content) {
  def toNode = 
    <flexiformal role="example" name={name.toPath} for={targets.mkString(" ")}> 
      {getMetaDataNode}
      {content.toNode} 
    </flexiformal>  
  def getComponents = (DefComponent -> content) :: Nil
}

class Assertion(home : Term, name : LocalName, val targets : List[GlobalName], content : NarrativeObject) 
	extends FlexiformalDeclaration(home, name, content) {
  def toNode = 
    <flexiformal role="assertion" name={name.toPath} for={targets.mkString(" ")}> 
      {getMetaDataNode}
      {content.toNode} 
    </flexiformal>
  def getComponents = (DefComponent -> content) :: Nil
}