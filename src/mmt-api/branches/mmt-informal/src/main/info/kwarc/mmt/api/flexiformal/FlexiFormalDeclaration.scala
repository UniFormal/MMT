package info.kwarc.mmt.api.flexiformal

import info.kwarc.mmt.api._
import objects._
import collection._
import symbols._
import modules._
import frontend._
import presentation._
import info.kwarc.mmt.api.metadata.MetaData
import xml.{Node, NamespaceBinding, Elem}

/*
abstract class TermExtension {
   def subobjects: List[(Context,Term)]
}

case class OMExtended[E <: TermExtension](e: E) {
   def subobjects = e.subobjects
}

abstract class TermExtension2[u <: GerneralTermType] extends Extension {
   def subobjects(x:u): List[(Context,Term)]
}

case class OMExtended2[u](E: TermExtension2[u], e: u) {
   def subobjects = E.subobjects(e)
}
*/

object FlexiformalDeclaration {
  def parseObject(n : scala.xml.Node)(implicit base : Path) : FlexiformalObject = n.label match {
    case _ if ((n \ "@type").text == "XML") => new FlexiformalXML(n)
    case "OMOBJ" => new FlexiformalTerm(Obj.parseTerm(scala.xml.Utility.trim(n), base))
    case "ref" if n.prefix == "omdoc" => 
      val targetS = (n \ "@target").text
      val target = Path.parse(targetS, base)
      val objects = n.child.map(parseObject)
      val self = (n \ "@self").text match {
        case "true" => true 
        case _ => false
      }
      val tp = (n \ "tp").text match {
        case "" => None
        case s => Some(ontology.Binary.parse(s))
      }
      new FlexiformalRef(target, tp, objects.toList, self)
    case "#PCDATA" => new FlexiformalXML(n)
    case _ => 
      val child = n.child.map(parseObject)
      new FlexiformalNode(n, child.toList)
  } 
  
  def parseDeclaration(n : scala.xml.Node, name : LocalName, mpath : MPath, base : Path) : FlexiformalDeclaration = (n \ "@role").text match {
    case "plain" =>
      val r = parseObject(n.child.head)(base)
      new PlainNarration(OMMOD(mpath), name, None, r)
    case "definition" =>
      val targetsS = (n \ "@for").text.split(" ")
      val targets = targetsS.map(st => Path.parseS(st, base))
      val metadataXML = n.child.find(_.label == "metadata")
      val contentXML = n.child.find(c => (c.label != "metadata") && (c.label != "#PCDATA")).get
      val metadata = metadataXML.map(MetaData.parse(_, base))
      val content = parseObject(contentXML)(base)
      val d = new Definition(OMMOD(mpath), name, targets.toList, None, content)
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

sealed trait FlexiformalObject extends Content with ComponentContainer {
  def toNode : scala.xml.Node 
  def governingPath = None
  def delete: Unit = throw ImplementationError("Cannot delete narrative object")
  def isDefined: Boolean = true
  def update(nw: ComponentContainer) = throw ImplementationError("cannot update : " + nw)
  
}

class FlexiformalXML(val node : scala.xml.Node) extends FlexiformalObject {
  def toNode = node
  def components = Nil
  def children = Nil
}

class FlexiformalTerm(val term : Term) extends FlexiformalObject {
  def toNode = new scala.xml.Elem("om", "OMOBJ", new scala.xml.PrefixedAttribute("xmlns","om", "http://www.openmath.org/OpenMath", scala.xml.Null), scala.xml.TopScope, false, term.toNode)
  def components = term :: Nil
  def children = List(term)
}

class FlexiformalRef(val target : Path, val tp : Option[ontology.Binary], val objects : List[FlexiformalObject], val self : Boolean = false) extends FlexiformalObject { //self is true e.g. for subjects of definitions "A 'prime number' p is ..."
  def toNode = <omdoc:ref target={target.toPath} tp={tp.map(_.toString).getOrElse("")} self={self.toString}> {objects.map(_.toNode)} </omdoc:ref>
  def components = objects
  def children = Nil
}

class FlexiformalNode(dirtyNode : scala.xml.Node,val child : List[FlexiformalObject]) extends FlexiformalObject {
  val node = FlexiformalDeclaration.cleanNamespaces(dirtyNode)
  def toNode = new scala.xml.Elem(node.prefix, node.label, node.attributes, node.scope, false, child.map(_.toNode) :_ *)
  def components = child
  def children = child
}

/* A Narration instance represents unstructured narrative content
 * such as sentences and paragraphs.
 * The tokens it contains are words, sentences, or mathematical objects. 
 */
abstract class FlexiformalDeclaration(val home : Term, val name : LocalName, val tp : Option[FlexiformalObject], val df : FlexiformalObject) extends Declaration {
  def getDeclarations = Nil
  def children = df :: tp.toList
}

class PlainNarration(home : Term, name : LocalName, tp : Option[FlexiformalObject], df : FlexiformalObject) 
	extends FlexiformalDeclaration(home, name, tp, df) {
  def toNode = 
    <flexiformal role="plain" name={name.toPath}> 
      {getMetaDataNode}
      {df.toNode}
    </flexiformal>
  def getComponents = tp.map(x => List(TypeComponent -> x)).getOrElse(Nil) ::: (DefComponent -> df) :: Nil 
}

class Definition(home : Term, name : LocalName, val targets : List[GlobalName], tp : Option[FlexiformalObject], df : FlexiformalObject) 
	extends FlexiformalDeclaration(home, name, tp, df) {
  def toNode = 
    <flexiformal role="definition" name={name.toPath} for={targets.mkString(" ")}> 
      {getMetaDataNode}
      {df.toNode} 
    </flexiformal>  
  def getComponents = tp.map(x => List(TypeComponent -> x)).getOrElse(Nil) ::: (DefComponent -> df) :: Nil 
}

class Example(home : Term, name : LocalName, val targets : List[GlobalName], tp : Option[FlexiformalObject], df : FlexiformalObject) 
	extends FlexiformalDeclaration(home, name, tp, df) {
  def toNode = 
    <flexiformal role="example" name={name.toPath} for={targets.mkString(" ")}> 
      {getMetaDataNode}
      {df.toNode} 
    </flexiformal>  
  def getComponents = tp.map(x => List(TypeComponent -> x)).getOrElse(Nil) ::: (DefComponent -> df) :: Nil 
}

class Assertion(home : Term, name : LocalName, val targets : List[GlobalName], tp : Option[FlexiformalObject], df : FlexiformalObject) 
	extends FlexiformalDeclaration(home, name, tp, df) {
  def toNode = 
    <flexiformal role="assertion" name={name.toPath} for={targets.mkString(" ")}> 
      {getMetaDataNode}
      {df.toNode} 
    </flexiformal>
  def getComponents = tp.map(x => List(TypeComponent -> x)).getOrElse(Nil) ::: (DefComponent -> df) :: Nil 
}