package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.presentation._

sealed trait SFContentElem extends Content {
  def toNode : scala.xml.Node
  def toString : String 
}

sealed trait SFDocElem extends SFContentElem {
   def governingPath = None
   def role = Role_value  
}

case class SFDocNode(label : String, child : List[SFDocElem]) extends SFDocElem {
  def toNode = new scala.xml.Elem(null, label, scala.xml.Null, scala.xml.TopScope, false, child.map(_.toNode) :_*)
  def components = presentation.StringLiteral(label) :: child.flatMap(_.components)
}

sealed trait SFModuleElem extends SFDocElem 

case class SFModuleNode(label : String, child : List[SFModuleElem]) extends SFModuleElem {
  def toNode = new scala.xml.Elem(null, label, scala.xml.Null, scala.xml.TopScope, false, child.map(_.toNode) :_*)
  def components = presentation.StringLiteral(label) :: child.flatMap(_.components)
}

case class FormalDeclaration(decl : Declaration) extends SFModuleElem {
  def components = List(decl)
  def toNode = decl.toNode
}

sealed trait SFDeclElem extends SFModuleElem

case class SFDeclNode(label : String, child : List[SFDeclElem]) extends SFDeclElem {
  def toNode = new scala.xml.Elem(null, label, scala.xml.Null, scala.xml.TopScope, false, child.map(_.toNode) :_*)
  def components = presentation.StringLiteral(label) :: child.flatMap(_.components)
}

case class FormalComponent(comp : Obj) extends SFDeclElem {
  def components = List(comp)
  def toNode = comp.toNode
}

sealed trait SFObjectElem extends SFModuleElem {
     def freeVars : List[LocalName]
}

case class SFObjectNode(label : String, child : List[SFObjectElem]) extends SFObjectElem {
  def toNode = new scala.xml.Elem(null, label, scala.xml.Null, scala.xml.TopScope, false, child.map(_.toNode) :_*)
  def components = presentation.StringLiteral(label) :: child.flatMap(_.components)
  def freeVars  = child.flatMap(_.freeVars)
}

case class Text(format: String, obj: String) extends SFObjectElem {
   def components = List(StringLiteral(obj))
   def toNode = <om:text format={format}>{scala.xml.PCData(obj)}</om:text>
   override def toString = "\"" + obj + "\""
   def freeVars : List[LocalName] = Nil
}
case class XMLNode(obj: scala.xml.Node) extends SFObjectElem {
   def components = List(XMLLiteral(obj))
   def toNode = <om:node>{obj}</om:node>
   override def toString = obj.toString
   def freeVars : List[LocalName] = Nil
}
case class Formal(obj: Term) extends SFObjectElem {
   def components = List(obj)
   def toNode = obj.toNode
   override def toString = obj.toString
   def freeVars : List[LocalName] = obj.freeVars_
}

trait SFContentElemList {
   val tokens: List[SFDocElem]
   def components = tokens
   override def toString = tokens.map(_.toString).mkString("", " ", "")
   def toNodeID(pos : Position) = <om:OMSF>{tokens.map(_.toNode)}</om:OMSF>
}