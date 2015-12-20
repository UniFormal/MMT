package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import symbols._
import objects._
import checking._
import frontend._

// this file is experimental; delete code if superseded

/**
 * An opaque declaration is a semantic entity whose precise structure is not accessible to MMT
 * but which may expose functionality that MMT can make use of.
 */
abstract class OpaqueDeclaration(format: String, raw: String) extends Declaration {
   private[api] var interpreter: OpaqueDeclarationInterpreter = null
   
   def toNode = {
      if (interpreter == null)
         <opaque name={name.toPath} format={format}>{getMetaDataNode}{raw}</opaque>
      else
         interpreter.toNode(this)
   }
   override def toString = {
      if (interpreter == null)
         raw
      else
         interpreter.toString(this)
   }
   def getComponents   = {
      if (interpreter == null)
         Nil
      else
         interpreter.getComponents(this)
   }
   def getDeclarations = {
      if (interpreter == null)
         Nil
      else
         interpreter.getDeclarations(this)
   }
}

/**
 * an extension that provides (parts of) the meaning of [[OpaqueDeclaration]]s
 */
abstract class OpaqueDeclarationInterpreter extends Extension {
   /** the format of opaque declaration this can interpret */
   def format : String
   
   def toString(oc: OpaqueDeclaration): String

   def fromNode(node: scala.xml.Node, base: Path): OpaqueDeclaration
   def toNode(oc: OpaqueDeclaration): scala.xml.Node

   def getDeclarations(oc: OpaqueDeclaration): List[ContentElement]
   def getComponents(oc: OpaqueDeclaration): List[DeclarationComponent]
}

class XMLConstant(val home: Term, val name: LocalName, node: scala.xml.Node) extends OpaqueDeclaration("xml", node.toString)

abstract class XMLConstantInterpreter extends OpaqueDeclarationInterpreter {
   def format = "xml"
}