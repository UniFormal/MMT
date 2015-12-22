package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import frontend._
import scala.xml._

/**
 * any element that MMT does not (fully) understand
 * 
 * Extensions of MMT with Opaque knowledge consists of a pair of
 * a subclass C of this class
 * and an implementation of [[OpaqueElementInterpreter]][C]
 */
abstract class OpaqueElement extends NarrativeElement {
   def parent: DPath
   /** the format of the element */
   def format: String
   /** the raw text that was used to create it */
   def raw: NodeSeq
   
   // default implementations that may be overridden

   def toNode = <opaque format={format}>{getMetaDataNode}{raw}</opaque>
   override def toString = raw.toString
   def name = LocalName.empty
   def path = parent / name
   def parentOpt = Some(parent)
   def getDeclarations = Nil
}

/**
 * an extension that provides (parts of) the meaning of [[OpaqueElement]]s
 */
abstract class OpaqueElementInterpreter[OE <: OpaqueElement] extends FormatBasedExtension {
   /** the format of [[OpaqueElement]]s this can interpret */
   def format : String
   def isApplicable(f: String) = f == format
   
   /** work around in case the Scala type system cannot tell that an OpaqueElement has type OE
    *  pre: oe : OE
    */
   def force(oe: OpaqueElement): OE = {
      if (isApplicable(oe.format)) {
         try {oe.asInstanceOf[OE]}
         catch {case _: Exception => throw ImplementationError("opaque element has bad type")}
      } else throw LocalError("opaque element has bad format: " + format)
   }
   
   /** constructs an [[OpaqueElement]] from a raw string */
   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq): OE
}