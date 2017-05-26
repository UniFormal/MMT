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
   val feature = "opaque-element"
   def parent: DPath
   /** the format of the element */
   def format: String
   /** the raw text that was used to create it */
   def raw: NodeSeq
   
   // default implementations that may be overridden

   def toNode = <opaque format={format}>{getMetaDataNode}{raw}</opaque>
   override def toString = raw.toString
   lazy val name = LocalName("") / "opaque" / raw.hashCode.toString //TODO this is not always unique  
   def path = parent / name
   def parentOpt: Some[DPath] = Some(parent)
   def getDeclarations = Nil
}

/**
 * an extension that provides (parts of) the meaning of [[OpaqueElement]]s
 *
 * Instances of this class must be coupled with a subclass OE <: [[OpaqueElement]].
 * Methods of an OpaqueElementInterpreter will be passed to and must return only instances of OE, never of other OpaqueElements.
 * 
 * OE is not a type parameter of OpaqueElementInterpreter because that is less helpful than one may think.
 * Interpreters are anyway chosen at run time based on the format so that the type information can rarely be exploited
 * and often requires casting anyway. 
 * 
 * Therefore, all methods work simply with [[OpaqueElement]].
 * But MMT and all oi:OpaqueElementInterpreter must respect the following invariant:
 * If oe:OpaqueElement and oi.isApplicable(oe.format), then oe.isInstanceOf[oi.OE].
 * In particular, the 'downcast' method can be used to refine the type of inputs passed to this class.
 */
abstract class OpaqueElementInterpreter extends FormatBasedExtension {
   type OE <: OpaqueElement
      
   /** the format of [[OpaqueElement]]s this can interpret */
   def format : String
   def isApplicable(f: String) = f == format
   
   /**
    * Casts an opaque element to type OE.
    * Guaranteed to be safe on all input passed to this class.
    */
   protected def downcast(oe: OpaqueElement): OE = {
      if (isApplicable(oe.format)) {
         try {oe.asInstanceOf[OE]}
         catch {case _: Exception => throw ImplementationError("opaque element has bad type")}
      } else throw LocalError("opaque element has bad format: " + format)
   }
   
   /** constructs an [[OpaqueElement]] from a raw string */
   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq): OE
}