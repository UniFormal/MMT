package info.kwarc.mmt.api.documents
import info.kwarc.mmt.api._
import presentation._


/**
 * An XRef represents a reference from a document to an external document part or module.
 * An XRef is semantically equivalent to copy-pasting the referenced module.
 * All documents are reprgesented as lists of XRefs no matter whether a module is given locally or remotely.
 * @param parent the containing document
 * @param target the referenced module
 * @param generated true iff the module was given directly in the document rather than referenced remotely
 */
abstract class XRef(val parent : DPath, val target : Path) extends NarrativeElement with DocumentItem {
   val path = parent
   def components = List(StringLiteral(target.toString), StringLiteral(target.last))
   def toNode : scala.xml.Node
   override def toString = "ref " +  target.toPath
}

/** reference to a document section */
class DRef(p : DPath, override val target : DPath) extends XRef(p, target) {
   val role = Role_DRef
   def toNode = <omdoc href={target.toPath}/>
}
object DRef {
   def apply(p : DPath, target : DPath, generated: Boolean = false): DRef = {
      val r = new DRef(p, target)
      if (generated) r.setOrigin(DocumentSkeleton)
      r
   }
}
/** reference to a module */
class MRef(p : DPath, override val target : MPath) extends XRef(p, target) {
   val role = Role_MRef
   def toNode = <mref target={target.toPath}/>
}
object MRef {
   def apply(p : DPath, target : MPath, generated: Boolean = false): MRef = {
      val r = new MRef(p, target) 
      if (generated) r.setOrigin(DocumentSkeleton)
      r
   }
}

class NRef(p: DPath, target: DPath) extends XRef(p, target) {
   val role = Role_DRef //TODO should get its own role
   def toNode = <nref href={target.toPath}/>
}  
