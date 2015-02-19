package info.kwarc.mmt.api.documents

import info.kwarc.mmt.api._



/**
 * An XRef represents a reference from a document to an external document part or module.
 * An XRef is semantically equivalent to copy-pasting the referenced module.
 * All documents are represented as lists of XRefs no matter whether a module is given locally or remotely.
 * @param parent the containing document
 * @param target the referenced module
 * @param generated true iff the module was given directly in the document rather than referenced remotely
 */
abstract class XRef(val parent : DPath, val target : Path) extends NarrativeElement {
   val path = parent
   def getDeclarations = Nil
   def children = Nil
   def toNode : scala.xml.Node
   override def toString = "ref " +  target.toPath
}

/** reference to a document section */
class DRef(p : DPath, override val target : DPath) extends XRef(p, target) {
   def toNode = <dref target={target.toPath}/>
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
   def toNode = <mref target={target.toPath}/>
}
object MRef {
   def apply(p : DPath, target : MPath, generated: Boolean = false): MRef = {
      val r = new MRef(p, target) 
      if (generated) r.setOrigin(DocumentSkeleton)
      r
   }
}

/** reference to a Statement/Symbol */
class SRef(p : DPath, override val target : GlobalName) extends XRef(p, target) {
  def toNode = <sref target={target.toPath}/>
}
object SRef {
  def apply(p : DPath, target : GlobalName, generated: Boolean = false) : SRef = {
    val r = new SRef(p, target)
    if (generated) r.setOrigin(DocumentSkeleton)
    r
  }
}

class NRRef(p: DPath, target: DPath) extends XRef(p, target) {
   def toNode = <nref href={target.toPath}/>
}  

case class XRefGroup(val parent : DPath, val refs : List[XRef]) extends NarrativeElement {
  def append(ref : XRef) : XRefGroup = XRefGroup(parent, refs ::: List(ref))
  def toNode = <group> {refs.map(_.toNode)}</group>
  def getDeclarations = Nil
  def children = Nil
  def path = parent 
}
