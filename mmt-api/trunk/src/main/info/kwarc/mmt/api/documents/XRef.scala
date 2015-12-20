package info.kwarc.mmt.api.documents

import info.kwarc.mmt.api._


/**
 * An NRef represents a reference from a document to an external document fragment or module.
 * An NRef is semantically equivalent to copy-pasting the referenced module.
 */
abstract class NRef extends NarrativeElement {
   val parent : DPath
   def parentOpt = Some(parent)
   val target : Path
   val name: LocalName
   val path = parent / name
   def getDeclarations = Nil
   def toNode : scala.xml.Node
   override def toString = "ref " +  target.toPath
}

/** reference to a document section */
class DRef(val parent : DPath, val name: LocalName, val target : DPath) extends NRef {
   def toNode = <omdoc href={target.toPath}/>
}

/** reference to a [[Module]] */
class MRef(val parent : DPath, val name: LocalName, val target : MPath) extends NRef {
   def toNode = <mref target={target.toPath}/>
}

object MRef {
   def apply(p : DPath, t : MPath) = new MRef(p, LocalName(t), t)
}

/** reference to a [[Declaration]]} */
class SRef(val parent : DPath, val name: LocalName, val target : GlobalName) extends NRef {
   def toNode = <sref target={target.toPath}/>
}
