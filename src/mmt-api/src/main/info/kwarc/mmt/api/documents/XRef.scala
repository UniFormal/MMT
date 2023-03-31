package info.kwarc.mmt.api.documents

import info.kwarc.mmt.api._


/**
 * An NRef represents a reference from a document to an external document fragment or module.
 * An NRef is semantically equivalent to copy-pasting the referenced module.
 */
abstract class NRef extends NarrativeElement {
   val feature = "reference"
   val parent : DPath
   def parentOpt = Some(parent)
   val target : Path
   val name: LocalName
   val path = parent / name
   def getDeclarations = Nil
   def toNode : scala.xml.Node
   override def toString = "ref " +  name + " -> " + target.toPath
}

/** reference to a document section */
class DRef(val parent : DPath, val name: LocalName, val target : DPath) extends NRef {
   def toNode = <dref name={name.toPath} target={target.toPath}>{getMetaDataNode}</dref>
}

/** reference to a [[info.kwarc.mmt.api.modules.Module]] */
class MRef(val parent : DPath, val name: LocalName, val target : MPath) extends NRef {
   def toNode = <mref name={name.toPath} target={target.toPath}>{getMetaDataNode}</mref>
   def nameIsTrivial = name == LocalName(target)
}

object DRef{
   def apply(parent : DPath, sub : DPath) = new DRef(parent,LocalName(sub.toPath),sub)
}

object MRef {
   def apply(p : DPath, t : MPath, generated: Boolean = false) = {
     val mr = new MRef(p, LocalName(t), t)
     if (generated) mr.setOrigin(GeneratedMRef)
     mr
   }
}

/** reference to a [[info.kwarc.mmt.api.symbols.Declaration]]} */
class SRef(val parent : DPath, val name: LocalName, val target : GlobalName) extends NRef {
   def toNode = <sref name={name.toPath} target={target.toPath}>{getMetaDataNode}</sref>
}

object SRef {
   def apply(p: DPath, t: GlobalName) = new SRef(p, LocalName(t.toMPath), t)
}
