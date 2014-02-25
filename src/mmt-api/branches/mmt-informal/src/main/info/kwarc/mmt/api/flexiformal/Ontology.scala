package info.kwarc.mmt.api.flexiformal

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.ontology._

sealed abstract class FlexiformalBinary(desc : String, backwardsDesc : String) 
    extends GenericBinary(desc, backwardsDesc) {
  def apply(subj : Path, obj : FragPath) : FlexiformalRelation = {
    FlexiformalRelation(this, subj, obj)
  }
}

/** helper methods for Binary items */
object FlexiformalBinary {
   val all = List()
   def parse(s: String) : FlexiformalBinary = all.find(_.toString == s) match {
      case Some(i) => i
      case _ => throw ParseError("flexiformal binary predicate expected, found: " + s)
   }
}

/**
 * An object of type FlexiformalRelation represents a binary predicate in the flexiformal ABox.
 * It differs from a relation in that the subject is a fragment path, allowing relations involving 
 * content fragments
 */
case class FlexiformalRelation(dep : FlexiformalBinary, subj : Path, obj : FragPath) extends RelationalElement {
   val path = subj
   def toNode = <relation subject={subj.toPath} predicate={dep.toString} obj-path={obj.path.toPath} obj-pos={obj.fragment.toString}/>
   override def toString = subj.toString + " " + dep.toString + " " + obj.path.toPath + " " + obj.fragment.toString
   def toPath = dep.toString + " " + subj.toPath + " " + obj.path.toPath + " " + obj.fragment.toString
}