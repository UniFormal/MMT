package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._

 /**
  * A ConstantAssignment represents an MMT assignment to a constant.<p>
  * 
  * @param home the {@link info.kwarc.mmt.api.names.Path} of the parent link
  * @param name the name of the instantiated symbol
  * @param target the term assigned to the symbol 
  */
class ConstantAssignment(val home : Morph, val name : LocalName, val target : Term) extends Assignment {
   def toNode = <conass name={name.flat}>{target.toOBJNode}</conass>
   override def toString = name + " |-> " + target.toString 
   def components = List(StringLiteral(name.flat), target)
   def role = info.kwarc.mmt.api.Role_ConAss
}

  /**
   * A DefLinkAssignment represents an MMT assignment to a definitional link.<p>
   * 
   * @param home the {@link info.kwarc.mmt.api.names.Path} of the parent link
   * @param name the name of the instantiated symbol
   * @param target the morphism assigned to the symbol 
   */
class DefLinkAssignment(val home : Morph, val name : LocalName, val target : Morph) extends Assignment {
   def toNode = name match {
      case !(IncludeStep(OMMOD(p))) => <include from={p.toPath}>{target.toOBJNode}</include>
      case !(IncludeStep(t)) =>        <include><from>{t.toOBJNode}</from>{target.toOBJNode}</include>
      case _ =>                     <strass name={name.flat}>{target.toOBJNode}</strass>
   }
   override def toString = name + " |-> " + target.toString 
   def components = List(StringLiteral(name.flat), target)
   def role = info.kwarc.mmt.api.Role_StrAss
}

class Open(val home : Morph, val name : LocalName, val as : Option[String]) extends Assignment {
   def toNode = <open name={name.flat} as={as.getOrElse(null)}/>
   override def toString = "open " + name.flat + as.map(" " + _).getOrElse("")
   def components = List(StringLiteral(name.flat), as.map(StringLiteral(_)).getOrElse(Omitted))
   def role = info.kwarc.mmt.api.Role_Open
}

/*
class Hides(parent : MPath, name : LocalPath) extends Assignment(parent, name) {
   def toNode = <hides name={name.flat}/>
   def components = List(Text(name.flat))
   def role = "hiding"
}

*/