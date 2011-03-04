package jomdoc.symbols
import jomdoc._
import jomdoc.objects._
import jomdoc.presentation._

/**
 * An Assignment represents an MMT assignment.<p>
 * 
 * @param parent the {@link jomdoc.names.Path} of the parent link
 * @param name the name of the instantiated symbol
 */
abstract class Assignment(parent : MPath, name : LocalPath) extends Declaration(parent, name) {
}

 /**
  * A ConstantAssignment represents an MMT assignment to a constant.<p>
  * 
  * @param parent the {@link jomdoc.names.Path} of the parent link
  * @param name the name of the instantiated symbol
  * @param target the term assigned to the symbol 
  */
class ConstantAssignment(parent : MPath, name : LocalPath, val target : Term) extends Assignment(parent, name) {
   def toNode = <maps name={name.flat}>{target.toOBJNode}</maps>
   override def toString = name + " |-> " + target.toString 
   def components = List(StringLiteral(name.flat), target)
   def role = jomdoc.Role_ConAss
}

  /**
   * A StructureAssignment represents an MMT assignment to a structure.<p>
   * 
   * @param parent the {@link jomdoc.names.Path} of the parent link
   * @param name the name of the instantiated symbol
   * @param target the morphism assigned to the symbol 
   */
class StructureAssignment(parent : MPath, name : LocalPath, val target : Morph) extends Assignment(parent, name) {
   /**
    * computes induced assignments, compare the corresponding method in {@link jomdoc.symbols.Structure}
    * @param sym a symbol of the domain theory of the instantiated structure
    * @return the induced assignment to that symbol
    */
   def applyTo(sym : Symbol) : Assignment = sym match {
      case c : Constant => new ConstantAssignment(parent, name / c.name, c.toTerm * target)
      case s : Structure => new StructureAssignment(parent, name / s.name, s.toMorph * target)
   }
   def toNode = <maps name={name.flat}>{target.toOBJNode}</maps>
   override def toString = name + " |-> " + target.toString 
   def components = List(StringLiteral(name.flat), target)
   def role = jomdoc.Role_StrAss
}

class Open(parent : MPath, name : LocalPath, val as : Option[String]) extends Assignment(parent, name) {
   def toNode = <open name={name.flat} as={as.getOrElse(null)}/>
   override def toString = "open " + name.flat + as.map(" " + _).getOrElse("")
   def components = List(StringLiteral(name.flat), as.map(StringLiteral(_)).getOrElse(Omitted))
   def role = jomdoc.Role_Open
}

/*
class Hides(parent : MPath, name : LocalPath) extends Assignment(parent, name) {
   def toNode = <hides name={name.flat}/>
   def components = List(Text(name.flat))
   def role = "hiding"
}

*/