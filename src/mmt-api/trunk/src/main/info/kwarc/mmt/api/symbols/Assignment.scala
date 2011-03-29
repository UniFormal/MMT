package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._

 /**
  * A ConstantAssignment represents an MMT assignment to a constant.<p>
  * 
  * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent link
  * @param name the name of the instantiated symbol
  * @param target the term assigned to the symbol 
  */
class ConstantAssignment(val home : Morph, val name : LocalName, val target : Term) extends Assignment {
   def toNode = <maps name={name.flat}>{target.toOBJNode}</maps>
   override def toString = name + " |-> " + target.toString 
   def components = List(StringLiteral(name.flat), target)
   def role = info.kwarc.mmt.api.Role_ConAss
}

  /**
   * A StructureAssignment represents an MMT assignment to a structure.<p>
   * 
   * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent link
   * @param name the name of the instantiated symbol
   * @param target the morphism assigned to the symbol 
   */
class StructureAssignment(val home : Morph, val name : LocalName, val target : Morph) extends Assignment {
   /**
    * computes induced assignments, compare the corresponding method in {@link info.kwarc.mmt.api.symbols.Structure}
    * @param sym a symbol of the domain theory of the instantiated structure
    * @return the induced assignment to that symbol
    */
   def applyTo(sym : Symbol) : Assignment = sym match {
      case c : Constant => new ConstantAssignment(home, name / c.name, c.toTerm * target)
      case s : Structure => new StructureAssignment(home, name / s.name, s.toMorph * target)
   }
   def toNode = <maps name={name.flat}>{target.toOBJNode}</maps>
   override def toString = name + " |-> " + target.toString 
   def components = List(StringLiteral(name.flat), target)
   def role = info.kwarc.mmt.api.Role_StrAss
}

/**
 * A LinkImport represents an MMT inclusion into a link.<p>
 *
 * @param to the link
 * @param of the included morphism
 */
case class IncludeAssignment(val home : Morph, val from : TheoryObj, val target : Morph) extends Assignment {
   val name = LocalName(IncludeStep(from))
   val role = Role_Include
   def components : List[Content] = List(target, from)
   override def toString : String = "include " + target + " : " + from  
   def toNode = target match {
      case OMMOD(p) => <include from={p.toPath}/>
      case _ => <include>{target.toOBJNode}</include>
   }
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