package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._
import modules._
import presentation._

 /**
  * A ConstantAssignment represents an MMT assignment to a constant.
  * 
  * @param home the [[info.kwarc.mmt.api.objects.Term]] representing the parent link
  * @param name the name of the instantiated symbol
  * @param alias the alias assigned to the instantiated symbol
  * @param target the term assigned to the symbol 
  */                                            // TODO term
class ConstantAssignment(val home : Term, val name : LocalName, val alias: Option[LocalName], val target : Option[Term]) extends Assignment {
   def toNode = <constant name={name.toPath} alias={alias.map(_.toPath).getOrElse(null)}>{getMetaDataNode :: target.toList.map(_.toOBJNode)}</constant>
   override def toString = name + target.map(" := " + _.toString).getOrElse("") +
                                  alias.map(" @ " + _.toString).getOrElse("")
   def components = List(StringLiteral(name.toPath), target.getOrElse(Omitted))
   def role = info.kwarc.mmt.api.Role_ConAss
   def toConstant = Constant(home, name, alias, None, target, None, None)
}

  /**
   * A DefLinkAssignment represents an MMT assignment to a definitional link.
   * 
   * @param home the [[info.kwarc.mmt.api.objects.Term]] representing the parent link
   * @param name the name of the instantiated symbol
   * @param target the morphism assigned to the symbol 
   */
class DefLinkAssignment(val home : Term, val name : LocalName, val from: MPath, val target : Term) extends Assignment {
   override def implicitKey = Some(from)
   def toNode = <import name={if (name.isAnonymous) null else name.toPath} domain={from.toPath}>
                     {getMetaDataNode}
                     <value>{target.toOBJNode}</value>
                </import>
   override def toString = (if (name.isAnonymous) "import " else name + " := ") + target.toString 
   def components = List(StringLiteral(name.toPath), target)
   def role = info.kwarc.mmt.api.Role_StrAss
   def toStructure = new DefinedStructure(home, name, from, target, false)
}

/** apply/unapply methods for the special case where a view includes another view */ 
object PlainViewInclude {
   /** pre: included is  view with domain from */
   def apply(home: Term, from: MPath, included: MPath) = new DefLinkAssignment(home, LocalName(ComplexStep(from)), from, OMMOD(included))
   def unapply(d: Assignment) : Option[(Term, MPath, MPath)] = {
      d match {
         case d : DefLinkAssignment => d.name match {
            case LocalName(List(ComplexStep(from))) => d.target match {
               case OMMOD(included) => Some((d.home, from, included))
               case _ => None
            }
            case _ => None
         }
         case _ => None
      }
   }
}

/*
class Hides(parent : MPath, name : LocalPath) extends Assignment(parent, name) {
   def toNode = <hides name={name.flat}>{getMetaDataNode}</hides>
   def components = List(Text(name.flat))
   def role = "hiding"
}

*/