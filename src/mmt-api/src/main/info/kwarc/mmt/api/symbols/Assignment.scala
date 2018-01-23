package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._
import modules._
import presentation._
import notations._

 /**
  * An MMT assignment to a constant is a special case of a Constant.
  *
  * @param home the parent link
  * @param name the name of the instantiated symbol
  * @param alias the alias assigned to the instantiated symbol
  * @param target the term assigned to the symbol
  */
object ConstantAssignment {
   def apply(home : Term, name : LocalName, alias: List[LocalName], target : Option[Term]) =
      Constant(home, name, alias, None, target, None, NotationContainer())
}

  /**
   * An MMT assignment to a definitional link is a special case of a DefinedStructure.
   *
   * @param home the parent link
   * @param name the name of the instantiated symbol
   * @param target the morphism assigned to the symbol
   */
object DefLinkAssignment {
   def apply(home : Term, name : LocalName, from: Term, target : Term) =
      DefinedStructure(home, name, from, target, false)
}

object ViewInclude {
   def apply(home: Term, from: MPath, included: Term) = DefLinkAssignment(home, LocalName(from), OMMOD(from), included)
}

/** apply/unapply methods for the special case where a view includes another view */
object PlainViewInclude {
   /** pre: included is  view with domain from */
   def apply(home: Term, from: MPath, included: MPath) = ViewInclude(home, from, OMMOD(included))
   def unapply(s: Declaration) : Option[(Term, MPath, MPath)] = {
      s match {
         case d : DefinedStructure => d.name match {
            case LocalName(List(ComplexStep(from))) => d.df match {
               case OMMOD(included) => Some((d.home, from, included))
               case _ => None
            }
            case _ => None
         }
         case _ => None
      }
   }
}
