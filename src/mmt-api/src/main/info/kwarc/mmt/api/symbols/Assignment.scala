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


/** include of a morphism into a link */
object LinkInclude {
  /**
   * @param home the link
   * @param from the domain of the included morphism
   * @param mor the morphism
   */
  def apply(home: Term, from: MPath, mor: Term) = DefLinkAssignment(home, LocalName(from), OMMOD(from), mor)
  def unapply(d: ContentElement) = d match {
    case d: DefinedStructure =>
      d.from match {
        case OMMOD(f) if d.name == LocalName(f) => Some((d.home, f, d.df))
        case _ => None
      }
    case _ => None
  }
}
