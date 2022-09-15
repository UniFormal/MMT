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
      Constant(home, name, alias, None, target, None, NotationContainer.empty())
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
      Structure(home, name, from, Some(target), false, false)
   def unapply(se: StructuralElement): Option[(Term,LocalName,Term,Term)] = se match {
     case s: Structure =>
       s.df map {df =>
         (s.home, s.name, s.from, df)
       }
     case _ => None
   }
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
    case s: Structure =>
      (s.from, s.df) match {
        case (OMMOD(f), Some(df)) if s.name == LocalName(f) => Some((s.home, f, df))
        case _ => None
      }
    case _ => None
  }
}

/** a [[LinkInclude]] of the identity morphism: the analog to a plain include in a theory */
object IdentityInclude {
  /**
   * @param home the link
   * @param from the theory that is included into domain and codomain and fixed by the link
   */
  def apply(home: Term, from: MPath) = LinkInclude(home, from, OMIDENT(OMMOD(from)))
  def unapply(d: ContentElement) = d match {
    case LinkInclude(h, f, OMIDENT(OMMOD(p))) if f == p => Some((h,f))
    case _ => None
  }
}
