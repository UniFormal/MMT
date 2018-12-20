package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._

/**
 * atomic MMT morphism, unifies views (which are modules) and structures (which are declarations)
 * 
 * The declarations in the body are assignments, which are
 * 1) [[Constant]]s, whose name is the qualified name (always starts with [[ComplexStep]])
 * of a domain [[Constant]]
 * and whose definiens is codomain [[Term]]
 * or
 * 2) or accordingly with [[DeclaredStructure]]s with definiens
 */
trait Link extends ModuleOrLink {
   /** the domain of the link */
   def fromC: TermContainer
   /** the codomain of the link (mutable for views but not structures) */
   def toC: AbstractTermContainer
   /** the domain of this link; pre: must have be given explicitly or have been inferred */
   def from = fromC.get.getOrElse(throw ImplementationError("can only call this method after domain has been inferred"))
   /** the codomain of this link; pre: must have be given explicitly or have been inferred */
   def to = toC.get.getOrElse(throw ImplementationError("can only call this method after codomain has been inferred"))
   /** the codomain as a context; pre: same as `to` */
   def codomainAsContext = toC.get match {
       case Some(ComplexTheory(cont)) => cont
       case _ => throw ImplementationError("codomain of link must be theory")
    }

   /** true if this link is implicit */
   def isImplicit : Boolean
   protected def implicitString = if (isImplicit) "implicit " else ""

   /** like getIncludes but also with includes of parametric theories and their instantiations */
   def getIncludes: List[(MPath,Term)] = getDeclarations.flatMap {
     case LinkInclude(_, from, df) => List((from,df))
     case _ => Nil
   }
   
   /** the prefix used when translating declarations along this link */
   def namePrefix: LocalName
}
