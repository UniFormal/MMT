package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

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
   def from: Term = fromC.get.getOrElse {
     throw ImplementationError("can only call this method after domain has been inferred")
   }
   /** the domain as a context */
   def domainAsContext: Context = toC.get match {
    case Some(ComplexTheory(cont)) => cont
    case _ => throw ImplementationError("domain of link must be union of atomic theories")
   }
  /** the codomain of this link; pre: must have been given explicitly or have been inferred */
   def to: Term = toC.get.getOrElse(throw ImplementationError("can only call this method after codomain has been inferred"))
   /** list of theories whose union is included into 'to' */
   def codomainPaths = codomainAsContext.getIncludes
   /** the codomain as a context */
   def codomainAsContext: Context = toC.get match {
    case Some(ComplexTheory(cont)) => cont
    case _ => throw ImplementationError("codomain of link must be union of atomic theories")
   }

   /** true if this link is implicit */
   def isImplicit : Boolean
   protected def implicitString: String = if (isImplicit) "implicit " else ""

   /** like getIncludes but also with includes of parametric theories and their instantiations */
   override def getAllIncludes: List[IncludeData] = getDeclarations.flatMap {
     case Include(id) => List(id)
     case _ => Nil
   }

  /**
    * Get an [[IncludeData]] referencing [[from]] as the domain and this very link
    * as the definiens (via [[OMMOD OMMOD(modulePath)]]).
    */
   override def selfInclude: IncludeData = {
     from match {
       case OMPMOD(p, args) => IncludeData(toTerm, p, args, Some(OMMOD(modulePath)), total = false)
       case f => IncludeData(toTerm, f.toMPath, Nil, Some(OMMOD(modulePath)), total = false)
     }
   }
   
   /** the prefix used when translating declarations along this link */
   def namePrefix: LocalName
}
