package info.kwarc.mmt.api.libraries

import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import modules._
import utils.HashMapToSet
import utils.MyList._

import scala.collection.mutable.HashSet

/** maintains a binary relation on N where pairs in the relation are labeled with values from E
 * hashes in both directions are used to make all lookups fast
 */
class LabeledHashRelation[N,E] {
   /** maps every FROM to the set of pairs (TO, VALUE) */
   private val edgesFrom = new HashMapToSet[N,(N,E)]
   /** maps every TO to the set of pairs (FROM, VALUE) */
   private val edgesTo   = new HashMapToSet[N,(N,E)]
   
   /** adds (from, to) into the relation with label edge
    * if the pair is already in the relation, the function clash is called before the value is set
    */  
   def update(from: N, to: N, edge: E) {
      edgesFrom += (from, (to,   edge))
      edgesTo   += (to,   (from, edge))
   }
   /** retrieves the label of the pair (from,to), if any */
   def apply(from: N, to: N) : Option[E] = {
      edgesFrom(from) find {
         case (n, e) => n == to
      } map {
         case (n, e) => e
      }
   }
   /** retrieves all pairs (TO,VALUE) for from */
   def outOf(from: N) : HashSet[(N,E)] = edgesFrom(from)
   /** retrieves all pairs (FROM,VALUE) for to */
   def into (to: N) : HashSet[(N,E)] = edgesTo(to)
   
   def clear {
      edgesFrom.clear
      edgesTo.clear
   }
   
   override def toString = edgesFrom.map {
      case (n, set) => "from " + n.toString + "\n" + (set.map {case (n,e) => "  " + n.toString + "\n  " + e.toString + "\n"}.mkString("\n"))
   }.mkString("\n")
}

/**
 * thrown if the uniqueness condition of UniqueGraph is violated
 *  @param value the existing value
 */
case class AlreadyDefined[E](old: E, nw: E) extends java.lang.Throwable

/** A diagram of theories and morphisms.
 *  i.e., edges between two nodes must be equal. 
 *  Morph.simplify is used to normalize paths, and equality of paths is checked by comparing normal forms;
 *  this criterion is sound and efficient but not complete.
 */
class UniqueGraph extends LabeledHashRelation[Term,Term] {
   /** overrides update to check for existing morphisms
    * @throws AlreadyDefined(m) if an implicit morphism m between the same theories already exists
    */
   override def update(from: Term, to: Term, morph: Term) {
      val morphN = Morph.simplify(morph)
      var current = apply(from,to)
      if (current.isDefined) {
           if (current.get == morphN)
              return
           else
              throw AlreadyDefined(current.get, morphN)
      }
      super.update(from, to, morphN)
   }
}

/** maintains a thin diagram of theories
 * This is the category generated by some edges that is guaranteed to be thin (i.e., at most one morphism between any two objects)
 * i.e., all paths between two nodes must be equal.
 * UniqueGraph is used to maintain the generated category, see its description for the treatment of equality.
 * The generated category is precomputed so that retrieval of morphisms takes constant and insertion up to quadratic time. 
 */ 
class ThinGeneratedCategory {
   /** generating edges of the diagram */
   private val direct = new UniqueGraph
   /** all morphisms of the diagram, i.e., including compositions (also includes direct edges) */
   private val impl   = new UniqueGraph
   
   /** adds an implicit morphism
    * @param from domain
    * @param to codomain
    * @param the morphism
    * @throws AlreadyDefined(m) if an implicit morphism m between the same theories already exists
    */
   def update(from: Term, to: Term, morph: Term) {
      // TODO: decompose links between complex theories (inverse to how apply composes them)
      val existsAlready = impl(from ,to).isDefined
      // if existsAlready == true, this will check equality and throw exception if inequal
      direct(from, to) = morph
      if (! existsAlready) {
         impl  (from, to) = morph
         (impl into from) foreach {
            case (f,m) =>
              impl(f,to) = OMCOMP(m, morph)
              (impl outOf to) foreach {
                  case (t,m2) => impl(f, t) = OMCOMP(m, morph, m2)
              }
         }
         (impl outOf to) foreach {
            case (t,m) => impl(from, t) = OMCOMP(morph, m)
         }
      }
   }
   
   def applyAtomic(from: MPath, to: MPath) = if (from == to) Some(OMCOMP()) else impl(OMMOD(from), OMMOD(to))
   private def checkUnique(mors: List[Term]) =
      if (mors.isEmpty)
         None
      else if (mors.distinct.length == 1)
         Some(mors.head)
      else
         None
   private def checkAgree(morOpts: List[Option[Term]]): Option[Term] = { 
      val mors = morOpts.map(_.getOrElse(return None))
      //TODO check that all morphisms agree on joint domains
      Some(MUnion(mors))
   }
   /** retrieves the implicit morphism between two theories (if any)
    * @param from domain
    * @param to codomain
    * @return the implicit morphism if one exists
    */
   def apply(from: Term, to: Term) : Option[Term] = {
      if (from == to) Some(OMCOMP()) else (from, to) match {
         // atomic domain, case split on codomain
         case (OMMOD(f), OMMOD(t)) => applyAtomic(f,t)
         case (OMMOD(f), OMPMOD(t,_)) => applyAtomic(f,t)
         case (OMMOD(f), TUnion(ts)) =>
            val tsMors = ts.flatMap {t => apply(from,t).toList}
            checkUnique(tsMors)
         case (OMMOD(f), ComplexTheory(toCont)) =>
            val toMors = toCont.getIncludes.flatMap {t => applyAtomic(f,t).toList}
            checkUnique(toMors)
         // otherwise, case split on domain for arbitrary codomain
         case (OMPMOD(p, args), _) =>
            // TODO check agreement with args
            apply(OMMOD(p), to)
         case (TUnion(ts), _) =>
            if (ts.isEmpty) return Some(OMCOMP())
            val tsMors = ts.map {t => apply(t, from)}
            checkAgree(tsMors)
         // TODO remove unions or handle their interaction with ComplexTheory
         case (ComplexTheory(fromC), _) =>
            val fromCMors = fromC.map {
               case IncludeVarDecl(p,args) =>
                  apply(OMPMOD(p,args), to)
               case vd => to match {
                  case ComplexTheory(toC) => Some(OMCOMP())
                  case _ => None
               }
            }
            checkAgree(fromCMors)
         case _ => None // catches semiformal theories, which may be generated by the parser
      }
   }

   /** retrieves all pairs (to,Morph) for from */
   def outOf(from: Term) : HashSet[(Term,Term)] = impl.outOf(from)
   /** retrieves all pairs (from,Morph) for to */
   def into (to: Term) : HashSet[(Term,Term)] = impl.into(to)
   
   def clear {
      direct.clear
      impl.clear
   }
}