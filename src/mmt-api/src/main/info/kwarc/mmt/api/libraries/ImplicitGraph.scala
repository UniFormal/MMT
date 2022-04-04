package info.kwarc.mmt.api.libraries

import info.kwarc.mmt.api._
import objects._
import utils._
import modules._

import scala.collection.mutable
import scala.collection.mutable._

private object ImplicitGraph {
  def emptyPath(i: MPath) = new IPath(i, i, Nil)

  /** mutable for validity, but validity does not affect identity */
  case class IEdge(val from: MPath, val to: MPath, val mor: Term) {
    def dependencies = to :: mentions(mor) // edges only depend on codomain
    private[ImplicitGraph] var valid = true
    def toPath = new IPath(from, to, List(this))
  }
  case class IPath(val from: MPath, val to: MPath, val parts: List[IEdge]) {
    lazy val morphism = OMCOMP(parts.map(_.mor))
    lazy val length = parts.length
    private[ImplicitGraph] var valid = true
    def conc(that: IPath) = new IPath(this.from, that.to, this.parts ::: that.parts)
  }
  
  private def mentions(mor: Term): List[MPath] = mor match {
    case OMS(q) => List(q.module)
    case OMPMOD(q,_) => List(q)
    case OMIDENT(t) => mentions(t)
    case OMStructuralInclude(q,r) => List(q, r) 
    case OMCOMP(ms) => ms flatMap mentions
    case OMINST(f,t,_) => List(f,t)
    case _ => Nil
  }

  class PathMap extends HashMapToSet[MPath,IPath]
}

import ImplicitGraph._

/** maintains a diagram of morphisms; used in particular by [[Library]] for the diagram of implicit morphisms
 * 
 *  the diagram does not have to be commutative
 *  
 *  the generated category is cached so that lookups do not require traversals of the diagram
 *  
 *  every edge maintains its dependencies and paths can be invalidated by calling delete
 *  invalidation is efficient: invalid paths are marked as invalid and removed only whenever they are encountered 
 *  if an invalid path becomes valid again later, a new path instance is created for it
 *  JVM garbage collection collects the invalid paths whenever they have been removed from everywhere
 */
class ImplicitGraph {
  /** maps a node to all incoming paths; may contain invalid paths, see getIncoming */
  private val incoming = new PathMap
  /** maps a node to all outgoing paths; may contain invalid paths, see getOutgoing */
  private val outgoing = new PathMap
  /** includes(from,to) if there is a plain include from -> to */
  private val includes = new IncrementalTransitiveClosure[MPath]
  /** caches the implicit path between two nodes (one of the paths if there are multiple) */
  private val primaryPath = new HashMap[(MPath,MPath),IPath]

  /** maps a structural element to all edges that depends on it */
  private val dependants = new HashMap[MPath,List[IEdge]]

  /** iterates over the valid paths in a set and, as a side effect, removes all invalid ones */
  @inline private def iterateValid(ps: HashSet[IPath]) = {
     ps.iterator.filter {p =>
       val v = isValid(p)
       if (!v) ps -= p
       v
     }
  }

 @inline private def isValid(p: IPath): Boolean = {
    // a morphism is invalid if it was previously invalidate, ...
    if (!p.valid) return false
    var lastCodomain = p.from
    p.parts.foreach {e =>
      // ... or if an edge is invalid, or if an include that makes an edge applicable is invalid
      if (!e.valid || !includes(lastCodomain,e.from)) {
        p.valid = false
        return false
      }
      lastCodomain = e.to
    }
    // or if the last edge's include into the target is invalid
    if (!includes(lastCodomain, p.to)) {
      p.valid = true
      return false
    }
    true
  }

  /** maps a node to all valid incoming paths */
  @inline private def getIncoming(to: MPath, refl: Boolean) = {
    val ps = incoming(to)
    val it = iterateValid(ps)
    if (refl) Iterator(emptyPath(to)) ++ it else it
  }
  /** maps a node to all valid outgoing paths */
  @inline private def getOutgoing(from: MPath, refl: Boolean) = {
    val ps = outgoing(from)
    val it = iterateValid(ps)
    if (refl) Iterator(emptyPath(from)) ++ it else it
  }

  /** maps a path to its dependants */
  @inline private def getDependants(p: MPath) = dependants.getOrElse(p, Nil)

  /** adds a path to all stateful data structures */
  @inline private def addPath(p: IPath) {
      /* because IPath is a case class and does not contain the include-edges,
         multiple paths that only differ in the include-steps result in only one element in the hash-set
         however, we have to use replace instead of simply adding to make sure an existing invalid path is replaced with the new valid one
       */
      replace(outgoing(p.from),p)
      replace(incoming(p.to), p)
      val tf = (p.from,p.to)
      val update = primaryPath.get(tf) match {
        case Some(q) => !isValid(q) || p.length < q.length
        case None => true
      }
      if (update) primaryPath(tf) = p
  }
  /** weird-looking function to replace an element in a hash-set with a new version of itself (or add it if it is not in the set yet)
    * only relevant if the hash-code is not injective, e.g., if it is a case class with mutable fields
    */
  @inline private def replace[U](hs: HashSet[U], u: U) {
    hs -= u
    hs += u
  }

  /** adds an implicit morphism
   *  @param from domain
   *  @param to codomain
   *  @param mor the morphism; absent if identity (i.e., plain include)
   */
  def add(from: MPath, to: MPath, mor: Option[Term]) {
    val e = mor match {
      case None =>
        includes.add(from,to)
        getIncoming(from,false) foreach {p =>
          getOutgoing(to,false)
        }
        IPath(from,to,Nil)
      case Some(m) =>
        val edge = new IEdge(from,to,m)
        edge.dependencies.foreach {d => dependants(d) = edge :: getDependants(d)}
        edge.toPath
    }
    val intoFrom = getIncoming(from,true).toList
    val outofTo = getOutgoing(to,true)
    outofTo foreach {q =>
      val eq = e conc q
      intoFrom foreach {p =>
        // peq = -p-> from -e-> to -q->
        // includes cases where p or q are empty paths, i.e., in particular e itself
        val peq = p conc eq
        addPath(peq)
      }
    }
  }
  
  /** deletes all implicit morphisms that depend on an element, must be called when that element is deleted */
  def delete(d: MPath) {
    getDependants(d) foreach {p =>
      p.valid = false
    }
    dependants -= d
    incoming -= d
    includes.delete(d, through=true,into=true,outOf=false)
    //outgoing -= d // experimental: edges only depend on codomain
  }

  /** retrieves all implicit morphisms into a theory */
  def getTo(to: MPath, refl: Boolean) = {
    val paths = getIncoming(to,refl)
    paths.map {p =>
      ((p.from, p.morphism))
    }
  }

  /** retrieves all implicit morphisms between two theories */
  private def get(from: MPath, to: MPath) = {
    val paths = getIncoming(to,true).filter {p =>
      p.from == from
    }
    val mors = paths.map(_.morphism)
    mors.toList.distinct // TODO "toList" is unnecessary and very inefficient but scala complains (multiple places in this class)
  }

  @inline private def newPrimary(from: MPath, to: MPath, p: IPath) = {
    primaryPath((from,to)) = p
    Some(p.morphism)
  }

  /** the most common lookup method: retrieves the implicit morphism between two theories (first if multiple), similar to get(from,to).headOption */
  def apply(from: MPath, to: MPath): Option[Term] = {
    val tf = (from,to)
    // if there is a valid cached path, use it
    primaryPath.get(tf) foreach {pp =>
      if (isValid(pp))
        return Some(pp.morphism)
    }
    // otherwise, find a path and update the primaryPath
    if (includes(from,to)) {
      return newPrimary(from, to, IPath(from,to,Nil))
    }
    var shortest: Option[IPath] = None
    // from != to now, otherwise above case applies
    getIncoming(to,false).foreach {p =>
      if (p.from == from) {
        if (p.length == 0) {
          return newPrimary(from, to, p)
        }
        shortest match {
          case Some(l) => if (p.length < l.length) shortest = Some(p)
          case None => shortest = Some(p)
        }
      }
    }
    shortest flatMap {p => newPrimary(from, to, p)}
  }
   /** retrieves the implicit morphism between two theory expressions (if any)
    * @param from domain
    * @param to codomain
    * @return the implicit morphism if one exists
    */
   def apply(from: Term, to: Term) : Option[Term] = {
      if (from == to) Some(OMCOMP()) else (from, to) match {
         // atomic domain: case split on codomain
         case (OMMOD(f), OMPMOD(t,_)) => apply(f,t)
         case (OMMOD(f), ComplexTheory(toCont)) =>
            val toMors = toCont.getIncludes.flatMap {t => apply(f,t).toList}
            checkUnique(toMors, from, to)
         // otherwise: case split on domain for arbitrary codomain
         case (OMPMOD(p, args), _) =>
            // TODO check agreement with args
            apply(OMMOD(p), to)
         case (ComplexTheory(fromC), _) =>
           val subs = fromC.map {
             case IncludeVarDecl(_, tp @ OMPMOD(p,_), None) =>
               apply(tp, to) match {
                 case None => return None
                 case Some(m) => IncludeSub(p,m)
               }
             case vd => to match {
               case ComplexTheory(toC) =>
                 // identity works if the same declaration occurs in the codomain
                 if (toC.variables contains vd)
                   Sub(vd.name, vd.toTerm)
                 else {
                   return None
                 }
               case _ =>
                 // no other variable declarations allowed
                 return None
             }
           }
           // the most important case: all parts of from are already included into to and we return an identity morphism
           val allIdentity = subs.forall {
             case IncludeSub(_, OMCOMP(Nil)) => true
             case Sub(n,OMV(m)) if n == m => true
             case _ => false
           }
           if (allIdentity) {
             Some(OMCOMP())
           } else {
             //TODO this should be as below, but we still need to check agreement
             // Some(ComplexMorphism(Substitution(subs: _*)))
             None
           }
         case _ => None // catches semiformal theories, which may be generated by the parser
      }
  }

  private def checkUnique(mors: List[Term], from: Term, to: Term) = {
    if (mors.isEmpty)
      None
    else {
      val h = mors.head
      if (mors.tail.forall(m => h == m))
       Some(mors.head)
      else {
       None
      }
    }
  }

  /** clears all data */
  def clear {
    incoming.clear
    outgoing.clear
    primaryPath.clear
    dependants.clear
    includes.clear
  }
}

/**
  * thrown if an implicit morphism 'old: from -> to' already existed when 'nw' was registered
  */
case class AlreadyDefined[E](from: Term, to: Term, old: E, nw: E) extends java.lang.Throwable {
  override def toString = s"implicit morphism $nw: $from -> $to already defined as $old"
}


/* old implementation of implicit graph, not used anymore but still interesting

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

/** A diagram of theories and morphisms with at most one edge between any two nodes.
 *  Morph.simplify is used to normalize paths, and equality of paths is checked by comparing normal forms;
 *  this criterion is sound and efficient but not complete.
 */
// note that Library may instantiate lib with itself, i.e., local lookup
class UniqueGraph(lib: Lookup) extends LabeledHashRelation[Term,Term] {
   /**
    * overrides update to check for existing morphisms
    *
    * throws [[AlreadyDefined]] if an implicit morphism between the same theories already exists
    */
   override def update(from: Term, to: Term, morph: Term) {
      val fromN  = TheoryExp.simplify(from)
      val toN    = TheoryExp.simplify(to)
      val morphN = Morph.simplify(morph)(lib)
      // compare to the current edge
      val current = apply(fromN,toN)
      current foreach {c =>
        // append a final implicit morphism (which Morph.equal would not know about otherwise)
        if (Morph.equal(c, morphN, from, to)(lib)) {
          return
        } else {
          throw AlreadyDefined(from, to, c, morphN)
        }
      }
      super.update(fromN, toN, morphN)
   }
}

/** maintains a thin diagram of theories
 * This is the category generated by some edges that is guaranteed to be thin (i.e., at most one morphism between any two objects)
 * i.e., all paths between two nodes must be equal.
 * UniqueGraph is used to maintain the generated category, see its description for the treatment of equality.
 * The generated category is precomputed so that retrieval of morphisms takes constant and insertion up to quadratic time.
 */
// they come up in particular as the inverse of conservative extensions such as Neg/Classical <--cons-- Neg+Classical
// The library stores includes of parametric theories as [[OMINST]].
class ThinGeneratedCategory(lib: Lookup) {
   /** generating edges of the diagram */
   private val direct = new UniqueGraph(lib)
   /** all morphisms of the diagram, i.e., including compositions (also includes direct edges) */
   private val impl   = new UniqueGraph(lib)

   /** adds an implicit morphism
    * @param from domain
    * @param to codomain
    * @param morph the morphism
    * throws [[AlreadyDefined]] if an implicit morphism m between the same theories already exists
    */
   def update(from: Term, to: Term, morph: Term) {
      // TODO: decompose links into complex theories
      from match {
         case OMPMOD(_, _) =>
             //TODO handle args
             direct(from, to) = morph
             impl(from, to) = morph // if a morphism already exists, this will check equality and throw exception if inequal
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
         case ComplexTheory(cont) =>
            cont.getIncludes.foreach {i => update(OMMOD(i), to, morph)}
         case _ =>
      }
   }

   def applyAtomic(from: MPath, to: MPath) = if (from == to) Some(OMCOMP()) else impl(OMMOD(from), OMMOD(to))

   private def checkUnique(mors: List[Term], from: Term, to: Term) = {
      if (mors.isEmpty)
        None
      else {
        val h = mors.head
        if (mors.tail.forall(m => Morph.equal(h,m, from, to)(lib)))
         Some(mors.head)
        else
         None
      }
   }


   /** retrieves the implicit morphism between two theories (if any)
    * @param from domain
    * @param to codomain
    * @return the implicit morphism if one exists
    */
   def apply(from: Term, to: Term) : Option[Term] = {
      if (from == to) Some(OMCOMP()) else (from, to) match {
         // atomic domain: case split on codomain
         case (OMMOD(f), OMMOD(t)) => applyAtomic(f,t)
         case (OMMOD(f), OMPMOD(t,_)) => applyAtomic(f,t)
         case (OMMOD(f), ComplexTheory(toCont)) =>
            val toMors = toCont.getIncludes.flatMap {t => applyAtomic(f,t).toList}
            checkUnique(toMors, from, to)
         // otherwise: case split on domain for arbitrary codomain
         case (OMPMOD(p, args), _) =>
            // TODO check agreement with args
            apply(OMMOD(p), to)
         case (ComplexTheory(fromC), _) =>
            val subs = fromC.map {
               case IncludeVarDecl(_, tp @ OMPMOD(p,_), None) =>
                  apply(tp, to) match {
                    case None => return None
                    case Some(m) => IncludeSub(p,m)
                  }
               case vd => to match {
                 case ComplexTheory(toC) =>
                   // identity works if the same declaration occurs in the codomain
                   if (toC.variables contains vd)
                     Sub(vd.name, vd.toTerm)
                   else {
                     return None
                   }
                 case _ =>
                   // no other variable declarations allowed
                   return None
               }
            }
            // the most important case: all parts of from are already included into to and we return an identity morphism
            val allIdentity = subs.forall {
              case IncludeSub(_, OMCOMP(Nil)) => true
              case Sub(n,OMV(m)) if n == m => true
              case _ => false
            }
            if (allIdentity) {
              Some(OMCOMP())
            } else {
              //TODO this should be as below, but we still need to check agreement
              // Some(ComplexMorphism(Substitution(subs: _*)))
              None
            }
         case _ => None // catches semiformal theories, which may be generated by the parser
      }
   }

   /** retrieves all pairs (to,morph) for from */
   def outOf(from: Term) : HashSet[(Term,Term)] = from match {
      case OMMOD(p) => impl.outOf(from)
      case OMPMOD(p, args) => impl.outOf(from) //TODO check agreement with args
      case TUnion(ts) => impl.outOf(TheoryExp.simplify(from)) //TODO does not yield all morphisms
      case ComplexTheory(cont) => impl.outOf(from)  //TODO does not yield all morphisms
      case _ => HashSet.empty
   }
   /** retrieves all pairs (from,morph) for to */
   def into (to: Term) : HashSet[(Term,Term)] = to match {
      case OMMOD(p) => impl.into(to)
      case OMPMOD(p, _) => impl.into(to)
      case ComplexTheory(cont) =>
         HashSet(cont.getIncludes:_*).flatMap(t => into(OMMOD(t)))
      case _ => HashSet.empty
   }

   def clear {
      direct.clear
      impl.clear
   }
}
*/