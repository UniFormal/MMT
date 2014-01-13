package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import libraries._
import modules._
import utils._

//TODO definition expansion everywhere

object Morph {
   val empty = ComplexMorphism(Substitution()) 
   /** pre: m is a well-structured morphism */
	def domain(m : Term)(implicit lib : Lookup) : Option[Term] = m match {
      case OMIDENT(t) => Some(t)
      case OMCOMP(n :: _) => domain(n)
      case OMMOD(path) => try {
         lib.get(path) match {case l: Link => Some(l.from)}
      } catch {
         case _ : Throwable => throw InvalidObject(m, "not a well-formed morphism")
      }
      case OMDL(h,n) => try {
         val structure = lib.getStructure(h % n)
         Some(structure.from)
      } catch {
         case _ : Throwable => throw InvalidObject(m, "not a well-formed morphism")
      }
      case _ => None
    }

  /** pre: m is a well-structured morphism */
   def codomain(m : Term)(implicit lib : Lookup) : Option[Term] = m match {
      case OMIDENT(t) => Some(t)
      case OMCOMP(l) if !l.isEmpty => codomain(l.last)
      case OMMOD(path) => try {
         lib.get(path) match {case l: Link => Some(l.to)}
      } catch {
         case _ : Throwable => throw throw InvalidObject(m, "not a well-formed morphism: " + m)
      }
      case OMDL(t, _ ) => Some(t)
      case _ => None
   }
   
   /** transform a morphism into a list of morphisms by using associativity of composition */ 
   def associateComposition(m: Term) : List[Term] = m match {
      case OMCOMP(ms) => ms flatMap associateComposition
      case m => List(m)
   }
   
   /** pre: m is a valid morphism
    *  post: m and simplify(m) are equal
    *  inclusion and identity morphisms are simplified to OMCOMP()
    */
   def simplify(m: Term) : Term = {
     val mS = OMCOMP(associateComposition(m))
     mS match {
        case OMMOD(p) => OMMOD(p)
        case OMDL(h,ln) =>
           val lnS = ln.simplify
           if (lnS.length == 1 && lnS.head.isInstanceOf[ComplexStep])
              OMCOMP()
           else
              OMDL(h, lnS)
        case OMIDENT(t) => OMCOMP()
        case OMCOMP(ms) =>
           val msS = (ms map simplify) filter {
             case OMIDENT(_) => false
             case OMCOMP(Nil) => false
             case _ => true
           }
           msS match {
             case Nil => OMCOMP()
             case m :: Nil => m
             case ms => OMCOMP(ms)
           }
        case MUnion(ms) =>
                                   //  associativity             neutrality           idempotence  commutativity
           val msS = (ms map simplify).flatMap(MUnion.associate).filterNot(_ == Morph.empty).distinct.sortBy(_.hashCode)
           msS match {
             case Nil => Morph.empty
             case m :: Nil => m
             case ms => MUnion(ms)
           }
     }
   }
/*
   /** restricts a well-formed morphism to a domain */
   def restrict(from: Term, m: Term)(implicit lib: Lookup) : Term = m 
   m match {     case OMMOD(p) =>       val l = lib.getLink(p)
         if (TheoryExp.equal(l.from, from))
            OMMOD(p)
         else from match {
            case OMMOD(t) =>
               lib.getO(p ? MorphismStep(OMIDENT(from))) match {
                  case Some(d: DefLinkAssignment) => d.target
                  case _ => OMMOD(p)
               }
            case _ => OMMOD(p)
         }
      case OMDL(h,n) =>
      case OMIDENT(t) => OMIDENT(from)
      case Morph.Empty => Morph.Empty
      case OMCOMP(ms) => ms match {
         case Nil => OMCOMP()
         case hd::tl => OMCOMP(restrict(from, hd) :: tl)
      }
      case MUnion(ms) =>
         MUnion(ms) //TODO: from must be included into MUnion(ms') for ms' <= ms
   }*/
      
   /** checks equality of two morphisms using the simplify method; sound but not complete */
   def equal(a: Term, b: Term): Boolean = simplify(a) == simplify(b)
}

object TheoryExp {
  val empty = ComplexTheory(Context())
  /** simplifies a theory expression using basic algebraic laws, e.g., commutativity of union */
  def simplify(thy: Term): Term = thy match {
     case OMMOD(p) => OMMOD(p)
     case TUnion(ts) =>
                                   //  associativity          idempotence  commutativity
        val tsS = (ts map simplify).flatMap(TUnion.associate).distinct.sortBy(_.hashCode)
        tsS match {
          case t :: Nil => t
          case ts => TUnion(ts)
        }
  }
  /** checks equality of two theory expression using simplify */
  def equal(s: Term, t: Term) = simplify(s) == simplify(t)
  /** computes the meta-theories of a theory, nearest one first
   *  @param all if true, stop after the first meta-theory, false by default
   */
  def metas(thy: Term, all: Boolean = true)(implicit lib: Lookup) : List[MPath] = thy match {
      case OMMOD(p) => lib.getTheory(p) match {
         case t: DeclaredTheory => t.meta match {
            case None => Nil
            case Some(m) => if (all) m :: metas(OMMOD(m)) else List(m)
         }
         case t: DefinedTheory => metas(t.df)
      }
      case TUnion(ts) =>
         val ms = ts map {t => metas(t)}
         if (!ms.isEmpty && ms.forall(m => m == ms.head)) ms.head
         else Nil
   }
  
   /** checks whether "from" is included into "to", relative to a base function that handles the case where both theory expressions are atomic */
   def imports(from: Term, to: Term)(implicit atomic: (MPath,MPath) => Boolean) : Boolean = {
      if (from == to) true else (from, to) match {
         case (OMMOD(f), OMMOD(t)) => atomic(f,t)
         case (_, TUnion(ts)) => ts exists {t => imports(from, t)}
         case (TUnion(ts), _) => ts forall {t => imports(t, from)}
         case _ => false // catches semiformal theories, which may be generated by the parser
      }
   }
   /** a sound but not complete approximation of imports that does not make use of any lookups
    *  and uses only the information given in the theory expressions themselves
    */ 
   def importsDefinitely(from: Term, to: Term) = imports(from,to) {(f,t) => f == t}
   
   /** returns a human-oriented (short) String representation of a theory expression */
   def toString(t: Term) : String = t match {
      case OMMOD(f) => f.last
      case TUnion(ts) => ts.map(toString).mkString("", " + ", "")
   }
}

/**
 * An OMMOD represents a reference to a module (which may be a theory or a morphism expression).
 */
object OMMOD {
   /** @param path the path to the module */
   def apply(path : MPath) : OMID = OMID(path)
   def unapply(term : Term) : Option[MPath] = term match {
     case OMID(m: MPath) => Some(m)
     case _ => None
   }
}

object TUnion {
   def apply(thys: List[Term]) : Term = thys match {
     case hd :: Nil => hd
     case _ => OMA(OMID(mmt.tunion), thys)
   }
   def unapply(union: Term) : Option[List[Term]] = union match {
     case OMA(OMID(mmt.tunion), thys) => Some(thys)
     
     case _ => None
   }
   /** applies associativity of union by merging nested TUnion */
   def associate(t: Term): List[Term] = t match {
     case TUnion(ts) => ts flatMap associate
     case t => List(t)
   }
}

object ComplexTheory {
   def apply(body: Context) = OMBINDC(OMID(mmt.tunion), body, Nil)
   def unapply(t: Term) : Option[Context] = t match {
      case OMBINDC(OMID(mmt.tunion), body, Nil) => Some(body)
      case OMMOD(p) => Some(Context(IncludeVarDecl(p)))
      case _ => None
   }
}
/**
 * An OMDL represents a structure
 */
object OMDL {
   def apply(cod: Term, name: LocalName) : Term = OMID(cod % name)
   def unapply(t: Term) : Option[(Term, LocalName)] = t match {
     case OMID(cod % name) => Some((cod, name))
     case _ => None
   }
}

/**
 * An OMCOMP represents the associative composition of a list of morphisms. Compositions may be nested.
 */
object OMCOMP {
 /** 
  * @param morphs the list of morphisms in diagram order
  * the trivial morphism OMCOMP() is dropped from morphs
  */
  def apply(morphs: List[Term]) : Term = {
     val morphsNT = morphs.filterNot {_ == OMCOMP()}
     morphsNT match {
        case hd :: Nil => hd
        case _ => OMA(OMID(mmt.composition), morphsNT)
     }
   }
   def apply(morphs: Term*) : Term = apply(morphs.toList)
   def unapply(t: Term) : Option[List[Term]] = t match {
     case OMA(OMID(mmt.composition), morphs) => Some(morphs)
     case _ => None
   }
}

/**
 * An OMIDENT represents the identity morphism of a theory.
 * @param theory the theory
 */
object OMIDENT {
   def apply(theory : Term) : Term = OMA(OMID(mmt.identity), List(theory))
   def unapply(t : Term) : Option[Term] = t match {
     case OMA(OMID(mmt.identity), List(theory)) => Some(theory)
     case _ => None
   }
}

object ComplexMorphism {
   def apply(body: Substitution) = ComplexTerm(mmt.munion, body, Context(), Nil)
   def unapply(t: Term) : Option[Substitution] = t match {
      case ComplexTerm(mmt.munion, sub, Context(), Nil) => Some(sub)
      case _ => None
   }
}

object MUnion {
   def apply(morphs: List[Term]) : Term = morphs match {
      case Nil => Morph.empty
      case hd :: Nil => hd
      case hd :: tl => OMA(OMID(mmt.munion), morphs)
   }
   def unapply(t : Term) : Option[List[Term]] = t match {
     case OMA(OMID(mmt.munion), morphs) if (morphs.forall(_.isInstanceOf[Term])) => Some(morphs.asInstanceOf[List[Term]])
     case _ => None
   }
   /** applies associativity of union by merging nested MUnion */
   def associate(m: Term): List[Term] = m match {
     case MUnion(ms) => ms flatMap associate
     case m => List(m)
   }
}

