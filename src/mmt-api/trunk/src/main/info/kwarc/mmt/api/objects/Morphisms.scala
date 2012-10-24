package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import libraries._
import modules._
import utils._

//TODO definition expansion everywhere

object Morph {
   val Empty = OMID(mmt.mempty) 
   /** pre: m is a well-structured morphism */
	def domain(m : Term)(implicit lib : Lookup) : Term = m match {
      case OMIDENT(t) => t
      case OMCOMP(Nil) => throw Invalid("cannot infer domain of empty composition")
      case OMCOMP(n :: _) => domain(n)
      case Morph.Empty => throw Invalid("cannot infer domain of empty union")
      case MUnion(ms) => TUnion(ms map domain)
      case OMMOD(path) => try {
         lib.get(path) match {case l: Link => l.from}
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
      case OMDL(h,n) => try {
         val structure = lib.getStructure(h % n)
         structure.from
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
      case _ => throw Invalid("not a well-formed morphism: " + m)
    }

  /** pre: m is a well-structured morphism */
   def codomain(m : Term)(implicit lib : Lookup) : Term = m match {
      case OMIDENT(t) => t
      case OMCOMP(Nil) => throw Invalid("cannot infer codomain of empty composition")
      case OMCOMP(l) => codomain(l.last)
      case Morph.Empty => throw Invalid("cannot infer codomain of empty union")
      case MUnion(ms) =>
         val cs = ms map codomain
         if (cs.forall(_ == cs.head)) cs.head
         else TUnion(cs)
      case OMMOD(path) => try {
         lib.get(path) match {case l: Link => l.to}
      } catch {
         case _ => throw Invalid("not a well-formed morphism: " + m)
      }
      case OMDL(t, _ ) => t
      case _ => throw Invalid("not a well-formed morphism: " + m)
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
        case OMDL(h,n) => OMDL(h,n)
        case OMIDENT(t) => OMCOMP()
        case Morph.Empty => Morph.Empty
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
           val msS = (ms map simplify).flatMap(MUnion.associate).filterNot(_ == Morph.Empty).distinct.sortBy(_.hashCode)
           msS match {
             case Nil => Morph.Empty
             case m :: Nil => m
             case ms => MUnion(ms)
           }
     }
   }
   
   /** checks equality of two morphisms using the simplify method; sound but not complete */
   def equal(a: Term, b: Term): Boolean = simplify(a) == simplify(b)
}

object TheoryExp {
  val Empty = OMID(mmt.tempty)
  /** simplifies a theory expression using basic algebraic laws, e.g., commutativity of union */
  def simplify(thy: Term): Term = thy match {
     case OMMOD(p) => OMMOD(p)
     case TheoryExp.Empty => TheoryExp.Empty
     case TUnion(ts) =>
                                   //  associativity          neutrality           idempotence  commutativity
        val tsS = (ts map simplify).flatMap(TUnion.associate).filterNot(_ == TheoryExp.Empty).distinct.sortBy(_.hashCode)
        tsS match {
          case Nil => TheoryExp.Empty
          case t :: Nil => t
          case ts => TUnion(ts)
        }
  }
  /** checks equality of two theory expression using simplify */
  def equal(s: Term, t: Term) = simplify(s) == simplify(t)
  /** computes the meta-theory of a theory */
  def meta(thy: Term)(implicit lib: Lookup) : Option[MPath] = thy match {
      case OMMOD(p) => lib.getTheory(p) match {
         case t: DeclaredTheory => t.meta
         case t: DefinedTheory => meta(t.df)
      }
      case TheoryExp.Empty => None
      case TUnion(ts) =>
         val ms = ts map {t => meta(t)}
         if (ms forall {m => m == ms.head}) ms.head
         else None
   }
   /** checks whether "from" is included into "to", relative to a base function that handles the case where both theory expressions are atomic */
   def imports(from: Term, to: Term)(implicit atomic: (MPath,MPath) => Boolean) : Boolean = {
      if (from == to) true else (from, to) match {
         case (OMMOD(f), OMMOD(t)) => atomic(f,t)
         case (TheoryExp.Empty, _ ) => true
         case (_, TheoryExp.Empty) => false
         case (_, TUnion(ts)) => ts exists {t => imports(from, t)}
         case (TUnion(ts), _) => ts forall {t => imports(t, from)}
         case _ => false // catches semiformal theories, which may be generated by the parser
      }
   }
   /** a sound but not complete approximation of imports that does not make use of any lookups
    *  and uses only the information given in the theory expressions themselves */ 
   def importsDefinitely(from: Term, to: Term) = imports(from,to) {(f,t) => f == t}
   
   /** returns a human-oriented (short) String representation of a theory expression */
   def toString(t: Term) : String = t match {
      case OMMOD(f) => f.last
      case TheoryExp.Empty => "empty"
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
     case Nil => TheoryExp.Empty
     case hd :: Nil => hd
     case hd :: tl => OMA(OMID(mmt.tunion), tl.toList)
   }
   def unapply(union: Term) : Option[List[Term]] = union match {
     case OMA(OMID(mmt.tunion), thys) if (thys.forall(_.isInstanceOf[Term])) => Some(thys.asInstanceOf[List[Term]])
     case _ => None
   }
   /** applies associativity of union by merging nested TUnion */
   def associate(t: Term): List[Term] = t match {
     case TUnion(ts) => ts flatMap associate
     case t => List(t)
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

object MUnion {
   def apply(morphs: List[Term]) : Term = morphs match {
      case Nil => Morph.Empty
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

object ExplicitMorph {
  def apply(rec : Record, dom : Term) : Term = OMA(OMID(mmt.explmorph), List(dom, OMREC(rec)))
  def unapply(m : Term) : Option[(Record, Term)] = m match {
    case OMA(OMID(mmt.explmorph), List(dom, OMREC(rec))) => Some((rec, dom))
    case _ => None
  }
}


/*
case class OMUNIONPIM(push: Morph, along: Morph, wth: Morph) extends Morph with ComposedModuleObject {
   def role = Role_pushout_im
   def components = List(push, along, wth)
   override def toString = push.toString + " + " + along.toString + " with " + wth.toString 
}

case class OMPIW(raise: Morph, to: TheoryObj) extends Morph with ComposedModuleObject {
   def role = Role_pushout_iim
   def components = List(raise, to)
   override def toString = raise.toString + " * " + to.toString
}
*/
