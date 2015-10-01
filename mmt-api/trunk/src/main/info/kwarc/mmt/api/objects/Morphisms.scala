package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import libraries._
import modules._
import utils._
import Conversions._

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
         case _ : Throwable => throw InvalidObject(m, "not a well-formed morphism: " + m)
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

/**
 * an auxiliary class to store domains of [[Context]]s and theory expressions (see [[TheoryExp]])

 * @param name the name
 * @param defined true if name has a definiens (including total morphisms in [[DefinedStructure]] and [[StructureVarDecl]])
 * @param subdomain if name is an import, the imported theory and
 *    (if not defined as in [[DeclaredStructure]] and [[StructureVarDecl]]) the instantiated names of the partial morphism
 */
case class DomainElement(name: LocalName, defined: Boolean, subdomain: Option[(Term,List[LocalName])])

object TheoryExp {
  val empty = ComplexTheory(Context())
  /** simplifies a theory expression using basic algebraic laws, e.g., commutativity of union */
  def simplify(thy: Term): Term = thy match {
     case OMMOD(p) => OMMOD(p)
     case ComplexTheory(cont) =>
        ComplexTheory(cont)
     case TUnion(ts) =>
                                   //  associativity          idempotence  commutativity
        val tsS = (ts map simplify).flatMap(TUnion.associate).distinct.sortBy(_.hashCode)
        tsS match {
          case t :: Nil => t
          case ts => TUnion(ts)
        }
     case thy => thy
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

   /**
    * @param l left theory
    * @param r right theory
    * @param the union of l and r (formed by concatenating contexts)
    */
   def union(l: Term, r: Term) = (l,r) match {
      case (ComplexTheory(lC), ComplexTheory(rC)) => ComplexTheory(lC ++ rC)
   }

   /**
    *  @param t a theory
    *  @return the list of elements in the domain of t (not flattened)
    */
   def getDomain(t: Term)(implicit lib: Lookup): List[DomainElement] = t match {
      case OMMOD(p) => lib.getTheory(p) match {
         case d: DeclaredTheory =>
            d.getPrimitiveDeclarations.map {
               case s: symbols.DefinedStructure =>
                  DomainElement(s.name, true, Some((s.from, Nil)))
               case s: symbols.DeclaredStructure =>
                  val definitions = s.getPrimitiveDeclarations.map(_.name)
                  DomainElement(s.name, false, Some((s.from, definitions)))
               case c: symbols.Constant =>
                  DomainElement(d.name, c.df.isDefined, None)
               case d =>
                  DomainElement(d.name, true, None) //by default, no map allowed
            }
         case d: DefinedTheory => getDomain(d.df)
      }
      case ComplexTheory(body) =>
         body.getDomain
   }

   /** all directly included named theories */
   def getSupport(t: Term): List[MPath] = t match {
      case OMMOD(p) => List(p)
      case ComplexTheory(cont) => cont.getIncludes.toList
      case TUnion(ts) => (ts flatMap getSupport).distinct
   }

   /** returns a human-oriented (short) String representation of a theory expression */
   def toString(t: Term) : String = t match {
      case OMMOD(f) => f.last
      case TUnion(ts) => ts.map(toString).mkString("", " + ", "")
   }
}

object ModExp extends uom.TheoryScala {
   val _base = DPath(utils.URI("http", "cds.omdoc.org") / "mmt")
   val _name = LocalName("ModExp")

   val theorytype = _path ? "theory"
   val morphtype = _path ? "morphism"
   val complextheory = _path ? "complextheory"
   val complexmorphism = _path ? "complexmorphism"
   val identity = _path ? "identity"
   val composition = _path ? "composition"
   val morphismapplication = _path ? "morphismapplication"

   //TODO deprecate
   val tunion = _path ? "theory-union"
   val munion = _path ? "morphism-union"
}

/**
 * An OMMOD represents a reference to a module (which may be a theory or a morphism expression).
 */
object OMMOD {
   /** @param path the path to the module */
   def apply(path: MPath) : OMID = OMID(path)
   def unapply(term : Term) : Option[MPath] = term match {
     case OMID(m: MPath) => Some(m)
     case _ => None
   }
}

/**
 * An OMPMOD represents a reference to a parametric module applied to arguments
 */
object OMPMOD {
   /** @param path the path to the module */
   def apply(path: MPath, args: List[Term]) = OMAorOMID(OMMOD(path), args)
   def unapply(term : Term) : Option[(MPath, List[Term])] = term match {
     case OMAorOMID(OMMOD(m), args) => Some((m,args))
     case _ => None
   }
}

object TUnion {
   def apply(thys: List[Term]) : Term = thys match {
     case hd :: Nil => hd
     case _ => OMA(OMID(ModExp.tunion), thys)
   }
   def unapply(union: Term) : Option[List[Term]] = union match {
     case OMA(OMID(ModExp.tunion), thys) => Some(thys)

     case _ => None
   }
   /** applies associativity of union by merging nested TUnion */
   def associate(t: Term): List[Term] = t match {
     case TUnion(ts) => ts flatMap associate
     case t => List(t)
   }
}

/**
 * the normal form of a theory expression is a context
 *
 * the apply/unapply functions convert between them
 */

object ComplexTheory {
   val path = ModExp.complextheory
   def apply(body: Context) = body match {
      case Context(IncludeVarDecl(p,args)) => OMPMOD(p,args)
      case _ => OMBINDC(OMID(this.path), body, Nil)
   }
   def unapply(t: Term) : Option[Context] = t match {
      case OMBINDC(OMID(this.path), body, Nil) => Some(body)
      case OMPMOD(p, args) => Some(Context(IncludeVarDecl(p, args)))
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
 * MorphType(from,to) is the type of morphisms
 */
object MorphType {
   val path = ModExp.morphtype
   def apply(from: Term, to: Term) = OMA(OMS(this.path), List(from,to))
   def unapply(t: Term) : Option[(Term,Term)] = t match {
      case OMA(OMS(this.path), List(from,to)) => Some((from, to))
      case _ => None
   }
}

object TheoryType {
   val path = ModExp.theorytype
   def apply(params: Context) = ComplexTerm(this.path, Nil, params, Nil)
   def unapply(t: Term): Option[Context] = t match {
      case OMID(this.path) => Some(Nil)
      case ComplexTerm(this.path, Substitution(), params, Nil) => Some(params)
      case _ => None
   }
}

/**
 * An OMM represents the application of a morphism to a term
 */
object OMM {
  val path = ModExp.morphismapplication
 /**
  * @param t the term
  * @param m the morphism
  */
  def apply(t: Term, m: Term) : Term = OMA(OMS(this.path), List(t,m))
  def unapply(t: Term) : Option[(Term,Term)] = t match {
     case OMA(OMS(this.path), List(t,m)) => Some((t,m))
     case _ => None
   }
}

/**
 * An OMCOMP represents the associative composition of a list of morphisms. Compositions may be nested.
 */
object OMCOMP {
 val path = ModExp.composition
 /**
  * @param morphs the list of morphisms in diagram order
  * the trivial morphism OMCOMP() is dropped from morphs
  */
  def apply(morphs: List[Term]) : Term = {
     val morphsNT = morphs.filterNot {_ == OMCOMP()}
     morphsNT match {
        case hd :: Nil => hd
        case _ => OMA(OMID(this.path), morphsNT)
     }
   }
   def apply(morphs: Term*) : Term = apply(morphs.toList)
   def unapply(t: Term) : Option[List[Term]] = t match {
     case OMA(OMID(this.path), morphs) => Some(morphs)
     case _ => None
   }
}

/**
 * An OMIDENT represents the identity morphism of a theory.
 * @param theory the theory
 */
object OMIDENT {
   val path = ModExp.identity
   def apply(theory : Term) : Term = OMA(OMID(this.path), List(theory))
   def unapply(t : Term) : Option[Term] = t match {
     case OMA(OMID(this.path), List(theory)) => Some(theory)
     case _ => None
   }
}

object ComplexMorphism {
   val path = ModExp.complexmorphism
   def apply(body: Substitution) = ComplexTerm(this.path, body, Context(), Nil)
   def unapply(t: Term) : Option[Substitution] = t match {
      case ComplexTerm(this.path, sub, Context(), Nil) => Some(sub)
      case _ => None
   }
}

object MUnion {
   val path = ModExp.munion
   def apply(morphs: List[Term]) : Term = morphs match {
      case Nil => Morph.empty
      case hd :: Nil => hd
      case hd :: tl => OMA(OMID(this.path), morphs)
   }
   def unapply(t : Term) : Option[List[Term]] = t match {
     case OMA(OMID(this.path), morphs) if (morphs.forall(_.isInstanceOf[Term])) => Some(morphs.asInstanceOf[List[Term]])
     case _ => None
   }
   /** applies associativity of union by merging nested MUnion */
   def associate(m: Term): List[Term] = m match {
     case MUnion(ms) => ms flatMap associate
     case m => List(m)
   }
}

