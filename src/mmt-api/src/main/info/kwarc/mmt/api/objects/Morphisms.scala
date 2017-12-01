package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects.Conversions._

//TODO definition expansion everywhere

object Morph {
  val empty = ComplexMorphism(Substitution())

  /** pre: m is a well-structured morphism */
  def domain(m: Term)(implicit lib: Lookup): Option[Term] = m match {
    case OMIDENT(t) => Some(t)
    case OMCOMP(n :: _) => domain(n)
    case OMMOD(path) => try {
      lib.get(path) match {
        case l: Link => Some(l.from)
      }
    } catch {
      case _: Exception => throw InvalidObject(m, "not a well-formed morphism")
    }
    case OMS(p) => try {
      val structure = lib.getStructure(p)
      Some(structure.from)
    } catch {
      case _: Exception => throw InvalidObject(m, "not a well-formed morphism")
    }
    case _ => None
  }

  /** pre: m is a well-structured morphism */
  def codomain(m: Term)(implicit lib: Lookup): Option[Term] = m match {
    case OMIDENT(t) => Some(t)
    case OMCOMP(l) if l.nonEmpty => codomain(l.last)
    case OMMOD(path) => try {
      lib.get(path) match {
        case l: Link => Some(l.to)
      }
    } catch {
      case _: Exception => throw InvalidObject(m, "not a well-formed morphism: " + m)
    }
    case OMS(p) => Some(OMMOD(p.module))
    case _ => None
  }

  /** transform a morphism into a list of morphisms by using associativity of composition */
  def associateComposition(m: Term): List[Term] = m match {
    case OMCOMP(ms) => ms flatMap associateComposition
    case _ => List(m)
  }

  /** pre: m is a valid morphism
    * post: m and simplify(m) are equal
    * inclusion and identity morphisms are simplified to OMCOMP()
    */
  def simplify(m: Term): Term = {
    val mS = OMCOMP(associateComposition(m))
    mS match {
      case OMMOD(p) => OMMOD(p)
      case OMS(p) => // TODO dangerous assumption that [mpath] is always a PlainInclude!
        if (p.name.length == 1 && p.name.steps.head.isInstanceOf[ComplexStep])
          OMCOMP()
        else
          OMS(p)
      case OMIDENT(t) => OMCOMP()
      case OMINST(t,args) =>
        if (args.isEmpty) OMCOMP() else m
      case OMCOMP(head :: ms) if ms.exists(OMINST.unapply(_).isDefined) =>
        // include/identity in a parametric theory is just include/identity again
        simplify(OMCOMP(head :: ms.filter(OMINST.unapply(_).isEmpty)))
      case OMCOMP(ms) =>
        val msS = (ms map simplify) filter {
          case OMIDENT(_) => false
          case OMCOMP(Nil) => false
          case _ => true
        }
        msS match {
          case Nil => OMCOMP()
          case h :: Nil => h
          case _ => OMCOMP(msS)
        }
    }
  }

  /** checks equality of two morphisms using the simplify method; sound but not complete */
  def equal(a: Term, b: Term): Boolean = simplify(a) == simplify(b)
}

/** an auxiliary class to store domains of [[Context]]s and theory expressions (see [[TheoryExp]])
  *
  * if parameter name is an import, subdomain is the imported theory and
  * (if not defined as in [[symbols.DeclaredStructure]] and [[StructureVarDecl]])
  * the instantiated names of the partial morphism
  *
  * @param name the name
  * @param defined true if name has a definiens (including total morphisms in [[symbols.DefinedStructure]] and [[StructureVarDecl]])
  * @param subdomain see above
  */
case class DomainElement(name: LocalName, defined: Boolean, subdomain: Option[(Term, List[LocalName])])

object TheoryExp {
  val empty = ComplexTheory(Context.empty)

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
        case _ => TUnion(tsS)
      }
    case _ => thy
  }

  /** checks equality of two theory expression using simplify */
  def equal(s: Term, t: Term) = simplify(s) == simplify(t)

  /** computes the meta-theories of a theory (non-reflexive), nearest one first
    *
    * @param all if true, stop after the first meta-theory, false by default
    */
  def metas(thy: Term, all: Boolean = true)(implicit lib: Lookup): List[MPath] = thy match {
    case OMMOD(p) => lib.getTheory(p) match {
      case t: DeclaredTheory => t.meta match {
        case None => Nil
        case Some(m) => if (all) m :: metas(OMMOD(m)) else List(m)
      }
      case t: DefinedTheory => metas(t.df)
    }
    case TUnion(ts) =>
      val ms = ts map { t => metas(t) }
      if (ms.nonEmpty && ms.forall(m => m == ms.head)) ms.head
      else Nil
  }

  /**
   * @param l left theory
   * @param r right theory
   * @return the union of l and r (formed by concatenating contexts)
   */
  def union(l: Term, r: Term) = (l, r) match {
    case (ComplexTheory(lC), ComplexTheory(rC)) => ComplexTheory(lC ++ rC)
  }

  /**
   * @param t a theory
   * @return the list of elements in the domain of t (not flattened)
   */
  def getDomain(t: Term)(implicit lib: Lookup): List[DomainElement] = t match {
    case OMMOD(p) => lib.getTheory(p) match {
      case d: DeclaredTheory =>
        d.getPrimitiveDeclarations.map {
          case s: symbols.DefinedStructure =>
            DomainElement(s.name, defined = true, Some((s.from, Nil)))
          case s: symbols.DeclaredStructure =>
            val definitions = s.getPrimitiveDeclarations.map(_.name)
            DomainElement(s.name, defined = false, Some((s.from, definitions)))
          case c: symbols.Constant =>
            DomainElement(d.name, c.df.isDefined, None)
          case pd =>
            DomainElement(pd.name, defined = true, None) //by default, no map allowed
        }
      case d: DefinedTheory => getDomain(d.df)
    }
    case ComplexTheory(body) =>
      body.getDomain
  }

  /** all inherently included named theories */
  // TODO properly
  def getSupport(t: Term): List[MPath] = t match {
    case OMMOD(p) => List(p)
    case ComplexTheory(cont) => cont.getIncludes
    case TUnion(ts) => (ts flatMap getSupport).distinct
  }

  /** returns a human-oriented (short) String representation of a theory expression */
  def toString(t: Term): String = t match {
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
  val instantiation = _path ? "theoryinstantiate"

  @deprecated("(still used by Twelf)")
  val tunion = _path ? "theory-union"
}

/** An OMMOD represents a reference to a module (which may be a theory or a morphism expression). */
object OMMOD {
  /** @param path the path to the module */
  def apply(path: MPath): OMID = OMID(path)

  def unapply(term: Term): Option[MPath] = term match {
    case OMID(m: MPath) => Some(m)
    case _ => None
  }
}

/** An OMPMOD represents a reference to a parametric module applied to arguments */
object OMPMOD {
  /** @param path the path to the module */
  def apply(path: MPath, args: List[Term]) = OMAorOMID(OMMOD(path), args)

  def unapply(term: Term): Option[(MPath, List[Term])] = term match {
    case OMAorOMID(OMMOD(m), args) => Some((m, args))
    case _ => None
  }
}

object TUnion {
  def apply(thys: List[Term]): Term = thys match {
    case hd :: Nil => hd
    case _ => OMA(OMID(ModExp.tunion), thys)
  }

  def unapply(union: Term): Option[List[Term]] = union match {
    case OMA(OMID(ModExp.tunion), thys) => Some(thys)

    case _ => None
  }

  /** applies associativity of union by merging nested TUnion */
  def associate(t: Term): List[Term] = t match {
    case TUnion(ts) => ts flatMap associate
    case _ => List(t)
  }
}

// TODO this should inherti from MutableElementContainer
class AnonymousTheory(val mt: Option[MPath], var decls: List[OML]) extends ElementContainer[OML] with DefaultLookup[OML] {
  def getDeclarations = decls
  
  def add(oml: OML, after: Option[LocalName] = None) {
    // TODO
  }
  def rename(old: LocalName, nw: LocalName) = {
    val i = decls.indexWhere(_.name == old)
    if (i != -1) {
      val ooml = decls(i)
      decls = decls.take(i) ::: OML(nw, ooml.tp, ooml.df, ooml.nt, ooml.featureOpt) :: decls.drop(i + 1)
    }
  }
  def toTerm = AnonymousTheory(mt, decls)
}

object AnonymousTheory {
  val path = ModExp.complextheory

  def apply(mt: Option[MPath], decls: List[OML]) = OMA(OMS(path), mt.map(OMMOD(_)).toList:::decls)
  def unapply(t: Term): Option[(Option[MPath],List[OML])] = t match {
    case OMA(OMS(this.path), OMMOD(mt)::OMLList(omls)) =>
      Some((Some(mt), omls))
    case OMA(OMS(this.path), OMLList(omls)) =>
      Some((None, omls))
    case _ => None
  }

  def fromTerm(t: Term) = unapply(t).map {case (m,ds) => new AnonymousTheory(m, ds)}
}

object OMLList {
  // awkward casting here, but this way the list is not copied; thus, converting back and forth between Term and AnonymousTheory is cheap
  def unapply(ts: List[Term]): Option[List[OML]] = {
    if (ts.forall(_.isInstanceOf[OML]))
      Some(ts.asInstanceOf[List[OML]])
    else
      None
  }
}

/** the normal form of a theory expression is a context
 *
 * the apply/unapply functions convert between them
 */

object ComplexTheory {
  val path = ModExp.complextheory

  def apply(body: Context) = body match {
    case Context(IncludeVarDecl(_, tp, None)) => tp
    case _ => OMBINDC(OMID(this.path), body, Nil)
  }

  def unapply(t: Term): Option[Context] = t match {
    case OMBINDC(OMID(this.path), body, Nil) => Some(body)
    case OMPMOD(p, args) => Some(Context(IncludeVarDecl(p, args)))
    case _ => None
  }
}

/** MorphType(from,to) is the type of morphisms */
object MorphType {
  val path = ModExp.morphtype

  def apply(from: Term, to: Term) = OMA(OMS(this.path), List(from, to))

  def unapply(t: Term): Option[(Term, Term)] = t match {
    case OMA(OMS(this.path), List(from, to)) => Some((from, to))
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

/** An OMM represents the application of a morphism to a term */
object OMM {
  val path = ModExp.morphismapplication

  /**
   * @param t the term
   * @param m the morphism
   */
  def apply(t: Term, m: Term): Term = OMA(OMS(this.path), List(t, m))

  def unapply(t: Term): Option[(Term, Term)] = t match {
    case OMA(OMS(this.path), List(a, m)) => Some((a, m))
    case _ => None
  }
}

/** An OMCOMP represents the associative composition of a list of morphisms. Compositions may be nested. */
object OMCOMP {
  val path = ModExp.composition

  /** the trivial morphism OMCOMP() is dropped from morphs
   *
   * @param morphs the list of morphisms in diagram order
   */
  def apply(morphs: List[Term]): Term = {
    val morphsNT = morphs.filterNot {
      _ == OMCOMP()
    }
    morphsNT match {
      case hd :: Nil => hd
      case _ => OMA(OMID(this.path), morphsNT)
    }
  }

  def apply(morphs: Term*): Term = apply(morphs.toList)

  def unapply(t: Term): Option[List[Term]] = t match {
    case OMA(OMID(this.path), morphs) => Some(morphs)
    case _ => None
  }
}

/** An OMIDENT represents the identity morphism of a theory. */
object OMIDENT {
  val path = ModExp.identity

  def apply(theory: Term): Term = OMA(OMID(this.path), List(theory))

  def unapply(t: Term): Option[Term] = t match {
    case OMA(OMID(this.path), List(theory)) => Some(theory)
    case _ => None
  }
}

object ComplexMorphism {
  val path = ModExp.complexmorphism

  def apply(body: Substitution) = ComplexTerm(this.path, body, Context(), Nil)

  def unapply(t: Term): Option[Substitution] = t match {
    case ComplexTerm(this.path, sub, Context(), Nil) => Some(sub)
    case _ => None
  }
}

/** An OMINST represents an instantiation of a parametric theory */
object OMINST {
  val path = ModExp.instantiation

  def apply(th : Term, pars : Term*) = OMA(OMS(this.path),th :: pars.toList)
  def unapply(morph : Term) : Option[(Term,List[Term])] = morph match {
    case OMA(OMS(`path`),th :: args) => Some((th,args))
    case _ => None
  }
}