package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import symbols._
import info.kwarc.mmt.api.objects.Conversions._

//TODO definition expansion everywhere

object Morph {
  val empty = ComplexMorphism(Substitution())

  /** pre: m is a well-structured morphism */
  def domain(m: Term)(implicit lib: Lookup): Option[Term] = m match {
    case OMIDENT(t) => Some(t)
    case OMINST(p,args) => Some(OMMOD(p))
    case OMCOMP(n :: _) => domain(n)
    case OMStructuralInclude(f,_) => Some(OMMOD(f))
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
    case OMINST(p,args) => None
    case OMCOMP(l) if l.nonEmpty => codomain(l.last)
    case OMStructuralInclude(_,t) => Some(OMMOD(t))
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
  def associateComposition(mors: List[Term]): List[Term] = mors flatMap {
    case OMCOMP(ms) => associateComposition(ms)
    case m => List(m)
  }

  /** pre: mor is a valid morphism and includes all implicit morphisms
    * post: mor and simplify(m) are equal
    * @param mor the morphism to simplify
    * @param preserveType if true, identity morphisms that restrict the type domain are preserved if they cannot be reduced; otherwise, they become OMCOMP()
    */
  def simplify(mor: Term, preserveType: Boolean = false)(implicit lib: Lookup): Term = {
    mor match {
      case OMMOD(p) => mor
      case OMS(p) => // TODO dangerous assumption that [mpath] is always a PlainInclude!
        if (p.name.length == 1 && p.name.steps.head.isInstanceOf[ComplexStep])
          OMCOMP()
        else
          mor
      case OMIDENT(t) => if (preserveType) mor else OMCOMP()
      case OMINST(t,args) => mor
      case OMStructuralInclude(f,t) => mor
      case OMCOMP(ms) =>
        val parts = associateComposition(ms)
        var partsS = parts map {m => simplify(m,true)} //TODO can we skip simplification? Nothing much happens because there are no nested compositions anymore
        var left = associateComposition(partsS) // usually redundant, but simplification may occasionally introduce a composition
        var result: List[Term] = Nil
        // we go through all morphisms m in 'ms', building the simplification in 'result' (in reverse order)
        // we put R = OMCOMP(result.reverse) for the morphism processed so far
        // by default, we just prepend m to result, so that eventually R = mS
        // but for some m, we inspect the previous morphism (e.g., result.head) to see if we can reduce
        // in particular, we can reduce R ; m ---> R ; (m|_codomain(R)) whenever if codomain(R) is properly included into domain(m)
        // for that reason, precomposition with identity is not redundant: id_t ; m ---> m|_t
        while (left.nonEmpty) {
          val m = left.head
          left = left.tail
          // the codomain of R
          lazy val cod = result.headOption.flatMap(h => codomain(h))
          // TODO OMINST(p,args);m ---> OMINST(p, args * m) 
          m match {
            case OMID(p) =>
              cod match {
                case Some(OMMOD(t)) =>
                  val pt = p match {
                    case p: MPath => p ? ComplexStep(t)
                    case p: GlobalName => p / ComplexStep(t)
                  }
                  lib.getO(pt) match {
                    case Some(l: DefinedStructure) =>
                      // restrict l to t
                      result ::= l.df
                    case _ =>
                      result ::= m
                  }
                case _ =>
                  result ::= m
              }
            case OMIDENT(t) =>
              // an identity is needed only at the beginning because it defines the codomain
              if (result.nonEmpty) {
                result ::= m
              }
            case OMStructuralInclude(from,to) =>
              val mergeWithPreviousTo = result.headOption flatMap {
                case OMStructuralInclude(from1,to1) =>
                  if (to1 == from) {
                    // merge two successive structural includes: StructuralInclude(a,b);StructuralInclude(b,c)=StructuralInclude(a,c)
                    Some(OMStructuralInclude(from1,to))
                  } else {
                    // TODO some simplification is still possible here
                    None
                  }
                case _ => None
              }
              mergeWithPreviousTo match {
                case Some(n) =>
                  result = n :: result.tail
                case None => cod match {
                  case Some(OMMOD(t)) =>
                    TheoryExp.metas(OMMOD(from), false).headOption match {
                      case Some(fromMeta) => lib.getImplicit(t,fromMeta) match {
                        case Some(_) =>
                          // t is visible to metaFrom: R ; m = R 
                        case None =>
                          // t is visible to from but not to metaFrom: R ; m = R ; StructuralInclude(cod,to) 
                          result ::= OMStructuralInclude(t,to)
                      }
                      case None =>
                        // from has no meta-theory: R ; m = R ; StructuralInclude(cod,to)
                        result ::= OMStructuralInclude(t,to)
                    }
                  case _ =>
                    result ::= m
                }
              }
            case OMINST(p,as) =>
              val mergeWithPrevious = result.headOption flatMap {
                case OMINST(q,bs) =>
                  // merge two successive instantiations: OMINST(q,bs);OMINST(p,as) = OMINST(q, bs[as])
                  val pThy = lib.getAs(classOf[Theory],p)
                  (pThy.parameters / as) map {asub =>
                     OMINST(q, bs map {b => b ^? asub})
                  }
                case _ =>
                  None
              }
              mergeWithPrevious match {
                case Some(n) =>
                  result = n :: result.tail
                case None =>
                  // R ; OMINST(p,args) = R  if the parameters do not occur in the image of R
                  // that is definitely the case if codomain(R) != p: in that case codomain(R) is properly included into p so that R cannot refer to the parameters of p
                  cod match {
                    case Some(OMMOD(t)) =>
                      if (t == p)
                        result ::= m
                    case _ =>
                      result ::= m
                  }
              }
            case OMCOMP(_) =>
              throw ImplementationError("no nested compositions possible")
            case _ =>
              result ::= m
          }
        }
        result = result.reverse
        // remove the leading identity if it is not needed to preserve the type
        if (!preserveType) result = result.filter {
          case OMIDENT(_) => false
          case _ => true 
        }
        // OMCOMP disappears if result has length 1
        OMCOMP(result)
    }
  }

  /** checks equality of two morphisms using the simplify method; sound but not complete
   *  pre: a and b are well-formed, include all implicit morphisms, and have domain 'from'
   */
  def equal(a: Term, b: Term, from: Term)(implicit lib: Lookup): Boolean = {
    if (a hasheq b) return true // optimization
    val aS = simplify(OMCOMP(OMIDENT(from),a))
    val bS = simplify(OMCOMP(OMIDENT(from),a))
    aS == bS
  }
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
    * @param all if false, stop after the first meta-theory, true by default
    */
  def metas(thy: Term, all: Boolean = true)(implicit lib: Lookup): List[MPath] = thy match {
    case OMMOD(p) if p.name.steps.length > 1 => // Nested theory; consider the parent theory as meta-theory
      val parent = lib.getTheory(p.parent ? p.name.steps.init)
      if (all) {
        parent.path :: (lib.getTheory(p) match {
          case t : DeclaredTheory => t.meta match {
            case None => Nil
            case Some(m) => m :: metas(OMMOD(m))
          }
        }) ::: metas(OMMOD(parent.path))
      } else List(parent.path)
    case OMMOD(p) => lib.getTheory(p) match {
      case t: DeclaredTheory => t.meta match {
        case None => Nil
        case Some(m) => if (all) m :: metas(OMMOD(m)) else List(m)
      }
      case t: DefinedTheory => metas(t.df)
    }
    case AnonymousTheory(mt,_) => mt.toList
    case TUnion(ts) =>
      val ms = ts map { t => metas(t) }
      if (ms.nonEmpty && ms.forall(m => m == ms.head)) ms.head
      else Nil
    case _ => Nil
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
  val _base = DPath(utils.URI("http", "cds.omdoc.org") / "urtheories")
  val _name = LocalName("ModExp")

  val anonymoustheory = _path ? "anonymoustheory"
  val anonymousmorphism = _path ? "anonymousmorphism"
  val theorytype = _path ? "theory"
  val morphtype = _path ? "morphism"
  val identity = _path ? "identity"
  val composition = _path ? "composition"
  val structuralinclude = _path ? "structuralinclude"
  val morphismapplication = _path ? "morphismapplication"
  val instantiation = _path ? "theoryinstantiate"

  @deprecated("use anonymous theories", "")
  val complextheory = _path ? "complextheory"
  @deprecated("use anonymous morphisms", "")
  val complexmorphism = _path ? "complexmorphism"
  @deprecated("not needed but still used by Twelf", "")
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

/**
 * OMStructuralInclude(f,t): f -> t is the identity on the meta-theory of f (if any) and renames every declaration f?c in f to t?c
 * For now, f may not have includes.
 */
object OMStructuralInclude {
  val path = ModExp.structuralinclude
  def apply(from: MPath,to: MPath) = OMA(OMID(this.path), List(OMMOD(from), OMMOD(to)))
  def unapply(t: Term): Option[(MPath,MPath)] = t match {
    case OMA(OMID(this.path), List(OMMOD(f),OMMOD(t))) => Some((f,t))
    case _ => None
  }
}

object ComplexMorphism {
  val path = ModExp.complexmorphism
  def apply(body: Substitution) = ComplexTerm(this.path, body, Context.empty, Nil)
  def unapply(t: Term): Option[Substitution] = t match {
    case ComplexTerm(this.path, sub, Context(), Nil) => Some(sub)
    case _ => None
  }
}

/**
 * an auxiliary morphism used only internally
 * OMINST(S, args): S -> T iff OMPMOD(S,args) -include-> T
 * The left side is well-typed if we think of the parameters of S as a part of S (which works if S is the domain of a morphism).
 * invariant: args.nonEmpty (apply method allows it by using OMIDENT instead; unapply method does not match OMIDENT) 
 */
object OMINST {
  val path = ModExp.instantiation
  def apply(p: MPath, args: List[Term]) =
    if (args.isEmpty) OMIDENT(OMMOD(p)) else OMA(OMS(this.path), OMMOD(p)::args)
  def unapply(morph : Term) : Option[(MPath,List[Term])] = morph match {
    case OMA(OMS(this.path), OMMOD(p) :: args) => Some((p,args))
    case _ => None
  }
}
