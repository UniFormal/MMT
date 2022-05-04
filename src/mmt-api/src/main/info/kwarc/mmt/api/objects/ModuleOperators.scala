package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import symbols._
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.utils.MMT_TODO

//TODO definition expansion everywhere

object Morph {
  val empty = ComplexMorphism(Substitution())

  /** pre: m is a well-structured morphism */
  def domain(m: Term)(implicit lib: Lookup): Option[Term] = m match {
    case OMIDENT(t) => Some(t)
    case OMINST(f,_,_) => Some(OMMOD(f)) // covariant interpretation: domain is not OMPMOD(f,args) but simply OMMOD(f)
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
    case OMINST(_,t,_) => Some(OMMOD(t))
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
      case AnonymousMorphismCombinator(_) => mor
      case OMID(p) =>
        lib.getO(p) match {
          case Some(l: Link) =>
            // expand definition
            l.df match {
              case Some(d) => simplify(d, preserveType)
              case None =>
                // expand undefined includes into identities
                l match {
                  case Include(id) => id.asMorphism
                  case _ => mor
                }
            }
          case _ => mor
        }
      case OMIDENT(t) => if (preserveType) mor else OMCOMP()
      case OMINST(_) => mor
      case OMStructuralInclude(f,t) => mor
      case OMCOMP(ms) =>
        val parts = associateComposition(ms)
        var partsS = parts map {m => simplify(m,true)}
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
          m match {
            case OMID(p) =>
              cod match {
                case Some(OMMOD(t)) =>
                  val pt = p match {
                    case p: MPath => p ? ComplexStep(t) // p is view
                    case p: GlobalName => p / ComplexStep(t) // p is structure
                  }
                  // check if p contains an assignment to t (if cod = p.from, lib.getO will fail leading to the correct behavior)
                  val ptL = lib.getO(pt)
                  ptL match {
                    case Some(l: Structure) =>
                      // restrict m to t
                      l.df match {
                        case Some(mR) =>
                          // the smaller we keep the codomain, the better
                          if (!isInclude(mR)) {
                             val mRS = expandUnlessCircular(mR,m)
                             result ::= mRS
                          }
                        case None =>
                          result ::= m
                      }
                    case _ =>
                      result ::= m
                  }
                case _ =>
                  result ::= m
              }
            case OMIDENT(t) =>
              // an identity is needed only at the beginning because it defines the domain
              if (result.isEmpty) {
                result ::= m
              }
            case OMStructuralInclude(from,to) =>
              val append = cod match {
                case Some(OMMOD(f)) =>
                  // m = StructuralInclude(from,to)
                  // t -include-> from ; m: try to restrict m to t
                  lib.getO(to ? ComplexStep(f)) match {
                    case Some(Include(id)) =>
                      /* defined include f -> to: use definiens
                       * undefined include f -> to: m|_to must be identity, drop m
                       * realization f -> to: m|_to is restricted realization 
                       */
                      id.df match {
                        case Some(d) =>
                          val dS = expandUnlessCircular(d,m)
                          Some(dS)
                        case None =>
                          if (id.isRealization) {
                            Some(OMStructuralInclude(f,to))
                          } else {
                            None
                          }
                      }
                    case _ => Some(m)
                  }
                case _ => Some(m)
              }
              append foreach {m => 
                result ::= m
              }
            /* old version for previous structural includes 
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
              }*/
            case OMINST(f,t,as) =>
              val mergeWithPrevious = result.headOption flatMap {
                case OMINST(f2,t2,bs) =>
                  // merge two successive instantiations: OMINST(f2,t2,bs);OMINST(f,t,as) = OMINST(f2,t, bs[as])
                  val pThy = lib.getAs(classOf[Theory],f)
                  (pThy.parameters / as) map {asub =>
                     OMINST(f2,t, bs map {b => b ^? asub})
                  }
                case _ =>
                  None
              }
              mergeWithPrevious match {
                case Some(n) =>
                  result = n :: result.tail
                case None =>
                  // R ; OMINST(f,t,args) = R  if the parameters do not occur in the image of R
                  // that is definitely the case if codomain(R) != f: in that case codomain(R) is properly included into f so that R cannot refer to the parameters of f
                  cod match {
                    case Some(OMMOD(t)) =>
                      if (t == f)
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
      case m => m // anything is allowed in content that is still being checked
    }
  }

  /** auxiliary method of simplify:
    * if atom has definiens df, expanding it would yield a cycle if the definiens is just a default definiens that composes an include with atom
    * this method checks for this and retains atom in that case
    */
  private def expandUnlessCircular(df: Term, atom: Term)(implicit lib: Lookup) = {
    df match {
      case OMCOMP(ms) if ms contains atom => atom
      case _ => simplify(df)
    }
  }
  
  /** true if m is a morphism that reduces to the identity/include */
  def isInclude(m: Term)(implicit lib: Lookup): Boolean = m match {
    case OMIDENT(_) => true
    case OMCOMP(ms) => ms forall isInclude
    case OMS(p) => lib.get(p) match {
      case Include(id) => id.df.isEmpty
      case _ => false
    }
    case _ => false
  }
  
  /** pre/postcomposes implicit morphisms if any
   *  pre: from -mor-> to
   *  returns OMCOMP(b,mor,a) such that from -b-> dom(mor) -mor-> cod(mor) -a-> to for implicit morphisms b, a
   */
  def insertImplicits(mor: Term, from: Term, to: Term)(implicit lib: Lookup) = {
    if (mor == OMCOMP()) {
      lib.getImplicit(from,to).getOrElse(mor)
    } else {
      val dom = domain(mor)
      val before = dom.flatMap {d => lib.getImplicit(from, d)}
      val cod = codomain(mor)
      val after = cod.flatMap {c => lib.getImplicit(c, to)}
      OMCOMP(before.toList ::: mor :: after.toList:_*)
    }
  }


  /** checks equality of two morphisms using the simplify method; sound but not complete
   *  pre: a and b are well-formed, include all implicit morphisms, and have domain 'from'
   */
  def equal(a: Term, b: Term, from: Term, to: Term)(implicit lib: Lookup): Boolean = {
    if (a hasheq b) return true // optimization
    val List(aS,bS) = List(a,b) map {m =>
      val mI = Morph.insertImplicits(m, from, to)
      simplify(OMCOMP(OMIDENT(from),mI))
    }
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
    case TUnion(ts) =>
      // associativity, idempotence
      val (noArgs,withArgs) = ts.distinct.partition(_._2.isEmpty)
      // commutativity
      val noArgsC = noArgs.sortBy(_.hashCode)
      TUnion(noArgsC ::: withArgs)
    case ComplexTheory(cont) =>
      ComplexTheory(cont)
    case _ => thy
  }

  /** checks equality of two theory expression using simplify */
  def equal(s: Term, t: Term) = simplify(s) == simplify(t)

  /** computes the meta-theories of a theory (non-reflexive), nearest one first
    *
    * @param all if false, stop after the first meta-theory, true by default
    */
  def metas(thy: Term, all: Boolean = true)(implicit lib: Lookup): List[MPath] = thy match {
    case OMPMOD(p,_) =>
      val t = lib.getAs(classOf[AbstractTheory],p)
      t.meta match {
        case None => 
          Nil
        case Some(m) =>
          if (all) m :: metas(OMMOD(m)) else List(m)
      }
    case AnonymousTheoryCombinator(at) => at.mt.toList
    case TUnion(ts) =>
      val ms = ts map {t => metas(OMPMOD(t))}
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
      case d: Theory =>
        d.getPrimitiveDeclarations.map {
          case s: symbols.Structure =>
            val definitions = s.getPrimitiveDeclarations.map(_.name)
            DomainElement(s.name, defined = false, Some((s.from, definitions)))
          case c: symbols.Constant =>
            DomainElement(d.name, c.df.isDefined, None)
          case pd =>
            DomainElement(pd.name, defined = true, None) //by default, no map allowed
        }
    }
    case ComplexTheory(body) =>
      body.getDomain
  }

  /** all inherently included named theories */
  // TODO properly
  def getSupport(t: Term): List[MPath] = t match {
    case OMMOD(p) => List(p)
    case ComplexTheory(cont) => cont.getIncludes
  }

  /** returns a human-oriented (short) String representation of a theory expression */
  def toString(t: Term): String = t match {
    case OMPMOD(f,as) => toString((f,as))
    case TUnion(ts) => ts.map(toString).mkString("", " + ", "")
  }
  def toString(at: OMPMOD.AtomicTheory) = at._1.last + (if (at._2.nonEmpty) "(...)" else "")
}

object ModExp extends uom.TheoryScala {
  val _base = DPath(utils.URI("http", "cds.omdoc.org") / "urtheories")
  val _name = LocalName("ModExp")

  val anonymoustheory = _path ? "anonymoustheory"
  val anonymousmorphism = _path ? "anonymousmorphism"
  val anonymousdiagram = _path ? "anonymousdiagram"

  val theorytype = _path ? "theory"
  val morphtype = _path ? "morphism"
  val diagramtype = _path ? "diagram"

  val identity = _path ? "identity"
  val composition = _path ? "composition"
  val structuralinclude = _path ? "structuralinclude"
  val morphismapplication = _path ? "morphismapplication"
  val instantiation = _path ? "theoryinstantiate"

  @MMT_TODO("use anonymous theories")
  val complextheory = _path ? "complextheory"
  @MMT_TODO("use anonymous morphisms")
  val complexmorphism = _path ? "complexmorphism"
  @MMT_TODO("not needed but still used by Twelf")
  // legacy OMA(tunion,args) should be interpreted as TUnion(args), which represents it as a complextheory
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
  /** an applied theory is a theory ID with a list of arguments */
  type AtomicTheory = (MPath, List[Term])
  /** @param path the path to the module */
  def apply(path: MPath, args: List[Term]): Term = OMAorAny(OMMOD(path), args)
  def apply(at: AtomicTheory): Term = apply(at._1, at._2)

  def unapply(term: Term): Option[AtomicTheory] = term match {
    case OMAorAny(OMMOD(m), args) => Some((m, args))
    case _ => None
  }
}

/** represents a union of atomic theories as a ComplexTheory */
object TUnion {
  def apply(thys: List[OMPMOD.AtomicTheory]): Term = thys match {
    case hd :: Nil => OMPMOD(hd)
    case _ =>
      val ds = thys.map(thy => IncludeVarDecl(thy._1,thy._2))
      ComplexTheory(Context(ds: _*))
  }

  def unapply(union: Term): Option[List[OMPMOD.AtomicTheory]] = union match {
    case OMPMOD(p,as) => Some(List((p,as)))
    case ComplexTheory(ctx) =>
      val ats = ctx.variables.toList.map {
        case IncludeVarDecl(_,OMPMOD(p,as),None) => (p,as)
        case _ => return None
      }
      Some(ats)
    case _ => None
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

  def apply(params: Context) = {
    if (params.isEmpty) OMS(this.path)
    else ComplexTerm(this.path, Nil, params, Nil)
  }

  def unapply(t: Term): Option[Context] = t match {
    case OMS(this.path) => Some(Nil)
    case ComplexTerm(this.path, Substitution(), params, Nil) => Some(params)
    case _ => None
  }
}

object DiagramType {
  def apply(): Term = OMS(ModExp.diagramtype)
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
 * Covariant interpretation of theory parameters: a morphism  m:S(args) --> T is seen as a morphism args,m: S --> T 
 * Such morphisms occur in particular if S(args) is included into T, i.e., if m is the identity of S
 * This case is represented by OMINST(S,T,args). args must be well-typed relative to T.
 * invariant: args.nonEmpty (apply method allows it by using OMIDENT instead; unapply method does not match OMIDENT) 
 */
object OMINST {
  val path = ModExp.instantiation
  def apply(from: MPath, to: MPath, args: List[Term]) =
    if (args.isEmpty) OMIDENT(OMMOD(from)) else OMA(OMS(this.path), OMMOD(from):: OMMOD(to)::args)
  def unapply(morph : Term) : Option[(MPath,MPath,List[Term])] = morph match {
    case OMA(OMS(this.path), OMMOD(from) :: OMMOD(to) :: args) => Some((from,to,args))
    case _ => None
  }
}
