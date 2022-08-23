package info.kwarc.mmt.api.libraries

import info.kwarc.mmt.api._
import frontend._
import documents._
import modules._
import objects._
import symbols._
import utils._

import scala.collection.mutable
import scala.ref.SoftReference

/** auxiliary class of [[Library]] to optimize storage
  *
  * This uses [[scala.ref.SoftReference]]s so that modules are automatically unloaded when memory is needed.
  */
class ModuleHashMap {
  private val underlying = new mutable.HashMap[MPath, SoftReference[Module]]
  def get(p: MPath): Option[Module] = {
    underlying.get(p).flatMap(_.get)
  }
  def update(p: MPath, m: Module): Unit = {
    val r = new SoftReference(m)
    underlying.update(p, r)
  }
  def -=(p: MPath): Unit = {
    underlying -= p
  }
  def keys = underlying.keys
  def values = underlying.values.flatMap(_.get)
  def clear: Unit = {
    underlying.clear
  }
  override def toString = {
    underlying.toList.sortBy(_._1.name.toPath).map { case (mp, r) =>
      val modString = r.get match {
        case Some(mod) => mod.toString
        case None => "[cleared]"
      }
      mp.toString + "\n" + modString
    }.mkString("\n")
  }
}

/** A Library is the in-memory representation of an MMT diagram, including the implicit-diagram, and all root documents.
  *
  * It implements the lookup and change of MMT URIs via get/add/update/delete/reorder methods. 
  * 
  * Invariant: This class guarantees structural well-formedness in the sense that
  * libraries conform to the MMT grammar and all declarations have canonical URIs.
  * The well-formedness of the objects in the declarations is not guaranteed.
  *
  * The [[Controller]] own a library and uses it to load elements into memory dynamically.
  * Therefore, access should always be through the corresponding methods of the controller.
  * Elements are unloaded dynamically if MMT runs out of memory.
  * 
  * @param extman the controller's extension manager
  * @param report parameter for logging.
  * @param previous a second library that stores the previous version whenever an element is changed
  */
class Library(extman: ExtensionManager, val report: Report, previous: Option[Library]) extends Lookup with Logger {self =>
  val logPrefix = "library"

  /** same as this but GetError instead of NotFound */
  val asLocalLookup = new LookupWithNotFoundHandler(this) with FailingNotFoundHandler {
    def forDeclarationsInScope(mod: Term)(f: (MPath,Term,Declaration) => Unit) = self.forDeclarationsInScope(mod)(f)
  }

  // ************************ stateful data structures and basic accessors

  /** all known root documents except for those induced by modules */
  private val documents = new scala.collection.mutable.HashMap[DPath,Document]
  /** all known root modules (which also induce root documents) */
  private val modules = new ModuleHashMap
  // private val computedModules = new scala.collection.mutable.HashMap[Term,Module]
  // private val generalizedMorphisms = new scala.collection.mutable.HashMap[Term,ExpressionTransformer]
  /** the diagram of implicit morphisms */
  private val implicitGraph = new ImplicitGraph

  override def toString = modules.values.map(_.toString).mkString("", "\n\n", "")

  /** returns all module paths indexed by this library */
  def getAllPaths = modules.keys

  /** retrieves all modules in any order */
  def getModules = modules.values

  
  private def ifActive[E <: StructuralElement](se: E): Option[se.type] = {
    if (InactiveElement.is(se)) None else Some(se)
  }
  
  /** direct lookup of p for mp = p / ln, also returns ln */
  private def modulesGetRoot(mp: MPath): (Module, LocalName) = {
    val top = mp.doc
    val name = mp.name
    val rootMod = name.prefixes.mapFind {n =>
       modules.get(top ? n)
    }.getOrElse{throw NotFound(mp)}
    val ln = name.drop(rootMod.name.length)
    (rootMod, ln)
  }

  /** direct lookup of p for dp = p / ln, also returns ln
   *  this considers both root documents  p / ln and root modules p / ln.init ? ln.head
   *  the longest known p is chosen in case of ambiguity
   */
  private def documentsGetRoot(dp: DPath): (Document, LocalName) = {
    val top = dp.^^
    val name = dp.name
    val rootDoc = name.prefixes.reverse.mapFind {n =>
       val topn = top / n
       documents.get(topn) orElse {
          if (n.nonEmpty) modules.get(topn.toMPath).map(m => seeAsDoc(m))
          else None // n may be empty if name is empty to begin with
       }
    }.getOrElse {throw NotFound(dp)}
    val ln = name.drop(rootDoc.path.name.length)
    (rootDoc,ln)
  }

  /** top level retrieval method */
  def get(p: Path): StructuralElement = {
     val error = (msg:String) => throw GetError(p, msg)
     val se = p match {
        case d: DPath => getNarrative(d)
        case p: MPath => getContent(p)
        case GlobalName(mp, n) => getDeclarationInTerm(OMMOD(mp), n)
        case c: CPath => throw GetError(c, "retrieval of components not possible")
     }
     ifActive(se).getOrElse {
       throw GetError(p, "element used to exist but is not valid anymore (this may also be caused by an illegal forward-reference)")
     }  
  }
  /** retrieval in a module expression */
  def get(home: Term, name: LocalName): Declaration = {
    val d = getDeclarationInTerm(home, name)
    ifActive(d).getOrElse {
       throw GetError(d.path, "declaration used to exist but is not valid anymore (this may also be caused by an illegal forward-reference)")
    }   
  }

  // ******************* document level retrieval
  /**
   *  dereferences a narrative URI d
   *  Retrieval starts in the root document doc such that d = doc.path / left.
   *  Then it dereferences left step-wise (from left to right), considering any nesting (e.g., documents or modules).
   *  seeAsDoc is used to turn intermediate retrieval results into [[Document]]s.
   *
   *  Note the similarity to getContent.
   */
  private def getNarrative(d: DPath): NarrativeElement = {
     /** step-wise looks up left in ne */
     def getNarrativeAux(ne: NarrativeElement, left: LocalName): NarrativeElement = {
        if (left.isEmpty) ne
        else {
            val doc = seeAsDoc(ne)
            val child = doc.getLocally(LocalName(left.head)).getOrElse {
               throw NotFound(doc.path / left.head)
               // no error here because the document may exist separately
               //error("no child " + left.head + " found in document " + doc.path)
            }
            getNarrativeAux(child, left.tail)
         }
     }
     // try root document
     val (doc, left) = documentsGetRoot(d)
     getNarrativeAux(doc, left)
  }

  /** tries to interpret a narrative element as a document
   *  in particular, this treats modules as documents, and dereferences NRefs
   */
  private def seeAsDoc(se: StructuralElement): Document = {
    def error(s: String) = throw GetError(se.path, s)
    se match {
      case d: Document => d
      case r: NRef => getO(r.target) match {
        case Some(se) => seeAsDoc(se)
        case None => error("referenced element does not exist: " + r.target)
      }
      case b: ModuleOrLink => b.asDocument
      case nm: NestedModule => seeAsDoc(nm.module)
      case _ => error("element exists but is not document-like")
    }
  }

  // ******************* module level retrieval

  /**
   *  dereferences a content URI p
   *  Retrieval starts in the root module mod such that p = mod.path / left.
   *  Then it dereferences left step-wise (from left to right), considering only the nesting of modules.
   *  seeAsMod is used to turn intermediate retrieval results into modules.
   *
   *  Note the similarity to getNarrative.
   */
  private def getContent(p: MPath): ContentElement = {
     /** step-wise looks up left in ce */
     def getContentAux(ce: ContentElement, left: LocalName): ContentElement = {
        if (left.isEmpty) ce
        else {
           val m = seeAsMod(ce)
           val d = getDeclarationInElement(m, Nil, LocalName(left.head))
           getContentAux(d, left.tail)
         }
     }
     val (mod, left) = modulesGetRoot(p)
     val ce = getContentAux(mod, left)
     ce match {
        case dd: DerivedDeclaration => dd
        // at this level, NestedModules are artifacts, so we unwrap the module if it's not a derived declaration
        case nm: NestedModule => nm.module
        case _ => ce
     }
  }
  /** tries to interpret a content element as a module
   *  In particular, [[NestedModule]]s are turned into [[Module]]s, which allows referencing into nested theory-like elements.
   */
  private def seeAsMod(ce: ContentElement): ModuleOrLink = ce match {
     case m: Module => m
     case nm: NestedModule => nm.module
     case s: Structure => s
     case dd: DerivedDeclaration => dd
     case _ => throw GetError(ce.path, "element exists but is not module-like")
  }

  // ******************* declaration level retrieval
  /** dereferences a declaration level reference
   *
   * Retrieval starts in the given module 'home'.
   * Then it dereferences 'name' step-wise, considering only the MMT module system, in particular elaboration.
   *
   * This method goes beyond URI dereferencing because the module may be a complex expression.
   * For plain URI dereferencing, the 'home' is simply an OMMOD.
   *
   * Defined modules are expanded, which allows referencing into their materialized body.
   *
   * @param home module expression that materializes into a [[Module]] or a [[NestedModule]]
   * @param name declaration name relative to that materialization
   * @param error error continuation
   */
  private def getDeclarationInTerm(home: Term, name: LocalName): Declaration = {
    /* convenience method for making an assignment that maps t?name to target; if target is omitted, this returns the identity assignment */
    def makeAssignment(t: Term, name: LocalName, target: Option[Term]) = get(t, name) match {
      case c: Constant => ConstantAssignment(home, name, Nil, target orElse Some(c.toTerm))
      case l: Structure => DefLinkAssignment(home, name, l.from, target getOrElse l.toTerm)
      case rc: RuleConstant => RuleConstant(home, name, target orElse rc.tp, None)
    }
    home match {
      // base case: lookup in atomic modules
      case OMPMOD(p, args) =>
         val mod = seeAsMod(getContent(p))
         val newName = name.steps match {
           case ComplexStep(`p`) :: r =>
             if (r.isEmpty)
               throw GetError(p, "cannot lookup element in itself")
             LocalName(r)
           case _ => name
         }
         getDeclarationInElement(mod, args, newName)
      // base case: lookup in atomic declaration
      case OMS(p) =>
         getO(p) match {
           case Some(ce: ContentElement) =>
             getDeclarationInElement(ce, Nil, name)
           case Some(e) =>
             throw GetError(p/name, "element " + e.path + " exists but cannot contain declarations")
           case None =>
             throw GetError(p/name, "containing declaration not found")
         }
      // complex cases: lookup in module expressions
      case ComplexTheory(cont) =>
        cont.mapVarDecls { case (before, vd) =>
          val vdDecl = vd.toDeclaration(ComplexTheory(before))
          vd match {
            case IncludeVarDecl(_, OMPMOD(p, args), _) =>
              name.head match {
                case ComplexStep(q) =>
                  // if name is visible to p, delegate to lookup in p
                  getImplicit(q, p).foreach {m =>
                    // we only need the existence of m to know in which direction to continue; translation along m will happen anyway
                    return get(OMPMOD(p,args), name)
                  }
                case _ =>
              }
            case StructureVarDecl(s, OMPMOD(p, args), dfOpt) =>
              name.head match {
                case s2@SimpleStep(_) if s == LocalName(s2) =>
                  val sym = getSymbol(p ? name.tail)
                  val struc = vdDecl.asInstanceOf[Structure]
                  val symT = translateByLink(sym, struc)
                  return symT
                case _ =>
              }
            case vd: VarDecl =>
              name.head match {
                case n2@SimpleStep(_) if vd.name == LocalName(n2) =>
                  val c = vdDecl.asInstanceOf[Constant]
                  return c
                case _ =>
              }
          }
        }

        // TODO we could throw a typing error here. But it's OK if the library succeeds on theories that aren't in scope.
        // added for LMFDB queries, where LMFDB theories might not be in the context of the query
        name.steps match {
          case ComplexStep(q)::ln =>
            return get(OMMOD(q), ln)
          case _ =>
        }
        throw GetError(ComplexTheory.path, s"name $name not found in " + cont)
      case OMCOMP(Nil) =>
        name match {
          case LocalName(ComplexStep(d) :: _) =>
            // infer domain of identity morphism
            get(OMIDENT(OMMOD(d)),name)
          case _ => throw GetError(OMCOMP.path, s"cannot look up $name in empty composition")
        }
      case OMCOMP(hd :: tl) =>
        val a = get(hd, name)
        if (tl.isEmpty)
          a
        else a match {
          case a: Constant => ConstantAssignment(home, a.name, a.alias, a.df.map(ApplyMorphs(_, OMCOMP(tl))))
          case DefLinkAssignment(ahome, aname, afrom, adf) => DefLinkAssignment(home, aname, afrom, OMCOMP(adf :: tl))
          case a: RuleConstant => RuleConstant(home, a.name, a.tp.map(ApplyMorphs(_, OMCOMP(tl))), None)
        }
      case OMIDENT(t) =>
        makeAssignment(t,name,None)
      case OMINST(f,t,_) =>
        // arguments are only relevant when looking up a declaration in the target, because this represents an include 
        makeAssignment(OMMOD(f),name,None)
      case OMStructuralInclude(f,t) =>
         val mod = seeAsMod(getContent(t))
         getAssignmentInModuleOrLink(mod, OMMOD(f), name)
      case Morph.empty => throw GetError(ComplexMorphism.path, s"cannot look up assignment for $name in empty morphism")
      case _ => throw GetError(home.head.getOrElse(utils.mmt.mmtcd), s"cannot look up $name in unknown module: " + home)
    }
  }

  /** auxiliary method of get for lookups in a parent that has already been retrieved */
  private def getDeclarationInElement(mod: ContentElement, args: List[Term], name: LocalName): Declaration = {
    def error(s: String) = throw GetError(mod.path, s"cannot get $name: $s")
    // only theories may have parameters for now
    mod match {
      case t: Theory =>
      case d =>
        if (0 != args.length)
          error("number of arguments does not match number of parameters")
    }
    // now the actual lookup
    mod match {
      case t: AbstractTheory =>
         // unifies theories and derived content elements
         t.df match {
           case Some(df) if !uom.ElaboratedElement.isPartially(t) =>
             // lookup in definiens if not elaborated yet; alternatively: call elaboration
             val d = getDeclarationInTerm(df, name)
             instantiate(d, t.parameters, args)
           case _ =>
             val d = getInTheory(t, args, name)
             d
         }
      case l: Link if l.df.isDefined =>
         // defined view or structure
         // TODO: lookup in elaboration if possible
         getDeclarationInTerm(l.df.get, name)
      case v: View =>
         // if v is partial, the returned declaration may have an empty definiens
         getInLink(v, name)
      case s: Structure =>
         val assig = getInLink(s, name)
         // structures are total: so we merge in a default definiens: s(n) = s/n
         def defaultDef = TermContainer(OMS(s.home.toMPath ? translateNameByLink(name, s)))
         assig match {
            case ca: Constant if ca.df.isEmpty =>
              new FinalConstant(ca.home, ca.name, ca.alias, ca.tpC, defaultDef, ca.rl, ca.notC, ca.vs)
            case sa: Structure if sa.df.isEmpty =>
              new Structure(sa.home, sa.name, sa.tpC, defaultDef, sa.isImplicit, sa.isTotal)
            case a => a
         }
      case nm: NestedModule =>
        getDeclarationInElement(nm.module, args, name)
      case e =>
        error(e.feature + " " + e.path + " declaration cannot contain declarations")
    }
  }

  /** auxiliary method of get for lookups in a [[Theory]] */
  private def getInTheory(t: AbstractTheory, args: List[Term], name: LocalName) = {
     def error(s: String) = throw GetError(t.path, s"cannot get $name: $s")
     val declLnOpt = t.getMostSpecific(name) map {
        case (d, ln) => (instantiate(d, t.parameters, args), ln)
     }
     declLnOpt match {
       case Some((d, LocalName(Nil))) => d // perfect match
       case Some((d, ln)) => d match {
         case Include(id) =>
           val sym = getDeclarationInTerm(OMPMOD(id.from,id.args),ln)
           id.df match {
             case Some(df) => translate(sym, df)
             case None => sym
           }
         // a prefix exists and resolves to d, a suffix ln is left
         case s:Structure =>
           val sym = getDeclarationInTerm(s.from, ln) // resolve ln in the domain of s
           translateByLink(sym, s) // translate sym along s
         case dd: DerivedDeclaration =>
            getInElaboration(t, dd, ln)
         case e =>
           error("local name " + ln + " left after resolving to " + e.path)
       }
       case None => name.steps match {
         // initial complex steps are possible even if no prefix of name is declared in t if t is not flattened
         case ComplexStep(mpath) :: ln =>
           getO(mpath) match {
             case Some(included: AbstractTheory) =>
               // continue lookup in (possibly implicitly) included theory
               val imp = implicitGraph(mpath, t match {
                 case t:Theory => t.path
                 case d:DerivedDeclaration => d.modulePath
               }) getOrElse {
                 error("no implicit morphism from " + mpath + " to " + t.path)
               }
               if (ln.isEmpty) {
                 imp match {
                   case OMINST(_, _, impargs) =>
                     Include(t.toTerm, mpath, impargs)
                   case OMIDENT(_) | OMCOMP(Nil) =>
                     Include(t.toTerm, mpath, Nil)
                   case _ =>
                     Include(t.toTerm, mpath, Nil, Some(imp)) // implicit morphism as a defined include
                 }
               } else {
                 val sym = getDeclarationInElement(included, Nil, ln)
                 val symT = translate(sym, imp) // translate the result along the implicit morphism
                 symT
               }
             case Some(l: Link) =>
               // continue lookup in domain of l
               val sym = getDeclarationInTerm(l.from, ln)
               translateByLink(sym, l) // translate the result along the link
             case Some(e) =>
               error("cannot resolve complex step " + mpath)
             case None =>
               error("cannot resolve " + mpath)
           }
         case Nil =>
           error("empty name not allowed")
         case _ => throw NotFound(t.modulePath ? name, Some(t.path)) // [[Storage]]s may add declarations to a theory dynamically, so we throw NotFound
       }
     }
  }

  /**
   * look up 'name' in elaboration of dd
   */
  private def getInElaboration(parent: AbstractTheory, dd: DerivedDeclaration, name: LocalName): Declaration = {
      def error(s: String) = throw GetError(parent.path, s"cannot get $name: $s")
      val sf = extman.get(classOf[StructuralFeature], dd.feature) getOrElse {
        error("structural feature " + dd.feature + " not known")
      }
      val elaboration = sf.elaborate(parent, dd)(None)
      elaboration.getMostSpecific(name) match {
        case Some((d: DerivedDeclaration, ln)) =>
          if (ln.isEmpty)
            d
          else
            getInElaboration(parent, d, ln)
        case Some((e,ln)) =>
          error("cannot elaborate " + ln + " after resolving to " + e.path)
        case None =>
          error("cannot resolve " + name + " in " + dd.path)
      }
  }

  /** convenience interface for getAssignmentInModuleOrLink */
  private def getInLink(l: Link, name: LocalName) = {
    getAssignmentInModuleOrLink(l, l.from, name)
  }
  /** auxiliary method of get to unify lookup theories, structures, and views
    * returns an empty Declaration (with only home and name set) if no assignment provided
    */
  private def getAssignmentInModuleOrLink(l: ModuleOrLink, from: Term, name: LocalName): Declaration = {
     def default = {
        val da = get(from, name) match {
          case c: Constant => Constant(l.toTerm, name, Nil, None, None, None)
          case d: Structure => new Structure(l.toTerm, name, d.tpC, new TermContainer, false, false)
          case rc: RuleConstant => new RuleConstant(l.toTerm, name, new TermContainer, None)
          case _ => throw ImplementationError(s"unimplemented default assignment (while looking up $name in link ${l.path})")
        }
        da.setOrigin(DefaultAssignment)
        da
     }
     val nameS = name.simplify
     l.getMostSpecific(nameS) match {
        case Some((a, LocalName(Nil))) =>
          a // perfect match
        case Some((a, ln)) => a match {
          case a: Constant =>
            throw GetError(a.path, s"local name $ln left after resolving $name to constant assignment")
          case a: Structure =>
            val aname = a.name
            val afrom = a.from
            val adf = a.df getOrElse {
              (l,a) match {
                case (_: AbstractTheory, Include(id)) if !(id.isRealization && id.df.isEmpty) =>
                  // if l is a theory that already includes afrom
                  // we have to be careful to avoid a cycle because the realization itself is found at this point if an assignment is missing
                  id.asMorphism
                case _ =>
                  throw GetError(a.path, s"different element found while trying to find assignment for $nameS (This may happen when trying to use a realization that is not total.)")
              }
            }
            val dom = afrom.toMPath
            val dfAssig = getDeclarationInTerm(adf, ComplexStep(dom)/ln)
            // dfAssig has the right definiens, but we need to change its home and name to fit the original request
            val h = dfAssig.name.head
            val prefix = if (h.isInstanceOf[ComplexStep] && aname.head == h) aname.init else aname
            dfAssig.translate(l.toTerm, prefix, IdentityTranslator, Context.empty)
        }
        case None =>
          // check if name lives in a theory theo included into from or directly in from; if the former, use an included morphism
          val (theo, tname) = nameS.steps match {
            case ComplexStep(theo)::tname =>
              if (from == OMMOD(theo))
                return default
              else
                (theo,tname)
            case _ => return default
          }
          def defaultMorph(p: MPath) = IncludeData(l.toTerm, p, Nil, Some(OMIDENT(OMMOD(p))), false)
          // for declarations of the meta-theory of the parent theory of l.from, we default to the identity if no other assignment is found
          val defaultMetaMorph = TheoryExp.metas(from, false)(this) map defaultMorph
          // note: if the parent theory is implicitly visible only, we need to apply the implicit morphism; see the corresponding case in the StructureChecker, which currently forbids this
          val defaultParentMorph = from match {
            case OMPMOD(fromP,_) => fromP.superModule.toList map defaultMorph
            case _ => Nil
          }
          // we look for the first assignment in l for a domain that includes theo
          // (there may be multiple, but they must be equal on theo if l well-formed)
          // defaultMetaMorph, being last, is only considered as a default
          (l.getAllIncludes ::: defaultMetaMorph ::: defaultParentMorph) foreach {
            case IncludeData(_,f,Nil,Some(m), false) =>
              val impl = getImplicit(theo, f)
              impl foreach {v =>
                // theo --v--> f --incl--> l.from --l--> l.to with l|_f == m; thus l|_theo == v;m
                val vm = OMCOMP(v,m)
                val vmA = if (tname.isEmpty) {
                  Include(l.toTerm, theo, Nil, Some(vm))
                } else {
                  getDeclarationInTerm(vm, nameS)
                }
                return vmA
              }
            case _ =>
              // can only happen if a theory has an undefined include
          }
          // otherwise, we use a default assignments
          return default
     }
  }

  /** instantiates parameters of a declaration with arguments
    *
    * the home of the new declarations is adapted
    */
  private def instantiate(decl: Declaration, params: Context, args: List[Term]): Declaration = {
    if (args.isEmpty) return decl // lookup from within parametric theory does not provide arguments
    val subs: Substitution = (params / args).getOrElse {
        throw GetError(decl.path, "number of arguments does not match number of parameters: " + params.length + " (" + params.map(_.name).mkString(", ") + ") given: " + args)
      }
    if (subs.isIdentity) return decl // avoid creating new instance
    val newHome = decl.home match {
        case OMPMOD(p, oldArgs) => OMPMOD(p, oldArgs ::: args)
        case _ => throw ImplementationError("cannot instantiate declaration of complex theory")
      }
    val dT = decl.translate(newHome, LocalName.empty, ApplySubs(subs), params)
    dT.setOrigin(CreatedForLookup)
    dT
  }

  /** translate a Declaration along a morphism */
  private def translate(d: Declaration, morph: Term): Declaration = {
    morph match {
      case OMMOD(v) =>
        val link = getView(v)
        translateByLink(d, link)
      case OMS(p) =>
        val link = getStructure(p)
        translateByLink(d, link)
      case OMIDENT(t) =>
        val imp = implicitGraph(d.home, t) getOrElse {
          throw GetError(d.path, "no implicit morphism to translate declaration to " + t)
        }
        //TODO better copy d and change home to t and name to d.home/d.name
        translate(d, imp)
      case OMINST(p,_,args) =>
        getDeclarationInTerm(OMPMOD(p,args), LocalName(d.path.module)/d.name)
      case OMStructuralInclude(f,t) =>
        val thy = getTheory(t)
        translateByModuleOrLink(d, thy, OMMOD(f))
      case OMCOMP(Nil) => d
      case OMCOMP(hd :: tl) =>
        translate(translate(d, hd), OMCOMP(tl))
      //TODO remaining cases
    }
  }

  /**
   * @param name name valid (possibly included) in the domain of l
   * @param l a link
   * @return [l] / name, but without name.head if simply refers to l.from
   */
  private def translateNameByLink(name: LocalName, l: Link): LocalName = {
    // the prefix of the new constant
    val nameRest = (name.steps,l.from) match {
      case (ComplexStep(p)::rest, OMMOD(q)) if p == q =>
        // rest is declared locally in l.from: namePrefix / rest
        LocalName(rest)
      case _ =>
        // in all other cases:  namePrefix / name
        name
    }
    l.namePrefix / nameRest
  }

  /** auxiliary method of translate to unify translation along structures and views */
  // if the morphism is partial, this returns a Constant with empty definiens;
  // for a structure, the definiens is the composed morphism (which may be defined for some constants even if it is not defined in general)
  private def translateByLink(decl: Declaration, l: Link) =
    translateByModuleOrLink(decl, l, l.from)
  private def translateByModuleOrLink(decl: Declaration, l: ModuleOrLink, from: Term): Declaration =
    l.df match {
      case Some(df) =>
        translate(decl, df)
      case None =>
        // if necessary, first translate decl along implicit morphism into l.from
        val imp = implicitGraph(decl.home, from) getOrElse {
          throw GetError(decl.path, "no implicit morphism to translate declaration to " + from)
        }
        val declT = translate(decl, imp)
        // get the assignment for declT provided by l (if any)
        val qualName = LocalName(declT.parent) / declT.name
        val assig = getAssignmentInModuleOrLink(l, from, qualName)
        val newName = l match {
          case l: Link => translateNameByLink(qualName, l)
          case t: AbstractTheory => qualName
        }
        def mapTerm(t: Term) = ApplyMorphs(t, l.toTerm, Context(from.toMPath))
        // make sure assignment has the expected feature (*)
        if (assig.feature != declT.feature) {
          throw InvalidElement(assig, s"link ${l.path} provides ${assig.feature} assignment for ${declT.feature} ${declT.path}")
        }
        val (namePrefix,to) = l match {
          case l: Link => (l.namePrefix,l.to)
          case t: AbstractTheory => (LocalName(declT.parent),t.toTerm)
        }
        // translate declT along assigOpt
        val newDecl = declT match {
          case c: Constant =>
            // c: original constant in domain
            // a: assignment provided by link
            // general idea: use components of a and fill omitted ones with translated counterparts of c
            val a = assig.asInstanceOf[Constant] // succeeds because of (*)
            val newAlias = a.alias ::: c.alias.map(namePrefix / _)
            val newTp = a.tp orElse c.tp.map(mapTerm)
            val newDef = a.df orElse c.df.map(mapTerm)
            val newRole = a.rl orElse c.rl
            // translating notations is the trickiest part, especially verbalization notations are experimental
            val newNotC = a.notC.copy
            // most important special case: copy over notation of c if a does not have one and it is unambiguous
            (a.notC.parsing, c.notC.parsing) match {
              case (None, Some(cn)) =>
                lazy val impl = l match {
                  case _: AbstractTheory => true
                  case l: Link => l.isImplicit
                }
                val cnR = if (impl) Some(cn) else {
                  // make inambiguous by inserting instance name
                  val fixR = cn.fixity.relativize
                  fixR map {f => cn.copy(fixity = f)}
                }
                cnR foreach {n =>
                  newNotC.update(ParsingNotationComponent, n)
                }
              case _ =>
            }
            Constant(to, newName, newAlias, newTp, newDef, newRole, newNotC)
          case s: Structure =>
            val a = assig.asInstanceOf[Structure] // succeeds because of (*)
            val newDef = a.df.getOrElse {
               OMCOMP(s.toTerm, l.toTerm) //TODO should result in DeclaredStructure containing a subset of the assignments in l
            }
            DefLinkAssignment(to, newName, s.from, newDef)
          case rc: RuleConstant =>
            val a = assig.asInstanceOf[RuleConstant] // succeeds because of (*)
            val newTp = a.tp orElse rc.tp.map(mapTerm)
            RuleConstant(to, newName, newTp, None)
        }
        newDecl.setOrigin(CreatedForLookup)
        newDecl
    }

  // ******************* additional retrieval methods

  /** all declarations that are visible (based on what is currently loaded) to a theory
   *  replaced with forDeclarationsInScope
   */
  /*
  def getDeclarationsInScope(mod: Term): List[StructuralElement] = {
   
    val impls = visibleVia(mod).toList
    impls.flatMap {
      case (OMMOD(from), OMCOMP(Nil)) =>
        get(from) match {
          case d: DeclaredTheory =>
            d.getDeclarations
          case _ => Nil //TODO materialize?
        }
      case (OMMOD(from), OMINST(from2, args)) if from == from2 =>
        get(from) match {
          case d: DeclaredTheory =>
            d.getDeclarations.map(c => getDeclarationInTerm(OMPMOD(from, args), c.name, s => throw GetError(s)))
          case _ => Nil //TODO materialize?
        }
      case (OMMOD(p), m) =>
        //TODO
        logError("not collecting declarations from " + p + ", which is visible via " + m )
        Nil
    }
  }*/

  def forDeclarationsInScope(mod: Term)(f: (MPath,Term,Declaration) => Unit): Unit = {
    val impls = visibleVia(mod).toList
    impls.foreach {
      case (p, m) =>
        val thO = get(p) match {
          case t: Theory =>
            Some(t)
          case dd: DerivedDeclaration =>
            Some(dd.module)
          case _ =>
            None//TODO materialize?
        }
        thO.foreach {th =>
          th.getDeclarations foreach {d => f(p, m, d)}
        }
      case _ =>
    }
  }

  /** retrieve the implicit morphism "from --implicit--> to" */
  def getImplicit(from: Term, to: Term): Option[Term] = implicitGraph(from, to)

  /** retrieve all implicit morphisms into a theory
    * @param to the theory
    * @return all pairs (theory,via) such that "via: theory --implicit--> to" (transitive and reflexive)
    */
  private def visibleVia(to: Term): List[(MPath, Term)] = {
    TheoryExp.getSupport(to) flatMap {p => implicitGraph.getTo(p,true)}
  }


  /** as visibleVia but dropping the implicit morphisms */
  def visible(to: Term) = visibleVia(to).map(_._1)

  /** as visible but only those where the morphism is trivial (i.e., all includes) */
  def visibleDirect(to: Term) = {
    val via = visibleVia(to)
    via.flatMap {
      case (from, OMCOMP(Nil)) => List(from)
      case (from, OMIDENT(_)) => List(from)
      case _ => Nil
    }
  }

  /** returns the symbol from which the argument symbol arose by a structure declaration */
  def preImage(p: GlobalName): Option[GlobalName] = p.name match {
    case hd / tl =>
      try {
        get(p.module ? hd) match {
          case s: Structure => Some(s.from.toMPath ? tl) //TODO
        }
      } catch {
        case _: Throwable => None
      }
    case !(hd) => None
    case _ => None
  }

  // ******************* state-changing interface: add, delete, update

  /** add, delete, and update must first locate the containing element
   *  Therefore, they share a lot of code, which is captured in this class.
   */
  private abstract class ChangeProcessor {
     def topError: Error
     def errorHandler[A](code: => A): A = try {code} catch {case l: LibraryError => throw topError.setCausedBy(l)}
     def primitiveDocument(dp: DPath): Unit
     def otherNarrativeElement(parent: Document, ln: LocalName): Unit
     def primitiveModule(mp: MPath): Unit
     def otherContentElement(parent: ModuleOrLink, ln: LocalName): Unit
     def component(cp: CPath, cont: ComponentContainer): Unit
     /** This does the relevant case distinction and then delegates to one of the abstract methods. */
     def apply(path: Path): Unit = {path match {
        case dp: DPath =>
           if (documents.isDefinedAt(dp))
             primitiveDocument(dp)
           else {
             val doc = errorHandler {seeAsDoc(getNarrative(dp ^))}
             otherNarrativeElement(doc, LocalName(dp.name.last))
           }
        case mp: MPath =>
           primitiveModule(mp)
        case GlobalName(p,ln) =>
            val se = errorHandler {seeAsMod(getContent(p))}
            se match {
              case b: ModuleOrLink =>
                 otherContentElement(b, ln)
              case _ => errorHandler {throw GetError(path, "parent does not resolve to container")}
            }
        case cp: CPath =>
           getO(cp.parent) foreach {se =>
              se.getComponent(cp.component).foreach {cont =>
                component(cp, cont)
              }
           }
     }}
  }

  // ******************* adding elements

  // TODO adding an explicit include that has already been generated by flattening currently causes an error
  // workaround: remove or reorder the include

  /**
    * Adds a [[StructuralElement]].
    *
    * If the element is a [[ContainerElement]] (incl. [[Structure]]s and [[PlainInclude]]s!),
    * you *must* call [[endAdd()]] sometime after calling this [[add()]] method.
    * Otherwise, you risk an inconsistent state of MMT.
    */
  def add(e: StructuralElement, at: AddPosition = AtEnd): Unit = {
    log("adding " + e.path + " (which is a " + e.feature + ")")
    val adder = new Adder(e, at)
    e match {
       case doc: Document if doc.root =>
          // special treatment for root documents, this case can't be detected by ChangeProcessor
          adder.primitiveDocument(doc.path)
       case ne: NarrativeElement if ne.name.length != 1 =>
          // this case is needed because ChangeProcessor assumes ne.name.length == 1 && ne.parentOpt = Some(ne.path ^)
          // can it be specified away?
          val parDoc = adder.errorHandler {seeAsDoc(getNarrative(ne.parentOpt.get))}
          adder.otherNarrativeElement(parDoc, ne.name)
       case _ =>
          adder.run
    }
    addImplicits(e, true)
  }
  
  /** add-related work that has to be done at the end of an element
   *  in particular: implicit morphisms are registered only when the inducing element has been checked 
   */
  def endAdd(c: ContainerElement[_]): Unit = {
    addImplicits(c, false)
  }

  /** the bureaucracy of adding 'se' */
  private class Adder(se: StructuralElement, at: AddPosition) extends ChangeProcessor {
     def errorFun(msg: String): LibraryError = AddError(se, msg)
     def topError = errorFun("unknown cause")
     def wrongType(exp: String): Unit = {errorFun("expected a " + exp + ", found " + se.feature)}
     def checkNoAfter: Unit = {
       if (at != AtEnd)
         errorFun("adding after a declaration only allowed in containers")
     }
     def run: Unit = {
        apply(se.path)
     }
     def primitiveDocument(dp: DPath) = {
        checkNoAfter
        se match {
           case doc: Document =>
             if (dp.^! == dp) documents(dp.^^) = doc
              documents(dp) = doc
           case _ =>
              wrongType("document")
        }
     }
     def otherNarrativeElement(doc: Document, ln: LocalName) = {
        se match {
           case ne: NarrativeElement =>
              doc.add(ne, at)
           case _ =>
              wrongType("narrative element")
        }
     }
     def primitiveModule(mp: MPath) = {
        checkNoAfter
        se match {
           case mod: Module =>
              modules.get(mp).foreach {m =>
                previous.foreach {_.modules(mp) = m}
                deleteImplicits(mod.path)
              }
              modules(mp) = mod
           case _ =>
              wrongType("module")
        }
     }
     def otherContentElement(body: ModuleOrLink, ln: LocalName) = {
        se match {
           case d: Declaration =>
              body.add(d, at)
           case _ =>
              wrongType("declaration")
        }
     }
     def component(cp: CPath, cont: ComponentContainer) = {
        checkNoAfter
        wrongType("content element") // should be impossible
     }
  }


  private def addImplicits(se: StructuralElement, before: Boolean): Unit = {
    se match {
      // before == true
      case t: AbstractTheory if before =>
        t.getAllIncludes foreach addIncludeToImplicit
      case dd: DerivedDeclaration if before =>
        implicitGraph.add(dd.home.toMPath, dd.modulePath, None)
      case e: NestedModule if before =>
        add(e.module)//, at)
        //TODO this makes every declaration in a theory T visible to any theory S nested in T, regardless of
        //  whether the declaration comes before or after S
        implicitGraph.add(e.home.toMPath, e.module.path, None)
      //TODO add equational axioms
      // before == false
      case Include(id) if !before  =>
        // elaboration includes do not have to be added to implicits because the implicitGraph computes them anyway
        if (!se.getOrigin.isInstanceOf[ElaborationOf]) {
          //println("implicit edge: " + id.from + " " + id.home + " " + id.asMorphism)
          id.home match {
            case OMMOD(h) =>
              get(h) match {
                case thy: AbstractTheory =>
                  val oldImpl = implicitGraph(OMMOD(id.from),id.home) flatMap {
                    case OMCOMP(Nil) => None
                    case i => Some(i)
                  }
                  (id.df.isEmpty,oldImpl) match {
                    case (true,Some(i)) =>
                    // an undefined include acquires an existing non-include implicit morphism as its definiens
                    // MoC design flaw because the implicit morphism may depend on a previous version of the theory
                    // but elaboration anyway handles this case now for the relevant case of include-induced morphisms
                    //c.asInstanceOf[Structure].dfC.analyzed = Some(i)
                    case _ =>
                      // this should be done at (*) below for realizations to avoid dependency cycles
                      // but it causes problems when includes are registered as implicit before the realization is encountered
                      //if (!id.isRealization)
                      //realizations are only added when totality has been checked at the end of the theory
                      addIncludeToImplicit(id)
                  }
                case _ =>
              }
            case _ =>
          }
        }
      case t: AbstractTheory if !before  =>
      // (*)
      //t.getRealizees foreach addIncludeToImplicit
      case l: Link if l.isImplicit && !before =>
        val fO = checkAtomic(se, l.from)
        val tO = checkAtomic(se, l.to)
        (fO,tO) match {
          case (Some(f),Some(t)) =>
            implicitGraph.add(f, t, Some(l.toTerm))
          case _ =>
        }

      case _ =>
    }
  }

  private def deleteImplicits(p: Path): Unit = {
    p match {
      case p: MPath => implicitGraph.delete(p)
      case _ =>
    }
  }

  private def checkAtomic(se: StructuralElement, t: Term) = t match {
    case OMPMOD(p,_) => Some(p)
    case OMSemiFormal(_) => None // this basically only happens when the parsing has previously failed; people would not expect this to be implicit
    case _ =>
      throw AddError(se, "implicit morphism must have atomic domain and codomain")
  }

  // shared code for adding an include
  private def addIncludeToImplicit(id: IncludeData): Unit = {
    val mor = if (!id.isRealization && id.args.isEmpty && !id.df.isDefined) None else Some(id.asMorphism)
    implicitGraph.add(id.from, id.home.toMPath, mor)
  }

  /* not used anymore
  private def alreadyDefinedHandler(e: StructuralElement)(code: => Unit) {
    try {code}
    catch {case AlreadyDefined(from, to, old, nw) =>
      logError(s"implicit morphism $nw from $from to $to induced by ${e.path} in conflict with existing implicit morphism $old")
      //throw AddError(s"implicit morphism $nw from $from to $to induced by ${e.path} in conflict with existing implicit morphism $old")
    }
  }*/

 // ******************* deleting elements

  /** deletes the element with the given URI
    *
    * @param path the path to the element to be deleted
    */
  def delete(path: Path): Unit = {
    deleteImplicits(path)
    new Deleter(path).apply(path)
  }
  
  def deactivate(se: StructuralElement): Unit = {
    InactiveElement.set(se)
    deleteImplicits(se.path)
  }
  
  def reactivate(se: StructuralElement): Unit = {
    InactiveElement.erase(se)
    addImplicits(se, true)
    addImplicits(se, false)
  }

  private val lib = this
  private class Deleter(path: Path) extends ChangeProcessor {
     def topError = DeleteError(path, "unknown cause")
     def primitiveDocument(dp: DPath) = {
        // delete all modules of this document
        documents.get(dp) foreach {doc =>
          doc.getModules(lib).foreach {mp =>
            primitiveModule(mp)
          }
        }
        // delete the document
        documents -= dp
     }
     def otherNarrativeElement(doc: Document, ln: LocalName) = {
        doc.delete(ln)
     }
     def primitiveModule(mp: MPath) = {
        modules.get(mp).foreach {m =>
          previous.foreach {_.modules(mp) = m}
        }
        modules -= mp
        //if (mp.name.length > 1) delete(mp.toGlobalName)
     }
     def otherContentElement(body: ModuleOrLink, ln: LocalName) = {
       body.delete(ln) foreach {s =>
         s.getComponents.foreach {case DeclarationComponent(comp, cont) =>
           if (cont.isDefined) notifyUpdated(s.path $ comp)
         }
       }
     }
     def component(cp: CPath, cont: ComponentContainer) = {
        cont.delete
        notifyUpdated(cp)
     }
  }

 // ******************* updating elements

  /** updates a StructuralElement */
  def update(e: StructuralElement): Unit = {
    log("updating " + e.path)
    deleteImplicits(e.path)
    new Updater(e).run
    addImplicits(e, true)
    addImplicits(e, false)
  }

  /** almost the same as Adder; the only overrides are replacing "add" and "Add" with "update" and "Update"
   * AddPosition is irrelevant
   */
  private class Updater(se: StructuralElement) extends Adder(se, AtEnd) {
     override def errorFun(msg: String) = UpdateError(se, msg)
     override def otherNarrativeElement(doc: Document, ln: LocalName) = {
        se match {
           case ne: NarrativeElement =>
              doc.update(ne)
           case _ =>
              wrongType("narrative element")
        }
     }
     override def otherContentElement(body: ModuleOrLink, ln: LocalName) = {
        se match {
           case d: Declaration =>
              body.update(d)
           case _ =>
              wrongType("declaration")
        }
     }
  }

  /** moves an element with a given path to the end of its parent document */
  def reorder(p: Path): Unit = {
    (new Reorderer(p)).apply(p)
  }
  private class Reorderer(path: Path) extends ChangeProcessor {
     def topError = GetError(path, "error while reordering")
     def primitiveDocument(dp: DPath): Unit = {}
     def otherNarrativeElement(doc: Document, ln: LocalName): Unit = {
        doc.reorder(ln)
     }
     def primitiveModule(mp: MPath): Unit = {}
     def otherContentElement(body: ModuleOrLink, ln: LocalName): Unit = {
        body.reorder(ln)
     }
     def component(cp: CPath, cont: ComponentContainer): Unit = {}
  }

  // change management

  /** marks all known dependent components as dirty
    *
    * This method is public because components may be changed from the outside.
    * In that case, this method may be called additionally to maintain invariants.
    *
    * When deleting a Symbol or Component, this is called automatically.
    *
    * @param p path to the component that was changed/deleted
    */
  def notifyUpdated(p: CPath): Unit = {
    log("updated: " + p)
    logGroup {
      // notify the definiens (if any) if a type changed
      if (p.component == TypeComponent) {
         getO(p.parent) foreach {
            se => se.getComponent(DefComponent) foreach {
               case df: TermContainer =>
                  if (df.isDefined) {
                    log("definiens needs recheck")
                    df.setAnalyzedDirty
                  }
               case _ =>
            }
         }
      }
      // notify all dependencies
      modules.values.foreach { case m: Module =>
        m.foreachComponent {
          case (comp, tc: TermContainer) =>
            if (tc.dependsOn contains p) {
              log("dependency needs recheck: " + comp)
              tc.setAnalyzedDirty
            }
          case _ =>
        }
      }
    }
  }

  // delete everything

  /** forgets everything */
  def clear: Unit = {
    modules.clear
    implicitGraph.clear
    documents.clear
    previous.map(_.clear)
  }
}

/**
 * if set, the element is deactivated
 */
object InactiveElement extends BooleanClientProperty[StructuralElement](utils.mmt.baseURI / "clientProperties" / "controller" / "status")
