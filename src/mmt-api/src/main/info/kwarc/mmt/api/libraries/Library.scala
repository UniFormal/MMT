package info.kwarc.mmt.api.libraries

import info.kwarc.mmt.api._
import frontend._
import documents._
import modules._
import objects._
import symbols._
import utils._

import scala.collection._
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
  def update(p: MPath, m: Module) {
    val r = new SoftReference(m)
    underlying.update(p, r)
  }
  def -=(p: MPath) {
    underlying -= p
  }
  def keys = underlying.keys
  def values = underlying.values.flatMap(_.get)
  def clear {
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

/** A Library represents an MMT theory graph.
  *
  * The Library implements the central structural algorithms, in particular lookup.
  * All access of the main data structures is through the library's get/add/update/delete methods.
  *
  * Invariance: This class guarantees structural well-formedness in the sense that
  * libraries conform to the MMT grammar and all declarations have canonical URIs.
  * The well-formedness of the objects in the declarations is not guaranteed.
  *
  * @param report parameter for logging.
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
  /** the diagram of implicit morphisms */
  private val implicitGraph = new ThinGeneratedCategory(asLocalLookup)

  override def toString = modules.values.map(_.toString).mkString("", "\n\n", "")

    /** returns all module paths indexed by this library */
  def getAllPaths = modules.keys

  /** retrieves all modules in any order */
  def getModules = modules.values

  
  /** direct lookup of p for mp = p / ln, also returns ln */
  private def modulesGetRoot(mp: MPath): (Module, LocalName) = {
    val top = mp.doc
    val name = mp.name
    val rootMod = name.prefixes.mapFind {n =>
       modules.get(top ? n)
    }.getOrElse {throw NotFound(mp)}
    val ln = name.drop(rootMod.name.length)
    (rootMod, ln)
  }

  /** direct lookup of p for dp = p / ln, also returns ln
   *  this considers both root documents  p / ln and root modules p / ln.init ? ln.head
   *  the longest known p is chosen in case of ambiguity
   */
  private def documentsGetRoot(dp: DPath, error: String => Nothing): (Document, LocalName) = {
    val top = dp.^^
    val name = dp.name
    val rootDoc = name.prefixes.reverse.mapFind {n =>
       val topn = top / n
       documents.get(topn) orElse {
          if (n.nonEmpty) modules.get(topn.toMPath).map(m => seeAsDoc(m, error))
          else None // n may be empty if name is empty to begin with
       }
    }.getOrElse {throw NotFound(dp)}
    val ln = name.drop(rootDoc.path.name.length)
    (rootDoc,ln)
  }

  /** top level retrieval method */
  def get(p: Path): StructuralElement = {
     val error = (msg:String) => throw GetError("error while retrieving " + p + ": " + msg)
     p match {
        case d: DPath => getNarrative(d, error)
        case p: MPath => getContent(p, error)
        case GlobalName(mp, n) => getDeclarationInTerm(OMMOD(mp), n, error)
        case c: CPath => throw GetError("retrieval of components not possible")
     }
  }
  /** retrieval in a module expression */
  def get(home: Term, name: LocalName, error: String => Nothing): Declaration = getDeclarationInTerm(home, name, error)

  // ******************* document level retrieval
  /**
   *  dereferences a narrative URI d
   *  Retrieval starts in the root document doc such that d = doc.path / left.
   *  Then it dereferences left step-wise (from left to right), considering any nesting (e.g., documents or modules).
   *  seeAsDoc is used to turn intermediate retrieval results into [[Document]]s.
   *
   *  Note the similarity to getContent.
   */
  private def getNarrative(d: DPath, error: String => Nothing): NarrativeElement = {
     /** step-wise looks up left in ne */
     def getNarrativeAux(ne: NarrativeElement, left: LocalName): NarrativeElement = {
        if (left.isEmpty) ne
        else {
            val doc = seeAsDoc(ne, error)
            val child = doc.getLocally(LocalName(left.head)).getOrElse {
               throw NotFound(doc.path / left.head)
               // no error here because the document may exist separately
               //error("no child " + left.head + " found in document " + doc.path)
            }
            getNarrativeAux(child, left.tail)
         }
     }
     // try root document
     val (doc, left) = documentsGetRoot(d, error)
     getNarrativeAux(doc, left)
  }

  /** tries to interpret a narrative element as a document
   *  in particular, this treats modules as documents, and dereferences NRefs
   */
  private def seeAsDoc(se: StructuralElement, error: String => Nothing): Document = se match {
     case d: Document => d
     case r: NRef => getO(r.target) match {
        case Some(se) => seeAsDoc(se, error)
        case None => error("referenced element does not exist: " + r.target)
     }
     case b: ModuleOrLink => b.asDocument
     case nm: NestedModule => seeAsDoc(nm.module, error)
     case _ => error("element exists but is not document-like: " + se.path)
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
  private def getContent(p: MPath, error: String => Nothing): ContentElement = {
     /** step-wise looks up left in ce */
     def getContentAux(ce: ContentElement, left: LocalName): ContentElement = {
        if (left.isEmpty) ce
        else {
           val m = seeAsMod(ce, error)
           val d = getDeclarationInElement(m, Nil, LocalName(left.head), error)
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
  // TODO Once structures are nested modules, this can return Module instead of ContentElement
  private def seeAsMod(ce: ContentElement, error: String => Nothing): ContentElement = ce match {
     case m: Module => m
     case nm: NestedModule => nm.module
     case s: Structure => s
     case dd: DerivedDeclaration => dd
     case _ => error("element exists but is not module-like: " + ce.path)
  }

  // ******************* declaration level retrieval
  private val sourceError = (s: String) => throw GetError("error while looking up source declaration: "+s)

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
  private def getDeclarationInTerm(home: Term, name: LocalName, error: String => Nothing): Declaration = {
    /* convenience method for making an assignment that maps t?name to target; if target is omitted, this returns the identity assignment */
    def makeAssignment(t: Term, name: LocalName, target: Option[Term]) = get(t, name, error) match {
      case c: Constant => ConstantAssignment(home, name, Nil, target orElse Some(c.toTerm))
      case l: Structure => DefLinkAssignment(home, name, l.from, target getOrElse l.toTerm)
      case rc: RuleConstant => RuleConstant(home, name, target orElse rc.tp, None)
    }
    home match {
      // base case: lookup in atomic modules
      case OMPMOD(p, args) =>
         val mod = seeAsMod(getContent(p, error), error)
         val newName = name.steps match {
           case ComplexStep(`p`) :: r =>
             if (r.isEmpty)
               throw GetError("cannot lookup " + p + " in itself")
             LocalName(r)
           case _ => name
         }
         getDeclarationInElement(mod, args, newName, error)
      // base case: lookup in atomic declaration
      case OMS(p) =>
         getO(p) match {
           case Some(ce: ContentElement) =>
             getDeclarationInElement(ce, Nil, name, error)
           case Some(e) =>
             error("element exists but cannot contain declarations: " + e.path)
           case None =>
             error("containing declaration not found: " + p)
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
                    return get(OMPMOD(p,args), name, error)
                  }
                case _ =>
              }
            case StructureVarDecl(s, OMPMOD(p, args), dfOpt) =>
              name.head match {
                case s2@SimpleStep(_) if s == LocalName(s2) =>
                  val sym = getSymbol(p ? name.tail)
                  val struc = vdDecl.asInstanceOf[Structure]
                  val symT = translateByLink(sym, struc, error)
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
            return get(OMMOD(q), ln, error)
          case _ =>
        }
        throw GetError("name " + name + " not found in " + cont)
      case TUnion(ts) => ts mapFind { t =>
        getO(t,name)
      } getOrElse {
        error("union of theories has no declarations except includes")
      }
      case OMCOMP(Nil) =>
        name match {
          case LocalName(ComplexStep(d) :: _) =>
            // infer domain of identity morphism
            get(OMIDENT(OMMOD(d)),name,error)
          case _ => throw GetError("cannot look up " + name + " in empty composition")
        }
      case OMCOMP(hd :: tl) =>
        val a = get(hd, name, error)
        if (tl.isEmpty)
          a
        else a match {
          case a: Constant => ConstantAssignment(home, a.name, a.alias, a.df.map(OMM(_, OMCOMP(tl))))
          case DefLinkAssignment(ahome, aname, afrom, adf) => DefLinkAssignment(home, aname, afrom, OMCOMP(adf :: tl))
          case a: RuleConstant => RuleConstant(home, a.name, a.tp.map(OMM(_, OMCOMP(tl))), None)
        }
      case OMIDENT(t) =>
        makeAssignment(t,name,None)
      case OMINST(p,_) =>
        // arguments are only relevant when looking up a declaration in the target, because this represents an include 
        makeAssignment(OMMOD(p),name,None)
      case OMStructuralInclude(f,t) =>
        val target = name.steps match {
          case ComplexStep(`f`) :: ln =>
            Some(OMS(t ? name))
          case _ => None
        }
        makeAssignment(OMMOD(f), name, target)
      case Morph.empty => error("empty morphism has no assignments")
      case _ => error("unknown module: " + home)
    }
  }

  /** auxiliary method of get for lookups in a parent that has already been retrieved */
  private def getDeclarationInElement(mod: ContentElement, args: List[Term], name: LocalName, error: String => Nothing): Declaration = {
    // only theories may have parameters for now
    mod match {
      case t: Theory =>
      case d =>
        if (0 != args.length)
          throw GetError("number of arguments does not match number of parameters")
    }
    // now the actual lookup
    mod match {
      case t: AbstractTheory =>
         // unifies theories and derived content elements
         t.df match {
           case Some(df) if !uom.ElaboratedElement.isPartially(t) =>
             // lookup in definiens if not elaborated yet; alternatively: call elaboration
             val d = getDeclarationInTerm(df, name, error)
             instantiate(d, t.parameters, args)
           case _ =>
             getInTheory(t, args, name, error)
         }
      case l: Link if l.df.isDefined =>
         // defined view or structure
         // TODO: lookup in elaboration if possible
         getDeclarationInTerm(l.df.get, name, error)
      case v: View =>
         // if v is partial, the returned declaration may have an empty definiens
         getInLink(v, name, error)
      case s: Structure =>
         val assig = getInLink(s, name, error)
         // structures are total: so we merge in a default definiens: s(n) = s/n
         def defaultDef = TermContainer(OMS(s.home.toMPath ? translateNameByLink(name, s)))
         assig match {
            case ca: Constant if ca.df.isEmpty =>
              new FinalConstant(ca.home, ca.name, ca.alias, ca.tpC, defaultDef, ca.rl, ca.notC, ca.vs)
            case sa: Structure if sa.df.isEmpty =>
              new Structure(sa.toTerm, sa.name, sa.tpC, defaultDef, sa.isImplicit)
            case a => a
         }
      case nm: NestedModule =>
        getDeclarationInElement(nm.module, args, name, error)
      case e =>
        error(e.feature + " declaration cannot contain declarations: " + e.path)
    }
  }

  /** auxiliary method of get for lookups in a [[Theory]] */
  private def getInTheory(t: AbstractTheory, args: List[Term], name: LocalName, error: String => Nothing) = {
     val declLnOpt = t.getMostSpecific(name) map {
        case (d, ln) => (instantiate(d, t.parameters, args), ln)
     }
     declLnOpt match {
       case Some((d, LocalName(Nil))) => d // perfect match
       case Some((d, ln)) => d match {
         case Include(_,p,as) =>
           getDeclarationInTerm(OMPMOD(p,as),ln,error)
         // a prefix exists and resolves to d, a suffix ln is left
         case s:Structure =>
           val sym = getDeclarationInTerm(s.from, ln, sourceError) // resolve ln in the domain of s
           translateByLink(sym, s, error) // translate sym along l
         case dd: DerivedDeclaration =>
            getInElaboration(t, dd, ln, error)
         case e =>
           error("local name " + ln + " left after resolving to " + e.path)
       }
       case None => name.steps match {
         // initial complex steps are possible even if no prefix of name is declared in t
         case ComplexStep(mpath) :: ln =>
           getO(mpath) match {
             case Some(included: Theory) =>
               // continue lookup in (possibly implicitly) included theory
               val imp = implicitGraph(OMMOD(mpath), t.toTerm) getOrElse {
                 error("no implicit morphism from " + mpath + " to " + t.path)
               }
               if (ln.isEmpty) {
                 imp match {
                   case OMINST(_, impargs) =>
                     Include(t.toTerm, mpath, impargs)
                   case OMIDENT(_) | OMCOMP(Nil) =>
                     Include(t.toTerm, mpath, Nil)
                   case _ =>
                     Include(t.toTerm, mpath, Nil, Some(imp)) // implicit morphism as a defined include
                 }
               } else {
                 val sym = getDeclarationInElement(included, Nil, ln, sourceError)
                 translate(sym, imp, error) // translate the result along the implicit morphism
               }
             case Some(l: Link) =>
               // continue lookup in domain of l
               val sym = getDeclarationInTerm(l.from, ln, sourceError)
               translateByLink(sym, l, error) // translate the result along the link
             case Some(e) =>
               error("cannot resolve complex step " + mpath)
             case None =>
               error("cannot resolve " + mpath)
           }
         case Nil =>
           throw GetError("empty name not allowed")
         case _ => throw NotFound(t.modulePath ? name, Some(t.path)) // [[Storage]]s may add declarations to a theory dynamically, so we throw NotFound
       }
     }
  }

  /**
   * look up 'name' in elaboration of dd
   */
  private def getInElaboration(parent: AbstractTheory, dd: DerivedDeclaration, name: LocalName, error: String => Nothing): Declaration = {
      val sf = extman.get(classOf[StructuralFeature], dd.feature) getOrElse {
        error("structural feature " + dd.feature + " not known")
      }
      val elaboration = sf.elaborate(parent, dd)
      elaboration.getMostSpecific(name) match {
        case Some((d: DerivedDeclaration, ln)) =>
          if (ln.isEmpty)
            d
          else
            getInElaboration(parent, d, ln, error)
        case Some((e,ln)) =>
          error("cannot elaborate " + ln + " after resolving to " + e.path)
        case None =>
          error("cannot resolve " + name + " in " + dd.path)
      }
  }

  /** auxiliary method of get to unify lookup in structures and views
    * returns an empty Declaration (with only home and name set) if no assignment provided
    */
  private def getInLink(l: Link, name: LocalName, error: String => Nothing): Declaration = {
     def default = {
        val da = get(l.from, name, sourceError) match {
          case c: Constant => Constant(l.toTerm, name, Nil, None, None, None)
          case d: Structure => new Structure(l.toTerm, name, d.tpC, new TermContainer, false)
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
            error("local name " + ln + " left after resolving to constant assignment")
          case DefLinkAssignment(_, aname, afrom, adf) =>
            val dom = afrom.toMPath
            val dfAssig = getDeclarationInTerm(adf, ComplexStep(dom)/ln, error)
            // dfAssig has the right definiens, but we need to change its home and name to fit the original request
            val h = dfAssig.name.head
            val prefix = if (h.isInstanceOf[ComplexStep] && aname.head == h) aname.init else aname
            dfAssig.translate(l.toTerm, prefix, IdentityTranslator, Context.empty)
        }
        case None =>
          val (theo, tname) = nameS.steps match {
            case ComplexStep(theo)::tname => (theo,tname)
            case _ => return default
          }
          // for declarations of the meta-theory of the parent theory of l.from, we default to the identity if no other assignment is found
          val defaultMetaMorph = TheoryExp.metas(l.from, false)(this).headOption.toList map {mt => 
            (mt, OMIDENT(OMMOD(mt)))
          }
          val defaultParentMorph = l.from match {
            case OMPMOD(fromP,_) => fromP.superModule.toList.map {par =>
              // note: if the parent theory is implicitly visible only, we need to apply the implicit morphism; see the corresponding case in the StructureChecker, which currently forbids this
              (par, OMIDENT(OMMOD(par)))
            }
            case _ => Nil
          }
          // we look for the first assignment in l for a domain that includes theo
          // (there may be multiple, but they must be equal on theo if l well-formed)
          // defaultMetaMorph, being last, is only considered as a default
          (l.getIncludes ::: defaultMetaMorph ::: defaultParentMorph) foreach {case (f,m) =>
            val impl = getImplicit(theo, f)
            impl foreach {v =>
               // theo --v--> f --incl--> l.from --l--> l.to with l|_f == m; thus l|_theo == v;m
               return getDeclarationInTerm(OMCOMP(v,m), name, error)
            }
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
        throw GetError("number of arguments does not match number of parameters of " + decl.path + ": " + params.length + " (" + params.map(_.name).mkString(", ") + ") given: " + args)
      }
    if (subs.isIdentity) return decl // avoid creating new instance
    val newHome = decl.home match {
        case OMPMOD(p, oldArgs) => OMPMOD(p, oldArgs ::: args)
        case _ => throw ImplementationError("cannot instantiate declaration of complex theory")
      }
    val dT = decl.translate(newHome, LocalName.empty, ApplySubs(subs), params)
    dT
  }

  /** translate a Declaration along a morphism */
  private def translate(d: Declaration, morph: Term, error: String => Nothing): Declaration = {
    morph match {
      case OMMOD(v) =>
        val link = getView(v)
        translateByLink(d, link, error)
      case OMS(p) =>
        val link = getStructure(p)
        translateByLink(d, link, error)
      case OMIDENT(t) =>
        val imp = implicitGraph(d.home, t) getOrElse {
          throw GetError("no implicit morphism from " + d.home + " to " + t)
        }
        //TODO better copy d and change home to t and name to d.home/d.name
        translate(d, imp, error)
      case OMINST(p,args) =>
        getDeclarationInTerm(OMPMOD(p,args), LocalName(d.path.module)/d.name, error)
      case OMStructuralInclude(f,t) =>
        val ren = Renamer(p => if (p.module == f) Some(t ? p.name) else None)
        d.translate(TraversingTranslator(ren), Context.empty)
      case OMCOMP(Nil) => d
      case OMCOMP(hd :: tl) =>
        translate(translate(d, hd, error), OMCOMP(tl), error)
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
  private def translateByLink(decl: Declaration, l: Link, error: String => Nothing): Declaration =
    l.df match {
      case Some(df) =>
        translate(decl, df, error)
      case None =>
        // if necessary, first translate decl along implicit morphism into l.from
        val imp = implicitGraph(decl.home, l.from) getOrElse {
          throw GetError("no implicit morphism from " + decl.home + " to " + l.from)
        }
        val declT = translate(decl, imp, error)
        // get the assignment for declT provided by l (if any)
        val qualName = ComplexStep(declT.parent) / declT.name
        val assig = getInLink(l, qualName, error)
        val newName = translateNameByLink(qualName, l)
        // TODO is it better to use lazy morphism application?
        def mapTerm(t: Term) = ApplyMorphs(t, l.toTerm, Context(l.from.toMPath)) //t * l.toTerm
        // make sure assignment has the expected feature (*)
        if (assig.feature != declT.feature) {
          throw InvalidElement(assig, s"link ${l.path} provides ${assig.feature} assignment for ${declT.feature} ${declT.path}")
        }
        // translate declT along assigOpt
        val newDecl = declT match {
          case c: Constant =>
            val a = assig.asInstanceOf[Constant] // succeeds because of (*)
            val newAlias = a.alias ::: c.alias.map(l.namePrefix / _)
            val newTp = a.tp orElse c.tp.map(mapTerm)
            val newDef = a.df orElse c.df.map(mapTerm)
            if (a.path.toString.contains("comp") && a.path.toString.contains("add"))
              true
            val newNotC = a.notC merge c.notC
            val newRole = a.rl orElse c.rl
            Constant(l.to, newName, newAlias, newTp, newDef, newRole, newNotC)
          case s: Structure =>
            val a = assig.asInstanceOf[Structure] // succeeds because of (*)
            val newDef = a.df.getOrElse {
               OMCOMP(s.toTerm, l.toTerm) //TODO should result in DeclaredStructure containing a subset of the assignments in l
            }
            DefLinkAssignment(l.to, newName, s.from, newDef)
          case rc: RuleConstant =>
            val a = assig.asInstanceOf[RuleConstant] // succeeds because of (*)
            val newTp = a.tp orElse rc.tp.map(mapTerm)
            RuleConstant(l.to, newName, newTp, None)
        }
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

  def forDeclarationsInScope(mod: Term)(f: (MPath,Term,Declaration) => Unit) {
    val impls = visibleVia(mod).toList
    impls.foreach {
      case (OMMOD(p), m) =>
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

  /** set the implicit morphism "from --implicit--> to" */
  // TODO should be private, public only because of Archive.readRelational
  def addImplicit(from: Term, to: Term, morph: Term) {
    implicitGraph(from, to) = morph
  }

  /** retrieve all implicit morphisms into a theory
    *
    * @param to the theory
    * @return all pairs (theory,via) such that "via: theory --implicit--> to" (transitive and reflexive)
    */
  private def visibleVia(to: Term): mutable.HashSet[(Term, Term)] = {
    val hs = implicitGraph.into(to)
    TheoryExp.getSupport(to) foreach {p => hs add(OMMOD(p), OMCOMP())}
    hs
  }


  /** as visibleVia but dropping the implicit morphisms */
  def visible(to: Term): mutable.HashSet[Term] = visibleVia(to).map(_._1)

  /** as visible but only those where the morphism is trivial (i.e., all includes) */
  def visibleDirect(to: Term): mutable.HashSet[Term] = {
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
     def errorFun(msg: String): Nothing
     def primitiveDocument(dp: DPath): Unit
     def otherNarrativeElement(parent: Document, ln: LocalName): Unit
     def primitiveModule(mp: MPath): Unit
     def otherContentElement(parent: ModuleOrLink, ln: LocalName): Unit
     def component(cp: CPath, cont: ComponentContainer): Unit
     /** This does the relevant case distinction and then delegates to one of the abstract methods. */
     def apply(p: Path) {p match {
        case dp: DPath =>
           if (documents.isDefinedAt(dp))
             primitiveDocument(dp)
           else {
             val doc = seeAsDoc(getNarrative(dp ^, errorFun), errorFun)
             otherNarrativeElement(doc, LocalName(dp.name.last))
           }
        case mp: MPath =>
           primitiveModule(mp)
        case GlobalName(p,ln) =>
            val se = seeAsMod(getContent(p, errorFun), errorFun)
            se match {
              case b: ModuleOrLink =>
                 otherContentElement(b, ln)
              case _ => errorFun("parent does not resolve to container " + se.path)
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

  /**
   * adds a declaration
   * @param e the added declaration
   */
  def add(e: StructuralElement, at: AddPosition = AtEnd) {
    log("adding " + e.path + " (which is a " + e.feature + ")")
    val adder = new Adder(e, at)
    e match {
       case doc: Document if doc.root =>
          // special treatment for root documents, this case can't be detected by ChangeProcessor
          adder.primitiveDocument(doc.path)
       case ne: NarrativeElement if ne.name.length != 1 =>
          // this case is needed because ChangeProcessor assumes ne.name.length == 1 && ne.parentOpt = Some(ne.path ^)
          // can it be specified away?
          val parDoc = seeAsDoc(getNarrative(ne.parentOpt.get, adder.errorFun), adder.errorFun)
          adder.otherNarrativeElement(parDoc, ne.name)
       case _ =>
          adder.run
    }
    alreadyDefinedHandler(e) {
      e match {
        case t: Theory =>
          t.getAllIncludes foreach {case (p,args) =>
             addIncludeToImplicit(t.toTerm, p, args)
          }
        case dd: DerivedDeclaration =>
          implicitGraph(dd.home, OMMOD(dd.modulePath)) = OMIDENT(dd.home)
        case e: NestedModule =>
          add(e.module, at)
          //TODO this makes every declaration in a theory T visible to any theory S nested in T, regardless of
          //  whether the declaration comes before or after S
          implicitGraph(e.home, e.module.toTerm) = OMIDENT(e.home)
        case _ =>
        //TODO add equational axioms
      }
    }
  }
  
  /** add-related work that has to be done at the end of an element */
  def endAdd(c: ContainerElement[_]) {
    alreadyDefinedHandler(c) {
      c match {
        case Include(to, p, args) =>
          addIncludeToImplicit(to, p, args)
        case l: Link if l.isImplicit =>
          implicitGraph(l.from, l.to) = l.toTerm
        case _ =>
      }
    }
  }
  // shared code for adding an include
  private def addIncludeToImplicit(to: Term, p: MPath, args: List[Term]) {
    // using OMINST for parametric includes (returns OMIDENT if  args.isEmpty)
    implicitGraph(OMMOD(p), to) = OMINST(p,args)
  }
  
  /** exception handler for [[AlreadyDefined]] */
  private def alreadyDefinedHandler(e: StructuralElement)(code: => Unit) {
    try {code}
    catch {case AlreadyDefined(from, to, old, nw) =>
        /* TODO in general, implicitness of a structure/view should only be added after checking the morphism, maybe implicit could be part of elaboration
         *  otherwise:
         *    the implicit morphism is already used in its own body (can cause infinite loops)
         *    equality check performed by implicit graph cannot yet look up in the morphism, thus missing out on equalities
         *  in particular, but not exclusively, the latter causes too many errors if morphisms are non-trivial,
         *  so the error below is commented out for now -FR for implicits paper and ODK review
         */
        logError(s"implicit morphism $nw from $from to $to induced by ${e.path} in conflict with existing implicit morphism $old")
      //throw AddError(s"implicit morphism $nw from $from to $to induced by ${e.path} in conflict with existing implicit morphism $old")
    }
  }

  /** the bureaucracy of adding 'se' */
  private class Adder(se: StructuralElement, at: AddPosition) extends ChangeProcessor {
     def errorFun(msg: String) = throw AddError(msg)
     def wrongType(exp: String) {errorFun("expected a " + exp + ", found " + se.feature + " " + se.path)}
     def checkNoAfter {
       if (at != AtEnd)
         errorFun("adding after a declaration only allowed in containers")
     }
     def run {
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
        wrongType("content element") // should be impoosible
     }
  }


 // ******************* deleting elements

  /** deletes the element with the given URI
    *
    * @param path the path to the element to be deleted
    */
  def delete(path: Path) {
    log("deleting " + path)
    Deleter.apply(path)
  }

  private val lib = this
  private object Deleter extends ChangeProcessor {
     def errorFun(msg: String) = throw DeleteError(msg)
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
  def update(e: StructuralElement) {
    log("updating " + e.path)
    new Updater(e).run
  }

  /** almost the same as Adder; the only overrides are replacing "add" and "Add" with "update" and "Update"
   * AddPosition is irrelevant
   */
  private class Updater(se: StructuralElement) extends Adder(se, AtEnd) {
     override def errorFun(msg: String) = throw UpdateError(msg)
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
  def reorder(p: Path) {
     Reorderer.apply(p)
  }
  private object Reorderer extends ChangeProcessor {
     def errorFun(msg: String) = throw UpdateError(msg)
     def primitiveDocument(dp: DPath) {}
     def otherNarrativeElement(doc: Document, ln: LocalName) {
        doc.reorder(ln)
     }
     def primitiveModule(mp: MPath) {}
     def otherContentElement(body: ModuleOrLink, ln: LocalName) {
        body.reorder(ln)
     }
     def component(cp: CPath, cont: ComponentContainer) {}
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
  def notifyUpdated(p: CPath) {
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
  def clear {
    modules.clear
    implicitGraph.clear
    documents.clear
  }
}
