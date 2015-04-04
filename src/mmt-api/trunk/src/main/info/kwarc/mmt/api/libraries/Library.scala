package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import objects._
import objects.Conversions._
import patterns._
import utils._
import utils.MyList._
import ontology._

import scala.xml.{Node,NodeSeq}
import scala.collection.mutable._
import scala.ref.SoftReference

/**
 * auxiliary class of [[Library]] to optimize storage
 *  
 * This uses [[SoftReferences]] so that modules are automatically removed when memory is needed.
 */
class ModuleHashMap {
   private val underlying = new HashMap[MPath,SoftReference[Module]]
   def get(p: MPath): Option[Module] = {
      underlying.get(p).flatMap(_.get)
   }
   def update(p: MPath, m: Module) {
      val r = new SoftReference(m)
      underlying.update(p,r)
   }
   def -=(p: MPath) {
      underlying -= p
   }
   def keys = underlying.keys
   def values = underlying.values.flatMap(_.get)
   def clear {underlying.clear}
   override def toString = {
      underlying.toList.sortBy(_._1.name.toPath).map {case (mp, r) =>
         val modString = r.get match {
            case Some(mod) => mod.toString
            case None => "[cleared]"
         }
         mp.toString + "\n" + modString 
      }.mkString("\n")
   }
}

/**
 * A Library represents an MMT theory graph.
 *
 * The Library implements the central structural algorithms, in particular lookup.
 * All access of the main data structures is through the library's get/add/update/delete methods.
 *
 * @param mem the memory
 * @param report parameter for logging.
 * 
 * Invariance: This class guarantees structural well-formedness in the sense that
 * libraries conform to the MMT grammar and all declarations have canonical URIs.
 * The well-formedness of the objects in the declarations is not guaranteed.
 */
class Library(mem: ROMemory, val report : frontend.Report) extends Lookup with Logger {
   private val modules = new ModuleHashMap
   private val implicitGraph = new ThinGeneratedCategory
   
   private def modulesGetNF(p : MPath) : Module =
      modules.get(p).getOrElse {
         throw frontend.NotFound(p)
      }
   val logPrefix = "library"
   
   /**
    * returns all module paths indexed by this library
    */
   def getAllPaths = modules.keys
   /**
    * special case of get with more specific types
    */
   def getModule(p : MPath) : Module = getAs(classOf[Module], p)
   
   /**
    * Special case of get that throws GetError with a standard error message
    */
   def get(p: Path) : ContentElement =
      get(p, msg => throw GetError("error while retrieving " + p + ": " + msg))
   /**
    * Dereferences a path and returns the found ContentElement.
    * @param path the path to be dereferenced
    * @param error the continuation to call on the error message
    * @return the content element
    */
   def get(p: Path, error: String => Nothing) : ContentElement = p match {
      case doc : DPath => throw ImplementationError("getting documents from library impossible")
      case mp: MPath => mp.name.length match {
        case 1 => modulesGetNF(mp)
        case _ => get(mp.toGlobalName, error) match {
          case nm: NestedModule => nm.module
//          case s: Structure => s
          case e => error("complex module path did not resolve to a declaration allowed as a module: " + e)
        }
      }
      //lookup in atomic modules
      case OMPMOD(p, args) % name => get(p, error) match {
         case t: DefinedTheory =>
             get(t.df % name, error) match {
                case d: Declaration => instantiate(d, t.parameters, args)
                case d => error("illegal declaration in theory " + d.path)
             }
             
         case t: DeclaredTheory =>
             val decl = t.getMostSpecific(name) map {case (d,ln) => (instantiate(d, t.parameters, args), ln)}
             decl match {
               case Some((d,LocalName(Nil))) => d // perfect match
               case Some((d, ln)) => d match {
                  // a prefix exists and resolves to d, a suffix ln is left
                  case _: Constant | _: RuleConstant => error("local name " + ln + " left after resolving to constant")
                  case s: Structure =>
                      val sym = getSymbol(s.from % ln, p => error("could not lookup source symbol " + p)) // resolve ln in the domain of d
                      translateByLink(sym, s, error) // translate sym along l
                  case nm: NestedModule =>
                     error("local name " + ln + " left after resolving to nested module")
               }
               case None => name match {
                  // no prefix of name is declared in t
                  case ComplexStep(p) / ln =>
                     get(p) match {
                       case _: Theory =>
                         // continue lookup in p
                         val imp = implicitGraph(OMMOD(p), t.toTerm) getOrElse {
                            throw GetError("no implicit morphism from " + p + " to " + t.path)
                         }
                         val sym = getSymbol(p ? ln, p => error("could not lookup source symbol " + p))
                         translate(sym, imp, error) // translate the result along the implicit morphism
                       case l: Link =>
                         // continue lookup in domain of l
                         val sym = getSymbol(l.from % ln)
                         translateByLink(sym, l, error) // translate the result along the link
                     }
                  case _ => throw GetError(name + " is not declared in " + t.path)
               }
            }
         case v : View =>
            if (0 != args.length)
                throw GetError("number of arguments does not match number of parameters")
            try {
               getInLink(v, name, error)
            } catch {
               case PartialLink() =>
                 val da = get(v.from % name) match {
                  // return default assignment
                  case c:Constant => ConstantAssignment(v.toTerm, name, None, None)
                  case s:Structure => DefLinkAssignment(v.toTerm, name, s.from, Morph.empty)
                  case _ => throw ImplementationError("unimplemented default assignment")
               }
               da.setOrigin(DefaultAssignment)
               da
            }
      }
      case OMDL(h,n) % name =>
         val s = getStructure(h % n, msg => throw GetError("declaration exists but is not a structure: " + h % n + "\n" + msg))
         try {
            getInLink(s, name, error)
         } catch {case PartialLink() =>
            // return default assignment
            val da = get(s.from % name) match {   
               case c:Constant => ConstantAssignment(s.toTerm, name, None, Some(OMID(s.to % (s.name / name))))
               case d:Structure => DefLinkAssignment(s.toTerm, name, d.from, OMDL(s.to, s.name / name))
               case _ => throw ImplementationError("unimplemented default assignment")
            }
            da.setOrigin(DefaultAssignment)
            da
         }
      // lookup in complex modules
      case (home @ ComplexTheory(cont)) % name =>
         cont.mapVarDecls {case (before, vd) =>
            val vdDecl = vd.toDeclaration(ComplexTheory(before))
            vd match {
               case IncludeVarDecl(p, args) =>
                  name.head match {
                     case ComplexStep(q) =>
                        getImplicit(q,p).foreach {m =>
                           val sym = getSymbol(OMPMOD(q, args) % name.tail)
                           val symT = translate(sym, m, error)
                           return symT
                        }
                     case _ =>
                  }
               case StructureVarDecl(s, OMPMOD(p, args), dfOpt) =>
                  name.head match {
                     case SimpleStep(s2) if s == s2 =>
                        val sym = getSymbol(p ? name.tail)
                        val struc = vdDecl.asInstanceOf[Structure]
                        val symT = translateByLink(sym, struc, error)
                        return symT
                     case _ =>
                  }
               case VarDecl(n, _, _, _) =>
                  name.head match {
                     case SimpleStep(n2) if n == n2 =>
                        val c = vdDecl.asInstanceOf[Constant]
                        return c
                     case _ =>
                  }
            }
         }
         throw GetError("name " + name + " not found in " + cont)
      case TUnion(ts) % name => ts mapFind {t =>
         getO(t % name)
      } getOrElse {
         throw GetError("union of theories has no declarations except includes")
      }
      case OMCOMP(Nil) % _ => throw GetError("cannot lookup in identity morphism without domain: " + p)
      case (m @ OMCOMP(hd::tl)) % name =>
         val a = get(hd % name, error)
         if (tl.isEmpty)
           a
         else a match {
            case a: Constant => ConstantAssignment(m, a.name, a.alias, a.df.map(_ * OMCOMP(tl)))
            case a: DefinedStructure => DefLinkAssignment(m, a.name, a.from, OMCOMP(a.df :: tl))
         }
      case (m @ OMIDENT(t)) % name => get(t % name) match {
         case c: Constant =>  ConstantAssignment(m, name, None, Some(c.toTerm))
         case l: Structure =>  DefLinkAssignment(m, name, l.from, l.toTerm)
      }
      case Morph.empty % name => throw GetError("empty morphism has no assignments")
      case MUnion(ms) % name => ms mapFind {
         m => getO(m % name)
      } getOrElse {
         throw GetError("union of morphisms has no assignments except includes")
      }
   }

   /** thrown by getInLink if the link provides no assignment */
   private case class PartialLink() extends java.lang.Throwable
   /** auxiliary method of get to unify lookup in structures and views
    * @throws PartialLink() if no assignment provided
    */
   private def getInLink(l: Link, name: LocalName, error: String => Nothing) : ContentElement = l match {
      case l: DefinedLink =>
         get(l.df % name, error)
      case l: DeclaredLink => l.getMostSpecific(name) match {
         case Some((a, LocalName(Nil))) => a  // perfect match
         case Some((a, ln)) => a match {
            case a: Constant => error("local name " + ln + " left after resolving to constant assignment")
            case a: DefinedLink => get(a.df % ln, error)
         }
         case None =>
            // TODO multiple ComplexSteps resulting from inclusions in a row must be normalized away somewhere
            throw PartialLink() 
      }
   }

   /**
    * instantiates parameters of a declaration with arguments
    * 
    * the home of the new declarations is adapted
    */
   private def instantiate(decl: Declaration, params: Context, args: List[Term]): Declaration = {
      if (args.isEmpty) return decl // lookup from within parametric theory does not provide arguments
      val subs: Substitution = (params / args).getOrElse {
         throw GetError("number of arguments does not match number of parameters")
      }
      if (subs.isIdentity) return decl // avoid creating new instance
      val newHome = decl.home match {
         case OMPMOD(p, oldArgs) => OMPMOD(p, oldArgs ::: args)
         case _ => throw ImplementationError("cannot instantiate declaration of complex theory")
      }
      decl match {
        case c: Constant =>
           Constant(newHome, c.name, c.alias, c.tp.map(_ ^? subs), c.df.map(_ ^? subs), c.rl, c.notC)
        case s: DefinedStructure =>
           DefinedStructure(newHome, s.name, s.from ^? subs, s.df ^? subs, s.isImplicit)
        case s: DeclaredStructure =>
           val sS = DeclaredStructure(newHome, s.name, s.from ^? subs, s.isImplicit)
           s.getPrimitiveDeclarations.foreach {case d =>
              sS.add(instantiate(d, params, args))
           }
           sS
        case nm: NestedModule => throw ImplementationError("substitution of nested modules not implemented yet")
      }
   }
   
   /** translate a Declaration along a morphism */
   private def translate(d: Declaration, morph: Term, error: String => Nothing) : Declaration = morph match {
      case OMMOD(v) =>
         val link = getView(v)
         translateByLink(d, link, error)
      case OMDL(h,n) =>
         val link = getStructure(h % n)
         translateByLink(d, link, error)
      case OMCOMP(Nil) => d
      case OMCOMP(hd::tl) => translate(translate(d, hd, error), OMCOMP(tl), error)
      case OMIDENT(t) =>
         val imp = implicitGraph(d.home, t) getOrElse {
            throw GetError("no implicit morphism from " + d.home + " to " + t)
         }
         //TODO better copy e and change home to t?
         translate(d, imp, error)
     //TODO remaining cases
   }
   
   /** auxiliary method of translate to unify translation along structures and views */
   private def translateByLink(decl: Declaration, l: Link, error: String => Nothing) : Declaration =
   l match {
      case l: DefinedLink => translate(decl, l.df, error)
      case l: DeclaredLink =>
          //compute e such that the home theory of decl is the domain of l by inserting an implicit morphism 
          val imp = implicitGraph(decl.home, l.from) getOrElse {
             throw GetError("no implicit morphism from " + decl.home + " to " + l.from)
          }
          val declT = translate(decl, imp, error)
          // get the assignment for e provided by l (if any)
          val assigOpt = try {
              Some(getInLink(l, declT.name, error))
          } catch {case PartialLink() =>
              None
          }
          // the prefix of the new constant
          val namePrefix = l match {
             case s: Structure => s.name
             case v: View => LocalName(ComplexStep(v.path))
          }
          val newName = namePrefix / declT.name
          def mapTerm(t: Term) = t * l.toTerm  
          // translate declT along assigOpt
          val newDecl = declT match {
              case c: Constant =>
                 val (alias, target, not) = assigOpt match {
                    //use alias and target (as definiens) from assignment
                    case Some(a: Constant) => (a.alias,a.df,a.not)
                    //translate old alias and definiens if no assignment given                       
                    case None => (None,None,None)
                    case _ => throw GetError("link " + l.path + " provides non-ConstantAssignment for constant " + c.path)
                 }
                 val newAlias = alias orElse c.alias.map(namePrefix / _)
                 val newDef = target orElse c.df.map(mapTerm)
                 val newNotC = not.map(notations.NotationContainer(_)) getOrElse c.notC
                 Constant(l.to, newName, newAlias, c.tp.map(mapTerm), newDef, c.rl, newNotC)
              case r: Structure => assigOpt match {
                  case Some(a: DefinedStructure) =>
                      DefinedStructure(l.to, newName, r.from, a.df, false)
                  case None =>
                      DefinedStructure(l.to, newName, r.from, OMCOMP(r.toTerm, l.toTerm), false) //TODO should be a DeclaredStructure
                  case _ =>  throw GetError("link " + l.path + " provides non-StructureAssignment for structure " + r.path)
              }
          }
          newDecl
   }

  def getDeclarationsInScope(mod : Term) : List[Content] = {
    val impls = visibleVia(mod).toList
    val decls = impls flatMap {case (from,via) => 
      get(from.toMPath).getDeclarations
    }
    decls
  }
  
   /** retrieve the implicit morphism "from --implicit--> to" */
   def getImplicit(from: Term, to: Term) : Option[Term] = implicitGraph(from, to)
   /** set the implicit morphism "from --implicit--> to" */
   // TODO should be private, public only because of Archive.readRelational
   def addImplicit(from: Term, to: Term, morph: Term) {implicitGraph(from, to) = morph}
   /** retrieve all implicit morphisms into a theory
    * @param to the theory
    * @return all pairs (theory,via) such that "via: theory --implicit--> to" (transitive and reflexive)
    */
   private def visibleVia(to: Term) : HashSet[(Term,Term)] = {
      val hs = implicitGraph.into(to)
      hs add (to, OMCOMP())
      hs
   }
   /** as visibleVia but dropping the implicit morphisms */
   def visible(to: Term) : HashSet[Term] = visibleVia(to).map(_._1)
   /** as visible but only those where the morphism is trivial (i.e., all includes) */
   def visibleDirect(to: Term) : HashSet[Term] = visibleVia(to).flatMap {case (from, via) =>
      if (via == OMCOMP()) List(from) else Nil
   }

  // TODO move to elaborator
   /** Elaborate a theory expression ("exp") into a module */
   private def materialize(path: MPath, exp: Term) : Theory = {
      exp match {
         case OMMOD(p: MPath) => getTheory(p)            // exists already
         /* case OMPMOD(p, args) =>
            val t = getTheory(p)
            new InstantiatedTheory(t, args) */
         case exp =>                 // create a new theory and return it
            val ComplexTheory(cont) = exp
            val meta = TheoryExp.metas(exp, false)(this).headOption
            val thy = new DeclaredTheory(path.parent, path.name, meta)
            cont.foreach {vd =>
               val d = vd.toDeclaration(exp)
               thy.add(d)
            }
            thy
      }
   }

   /** returns the symbol from which the argument symbol arose by a structure declaration */
   def preImage(p : GlobalName) : Option[GlobalName] = p.name match {
         case hd / tl =>
            try {
               get(p.module % !(hd)) match {
                  case s : Structure => Some(s.from % tl)
               }
            } catch {case _ : Throwable => None}
         case !(hd) => None
         case _ => None
   }
   
   private def getContainer(m: Term, error: String => Nothing) : ContentElement = m match {
      case OMMOD(p) => get(p)
      case OMDL(OMMOD(p), !(n)) => get(OMMOD(p) % !(n), error) match {
         case s: Structure => s
         case _ => error("not a structure: " + m) 
      }
      case _ => error("not a theory, view, or structure: " + m)
   }

   /**
    * adds a declaration
    * @param e the added declaration
    */
  def add(e : ContentElement) {
    log("adding " + e.path)
    (e.path, e) match {
      case (_, doc : DPath) => throw ImplementationError("addtion of document to library impossible")
      case (doc ? mod, m : Module) =>
         modules(doc ? mod) = m
      case (par % ln, _) =>
         val c = getContainer(par, msg => throw AddError("illegal parent: " + msg))
         (c,e) match {
            case (b: Body, e: Declaration) =>
               b.add(e)
            case _ => throw AddError("only addition of symbols to declared theories or assignments to declared links allowed")
         }
    }
    try {
       e match {
          case l: Link if l.isImplicit =>
             implicitGraph(l.from, l.to) = l.toTerm
          case t: DeclaredTheory =>
             t.getIncludes foreach {m =>
                 implicitGraph(OMMOD(m), t.toTerm) = OMIDENT(OMMOD(m))
             }
          case t: DefinedTheory =>
             implicitGraph(t.df, t.toTerm) = OMIDENT(t.toTerm)
          case e: NestedModule =>
             add(e.module)
             implicitGraph(e.home, e.module.toTerm) = OMIDENT(e.home)
          case _ =>
          //TODO add equational axioms
       }
    } catch {case AlreadyDefined(old, nw) =>
       throw AddError(s"implicit morphism $nw induced by ${e.path} in conflict with existing implicit morphism $old")
    }
  }

   /**
    * Dereferences a path and deletes the found content element.
    * @param path the path to the element to be deleted
    */
   def delete(path : Path) {
      log("deleting " + path)
      path match {
         case doc : DPath => throw ImplementationError("deletion of documents from library impossible")
         case doc ? mod => modules -= doc ? mod
         case par % ln => getContainer(par, msg => throw DeleteError("illegal parent: " + msg)) match {
            case t: DeclaredTheory =>
               t.delete(ln) foreach {s =>
                  s.getComponents.foreach {case (comp,cont) =>
                     if (cont.isDefined) notifyUpdated(s.path $ comp)
                  }
               }
            case l: DeclaredLink =>
               l.delete(ln)
            case _  => throw DeleteError("cannot delete from " + path)
         }
         case cp @ CPath(par, comp) => getO(par) foreach {ce =>
            ce.getComponent(comp).foreach {_.delete}
            notifyUpdated(cp)
         }
      }
   }
   /** updates a ContentElement by deleting and adding it */
   def update(e : ContentElement) {
	   delete(e.path)
	   add(e)
   }
   /**
    * marks all known dependent components as dirty 
    * @param p path to the component that was changed/deleted
    * 
    * This method is public because components may be changed from the outside.
    * In that case, this method may be called additionally to maintain invariants.
    * 
    * When deleting a Symbol or Component, this is called automatically.
    */ 
   def notifyUpdated(p: CPath) {
      log("updated: " + p)
      logGroup {
         modules.values.foreach {case m: Module =>
            m.foreachComponent {
               case (comp,tc: TermContainer) =>
                  if (tc.dependsOn contains p) {
                     log("setting dirty: " + comp)
                     tc.setAnalyzedDirty
                  }
               case _ =>
            }
         }
      }
   }
   /** forgets everything */
   def clear {
      modules.clear
      implicitGraph.clear
   }

   /** retrieves all modules in any order */
   def getModules = modules.values
   
   def toNode : NodeSeq = modules.values.map(_.toNode).toList
   override def toString = modules.values.map(_.toString).mkString("","\n\n","")
}
/*   /**
    * Convenience method to check whether a theory is imported into another one.
    * @param from the domain theory
    * @param to the codomain theory
    * @return true iff <code>from</code> is imported into <code>to</code> (transitive, reflexive)
    */
   def imports(from : MPath, to : MPath) : Boolean =
      from == to ||
      getTheory(to).getImports.exists {case TheoImport(_, OMMOD(f)) => imports(from, f)}
   
   /**
    * returns the set of imports (including a possible meta-theory) into a module
    * imports of atomic objects are flattened recursively
    * @param to the module
    * @return the set of imports into <code>to</code> (transitive, irreflexive)
    */
   def importsTo(to : MPath) : List[ModuleObj] = {
      var res : List[ModuleObj] = Nil
      get(to) match {
         case t : DeclaredTheory =>
            t.getImports.foreach {
               case TheoImport(_, OMMOD(from)) => res = OMMOD(from) :: importsTo(from) ::: res
            }
            res.distinct
         case l : DeclaredLink =>
            var res : List[ModuleObj] = Nil
            l.getImports foreach {
               case LinkImport(_, mor) =>
                  res = mor :: res
                  mor match {
                     case OMMOD(l) => res = importsTo(l) ::: res
                     case _ =>
                  }
            }
            res.distinct
         case l : DefinedLink => Nil
      }
   } 
*/

/*
object Normalize extends Traverser[(Lookup,Morph)] {
	def apply(cont : Continuation[(Lookup,Morph)], t: Term)(implicit con : Context, state : (Lookup,Morph)) : Term = {
		val (lib, mor) = state
		t match {
         case OMM(arg, via) => cont(this, arg)(con, (lib, mor * via))
		   case OMID(gname) => applyMorph(lib, con, path, mor)
		   case t => cont(this,t)
		}
	}
	/** non-hiding assignment: use it
	 *  hiding assignment: expand definition or hide
	 *  no assignment: view: expand definition or hide
	 *                 structure: map to constant
	 */
	def applyMorph(lib: Lookup, con : Context, path: SPath, mor: Morph) : Term = mor match {
		case OMIDENT(_) => OMS(path)
		case OMMOD(p) =>
			lazy val expandDef = {
				val from = lib.getLink(p).from
				lib.globalGet(from, OMID(path)) match {
					case c : Constant => c.df match {
                       case None => OMHID()
                       case Some(d) => apply(d, (lib, OMMOD(p)), con)
					}
				}
			}
			try {
		  	   lib.get(p ? path.name) match {
		  	  	   case a : ConstantAssignment => a.target match {
   		              case OMHID() => expandDef
   		              case t => t
		  	  	   }
		  	   }
		   } catch {
		  	   case GetError(_) => lib.get(p) match {
		  	  	  case v : DeclaredView => expandDef
			      case s : DeclaredStructure =>
		  	  	     val mod ?? name = lib.structureModToSym(p)
		  	  	     OMS(mod ? name / path.name)
				  case _ => throw Invalid("ill-formed term")					
			   }
		   }
		case OMCOMP(Nil) => OMS(path)
		case OMCOMP(f::Nil) => applyMorph(lib, con, path, f)
		case OMCOMP(f :: tl) =>
		   val s = applyMorph(lib, con, path, f)
		   apply(s, (lib, OMCOMP(tl)), con)
	}
}
*/