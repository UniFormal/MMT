package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import objects._
import patterns._
import utils._
import utils.MyList._
import ontology._

import scala.xml.{Node,NodeSeq}
import collection.mutable.HashSet

/*abstract class ContentMessage
case class Add(e : ContentElement) extends ContentMessage
case class Get(path : Path) extends ContentMessage
case class Delete(path : Path) extends ContentMessage
*/

/**
 * A Library represents an MMT theory graph.
 *
 * The Library instance is the central object of the implementation. represent the main interface between frontend and intelligence.
 * All access of the frontend to the main data structures is through the library's get/add/update/delete interface.
 *
 * @param mem the memory
 * @param report Parameter for logging.
 * 
 * Invariance: This class guarantees structural well-formedness in the sense that libraries conform to the MMT grammar and all declarations have canonical URIs
 * The well-formedness of the objects in the declarations is not guaranteed.
 */
class Library(mem: ROMemory, report : frontend.Report) extends Lookup(report) {
   private val modules = new scala.collection.mutable.HashMap[MPath,Module]
   private val implicitGraph = new ThinGeneratedCategory
   
   private def modulesGetNF(p : MPath) : Module =
      try {modules(p)}
      catch {case _ => if (false) throw GetError("module does not exist: " + p)
                       else     throw new frontend.NotFound(p)
            }
   def log(s : => String) = report("library", s)
   
   /**
    * returns all module paths indexed by this library
    */
   def getAllPaths = modules.keys
   
   def getModule(p : MPath) : Module = modulesGetNF(p)
   
   /**
    * Dereferences a path and returns the found ContentElement.
    * @param path the path to be dereferenced
    * @return the content element
    */
   def get(p: Path) : ContentElement =
      get(p, msg => throw GetError("error while retrieving " + p + ": " + msg))
   def get(p: Path, error: String => Nothing) : ContentElement = p match {
      case doc : DPath => throw ImplementationError("getting documents from library impossible")
      case doc ? !(mod) => modulesGetNF(doc ? mod)
      //case doc ? t / !(str)) => getStructure(doc ? t ? str)    
      case doc ? _ => throw GetError("retrieval of complex module name " + p + " not possible")
      //lookup in atomic modules
      case OMMOD(p) % name => get(p, error) match {
         case t: DefinedTheory =>
            get(t.df % name, error)
         case t: DeclaredTheory =>
             t.getMostSpecific(name) match {
               case Some((d,LocalName(Nil))) => d  // perfect match
               case Some((d, ln)) => d match {
                  // a prefix exists and resolves to d, a suffix ln is left
                  case c: Constant => error("local name " + ln + " left after resolving to constant")
                  case p: Pattern  => error("local name " + ln + " left after resolving to pattern")
                  case i: Instance => error("resolution in unelaborated instances not implemented yet")
                  case s: Structure =>
                      val sym = getSymbol(s.from % ln, p => error("could not lookup source symbol " + p)) // resolve ln in the domain of d
                      translateByLink(sym, s, error) // translate sym along l
               }
               case None => name match {
                  // no prefix of name is declared in t
                  case MorphismStep(m) / ln => 
                     // continue lookup in the domain of m
                     val from = Morph.domain(m)(this)
                     val to = Morph.codomain(m)(this)
                     val imp = implicitGraph(to, t.toTerm) getOrElse {
                        throw GetError("no implicit morphism from " + to + " to " + t.path)
                     }
                     val sym = getSymbol(from % ln, p => error("could not lookup source symbol " + p))
                     translate(sym, OMCOMP(m, imp), error) // translate the result along m
                  case _ => throw GetError(name + " is not valid in " + t.path)
               }
            }
         case v : View =>
            try {
               getInLink(v, name, error)
            } catch {
               case PartialLink() => get(v.from % name) match {
                  // return default assignment
                  case c:Constant => new ConstantAssignment(v.toTerm, name, OMHID)
                  case s:Structure => new DefLinkAssignment(v.toTerm, name, s.from, Morph.Empty)
                  case _ => throw ImplementationError("unimplemented default assignment")
               }
            }
      }
      // lookup in complex modules
      case TheoryExp.Empty % name => throw GetError("empty theory has no declarations")
      case TUnion(ts) % name => ts mapFind {t =>
         getO(t % name)
      } match {
         case Some(d) => d
         case None => throw GetError("union of theories has no declarations except includes")
      }
      case OMDL(h,n) % name =>
         val s = getStructure(h % n, msg => throw GetError("declaration exists but is not a structure: " + h % n + "\n" + msg))
         try {
            getInLink(s, name, error)
         } catch {case PartialLink() =>
            // return default assignment
            get(s.from % name) match {   
               case c:Constant => new ConstantAssignment(s.toTerm, name, OMID(s.to % (s.name / name)))
               case d:Structure => new DefLinkAssignment(s.toTerm, name, d.from, OMDL(s.to, s.name / name))
               case _ => throw ImplementationError("unimplemented default assignment")
            }
         }
      case OMCOMP(Nil) % _ => throw GetError("cannot lookup in identity morphism without domain: " + p)
      case (m @ OMCOMP(hd::tl)) % name =>
         val a = get(hd % name, error)
         if (tl.isEmpty)
           a
         else a match {
            case a: ConstantAssignment => new ConstantAssignment(m, a.name, a.target * OMCOMP(tl))
            case a: DefLinkAssignment => new DefLinkAssignment(m, a.name, a.from, OMCOMP(a.target :: tl))
         }
      case (m @ OMIDENT(t)) % name => get(t % name) match {
         case c: Constant => new ConstantAssignment(m, name, c.toTerm)
         case l: Structure => new DefLinkAssignment(m, name, l.from, l.toTerm)
      }
      case Morph.Empty % name => throw GetError("empty morphism has no assignments")
      case MUnion(ms) % name => ms mapFind {
         m => getO(m % name)
      } match {
         case Some(a) => a
         case None => throw GetError("union of morphisms has no assignments except includes")
      }
   }

   /** thrown by getInLink if the link provides no assignment */
   private case class PartialLink() extends java.lang.Throwable
   
   //TODO definition expansion
   /** auxiliary method of get to unify lookup in structures and views
    * @throws PartialLink() if no assignment provided
    */
   private def getInLink(l: Link, name: LocalName, error: String => Nothing) : ContentElement = l match {
      case l: DefinedLink =>
         get(l.df % name, error)
      case l: DeclaredLink => l.getMostSpecific(name) match {
         case Some((a, LocalName(Nil))) => a  // perfect match
         case Some((a, ln)) => a match {
            case a: ConstantAssignment => error("local name " + ln + " left after resolving to constant assignment")
            case a: DefLinkAssignment => get(a.target % ln, error)
         }
         case None => throw PartialLink() 
      }
   }

   /** translate a Symbol along a morphism */
   def translate(e: Symbol, morph: Term, error: String => Nothing) : Symbol = morph match {
      case OMMOD(v) =>
         val link = getView(v)
         translateByLink(e, link, error)
      case OMDL(h,n) =>
         val link = getStructure(h % n)
         translateByLink(e, link, error)
      case OMCOMP(Nil) => e
      case OMCOMP(hd::tl) => translate(translate(e, hd, error), OMCOMP(tl), error)
      case OMIDENT(t) =>
         val imp = implicitGraph(e.home, t) getOrElse {
            throw GetError("no implicit morphism from " + e.home + " to " + t)
         }
         //TODO better copy e and change home to t?
         translate(e, imp, error)
     //TODO remaining cases
   }
   
   /** auxiliary method of translate to unify translation along structures and views */
   private def translateByLink(s: Symbol, l: Link, error: String => Nothing) : Symbol = l match {
      case l: DefinedLink => translate(s, l.df, error)
      case l: DeclaredLink =>
          //compute e such that the home theory of e is the domain of l by inserting an implicit morphism 
          val imp = implicitGraph(s.home, l.from) getOrElse {
             throw GetError("no implicit morphism from " + s.home + " to " + l.from)
          }
          val e = translate(s, imp, error)
          // get the assignment for e provided by l (if any)
          val assigOpt = try {
              Some(getInLink(l, e.name, error))
          } catch {case PartialLink() =>
              None
          }
          // the prefix of the new constant
          val namePrefix = l match {
             case s: Structure => s.name
             case v: View => LocalName(MorphismStep(v.toTerm))
          }
          val newName = namePrefix / e.name
          // translate e along assigOpt
          e match {
              case c: Constant =>
                 val newDef = assigOpt match {
                    case Some(a: ConstantAssignment) => Some(a.target) //use assignment as definiens
                    case None => c.df.map(_ * l.toTerm) //translate old definiens if no assignment given
                    case _ => throw GetError("link " + l.path + " provides non-ConstantAssignment for constant " + c.path)
                 }
                 new Constant(l.to, newName, c.tp.map(_ * l.toTerm), newDef, c.rl, c.not)
              case r: Structure => assigOpt match {
                  case Some(a: DefLinkAssignment) =>
                      new DefinedStructure(l.to, newName, r.from, a.target, false)
                  case None =>
                      new DefinedStructure(l.to, newName, r.from, OMCOMP(r.toTerm, l.toTerm), false) //TODO should be a DeclaredStructure
                  case _ =>  throw GetError("link " + l.path + " provides non-StructureAssignment for structure " + r.path)
              }
          }
   }

  def getDeclarationsInScope(mod : Term) : List[Content] = {
    val impls = visibleVia(mod).toList
    val decls = impls flatMap {case (from,via) => 
      get(from.toMPath).components
    }
    decls
  }

  /** visible theories (implicit imports and views)
   * @param to the theory
   * @return all pairs (theory,via) such that theory is visible in to
   * transitive and reflexive
   */
  def visibleVia(to: Term) : HashSet[(Term,Term)] = {
     val hs = implicitGraph.into(to)
     hs add (to, OMCOMP())
     hs
  }
  //TODO: both these methods do not take complex to-terms into account, e.g., a union should see all imports into its parts
  /** visible theories (implicit imports and views)
   * as visibleVia but dropping the implicit morphisms
   */
  def visible(to: Term) : HashSet[Term] = {
     val hs = implicitGraph.into(to)
     hs add (to, OMCOMP())
     hs.map(_._1)
  }
  
   /** iterator over all includes into a theory (including the meta-theory)
    * a new iterator is needed once this has been traversed 
    */
   private def importsTo(to: Term) : Iterator[Term] = to match {
      case OMMOD(p) =>
         getTheory(p) match {
            case t: DefinedTheory => importsTo(t.df)
            case t: DeclaredTheory =>
              val mt = t.meta map {p => Iterator(OMMOD(p))} getOrElse(Iterator.empty)
              val incls = t.iterator filter {case s: Structure => s.name.isAnonymous; case _ => false} map {case s: Structure => s.from}
              mt ++ incls
         }
      case TheoryExp.Empty => Iterator.empty
      case TUnion(ts) => (ts flatMap importsTo).iterator //TODO remove duplicates
   }

   /** Checks whether a theory ("from") is included into another ("to"), transitive, reflexive */
   def imports(from: Term, to: Term) : Boolean = {
      TheoryExp.imports(from,to) {(f,t) => 
         importsTo(OMMOD(t)) contains OMMOD(f)
      }
   }
   
   def getImplicit(from: Term, to: Term) : Option[Term] = implicitGraph(from, to)
   def addImplicit(from: Term, to: Term, morph: Term) {implicitGraph(from, to) = morph}

   // TODO add it to the library (i.e. cache results), and check at the beginning if it's cached
   /** Elaborate a theory expression ("exp") into a module */
   def materialize(exp: Term) : Theory = {
      exp match {
         case OMMOD(p: MPath) => getTheory(p)            // exists already
         case exp =>                 // create a new theory and return it
            val path = exp.toMPath
            val meta = TheoryExp.meta(exp)(this)
            val thy = new DeclaredTheory(path.parent, path.name, meta)
            exp match {
               case TheoryExp.Empty => thy 
               case TUnion(ts) =>
                  ts foreach {t => thy.add(Include(exp, t))}
                  thy
               case OMMOD(_) => throw ImplementationError("impossible case")
               case _ => throw ImplementationError("cannot materialize; (note that materializing morphisms not implemented yet)")
            }
      }
   }

   /** returns the symbol from which the argument symbol arose by a structure declaration */
   def preImage(p : GlobalName) : Option[GlobalName] = p.name match {
         case hd / tl =>
            try {
               get(p.module % !(hd)) match {
                  case s : Structure => Some(s.from % tl)
               }
            } catch {case _ => None}
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
    (e.path, e) match {
      case doc : DPath => throw ImplementationError("addtion of document to library impossible")
      case (doc ? mod, m : Module) =>
         modules(doc ? mod) = m
      case (par % ln, _) =>
         val c = getContainer(par, msg => throw AddError("illegal parent: " + msg))
         (c,e) match {
            case (t: DeclaredTheory, e: Symbol) => t.add(e)
            case (l: DeclaredLink, e: Assignment) => l.add(e)
            case _ => throw AddError("only addition of symbols to declared theories or assignments to declared links allowed")
         }
    }
    e match {
       case l: Link if l.isImplicit => implicitGraph(l.from, l.to) = l.toTerm
       case t: DeclaredTheory => t.meta match {
          case Some(m) => implicitGraph(OMMOD(m), t.toTerm) = OMIDENT(t.toTerm)
          case None =>
       }
       case _ =>
       //TODO add equational axioms
    }
  }

   /**
    * Dereferences a path and deletes the found content element.
    * @param path the path to the element to be deleted
    */
   def delete(path : Path) {
      path match {
         case doc : DPath => throw ImplementationError("deletion of documents from library impossible")
         case doc ? mod => modules -= doc ? mod
         case par % ln => getContainer(par, msg => throw DeleteError("illegal parent: " + msg)) match {
            case t: DeclaredTheory => t.delete(ln)
            case l: DeclaredLink => l.delete(ln)
            case _  => throw DeleteError("cannot delete from " + path)
         }
      }
   }
   /** updates a ContentElement by deleting and adding it */
   def update(e : ContentElement) {
	   delete(e.path)
	   add(e)
   }
   /** forgets everything */
   def clear {
      modules.clear
      implicitGraph.clear
   }
   /** retrieves all modules in any order */
   def getModules = modules.values.toList
   
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