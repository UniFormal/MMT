package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.ontology._
import scala.xml.{Node,NodeSeq}

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
 * @param checker The checker for added declarations.
 * @param report Parameter for logging.
 */
class Library(checker : Checker, dependencies : ABoxStore, report : frontend.Report) extends Lookup(report) {
   private val modules = new scala.collection.mutable.HashMap[MPath,Module]
   private def trymodules(p : MPath) : Module =
      try {modules(p)}
      catch {case _ => throw frontend.NotFound(p)}
   def log(s : => String) = report("library", s)
   
   /**
    * Dereferences a path and returns the found ContentElement.
    * @param path the path to be dereferenced
    * @return the content element
    */
   def get(p : MPath) : Module = p match {
      case doc ? !(mod) => trymodules(doc ? mod)
      //case doc ? (t / !(str)) => getStructure(doc ? t ? str)	   
      case doc ? _ => throw GetError("retrieval of complex module name " + p + " not possible")
   }
   def get(gname: GlobalName) : Declaration =
      get(gname, msg => throw GetError("error while retrieving " + gname + ": " + msg))
   def get(gname: GlobalName, error: String => Nothing) : Declaration = gname.parent match {
      case OMMOD(p) => get(p) match {
         case t: DefinedTheory =>
            get(t.df % gname.name, error)
         case t: DeclaredTheory =>
            t.getFirst(gname.name, error) match {
               case (d, None) => d
               case (c: Constant, Some(ln)) => error("local name " + ln + " left after resolving to constant")
               case (l: DefinitionalLink, Some(ln)) => translate(get(l.from % ln, error), OMDL(l.path))
            }
         case v : View => getInLink(v, gname.name, error) 
      }
      case OMDL(p) => get(p, error) match {
         case s : Structure => getInLink(s, gname.name, error)
         case _ => throw GetError("declaration exists but is not a structure: " + p)
      }
      case OMCOMP(hd::tl) =>
         val a = get(hd % gname.name, error)
         tl match {
            case Nil => a
            case _ => translate(a, OMCOMP(tl))
         }
      case OMIDENT(t) => get(t % gname.name) match {
         case c: Constant => new ConstantAssignment(OMIDENT(t), gname.name, c.toTerm)
         case l: DefinitionalLink => new StructureAssignment(OMIDENT(t), gname.name, l.toMorph)
      }
         
   }
   //TODO definition expansion, partiality
   private def getInLink(l: Link, name: LocalName, error: String => Nothing) : Declaration = l match {
      case l: IncludeLink => get(OMIDENT(l.from) % name)
      case l: DefinedLink =>
         get(l.df % name, error)
      case l: DeclaredLink =>
         l.getFirst(name, error) match {
            case (a, None) => a
            case (a: ConstantAssignment, Some(ln)) => error("local name " + ln + " left after resolving to constant assignment")
            case (a: StructureAssignment, Some(ln)) => get(a.target % ln, error)
         }
   }
   def translate(d: Declaration, m : Morph) : Declaration
	 /*  
	   
	   path match {
      case doc : DPath => throw ImplementationError("retrieval of document from library")
      case mod % name => 
         val Mod = get(mod)
         (Mod, name) match {
            case (t : DeclaredTheory, !(n)) => t.getO(n).getOrElse(throw GetError(path + " not found"))
            case (t : DeclaredTheory, hd / tl) => t.getO(hd) match {
               case Some(struc : Structure) =>
                  val sym = resolveAlias(localGet(struc.from, tl))
                  struc.applyTo(sym)
               case Some(_ : Constant) => throw GetError("local path continued after resolving to constant")
               case None => throw GetError(hd + " not found in " + mod)
            }
            case (l : DeclaredLink, !(n)) => l.getO(n).getOrElse(throw GetError(path + " not found"))
            case (l : DeclaredLink, init \ last) => l.getO(name) match {
               case Some(a) => a
               case _ =>
                  val assig = getStructureAssignment(mod ? init)
                  val sym = resolveAlias(localGet(Morph.domain(assig.target)(this), last))
                  assig.applyTo(sym)
            }
            case (l : DefinedLink, _) => localGet(l.from, name) match {
               case con : Constant => new ConstantAssignment(mod, name, con.toTerm * l.df) 
               case struc : Structure => new StructureAssignment(mod, name, struc.toMorph * l.df)
            }
            case _ => throw GetError("module of " + path + " was found but points to neither theory or link or name was empty -- should be impossible")
         }
   }
   */
   /*
   def structureModToSym(p : MPath) : SPath = p match {
	   case doc ? (t / str) => doc ? t ? str
   }
   /** recursively resolves an Alias; result is not an Alias */
   def resolveAlias(sym : Symbol) : Symbol = sym match {
         case a : Alias => resolveAlias(get(a.forpath))
         case _ => sym
   }*/
   /** returns the symbol from which the argument symbol arose by a structure declaration */
   def preImage(p : GlobalName) : Option[GlobalName] = p.name match {
         case hd / tl =>
            try {
               get(p.parent % !(hd)) match {
                  case s : Structure => Some(s.from % tl)
               }
            } catch {case _ => None}
         case !(hd) => None
   }
   
   private def getContainer(m: ModuleObj, error: String => Nothing) : ContentElement = m match {
      case OMMOD(p) => get(p)
      case OMDL(OMMOD(p) % !(n)) => get(OMMOD(p) % !(n), error) match {
         case s: Structure => s
         case _ => error("not a structure: " + m) 
      }
      case _ => error("not a theory, view, or structure: " + m)
   }
   /**
    * Validates and, if successful, adds a ContentElement to the theory graph.
    * Note that the element already points to the intended parent element
    * so that no target path is needed as an argument.
    * @param e the element to be added
    */
   def add(e : ContentElement) {
      try {checker.check(e)(this) match {
         case Fail(msg) => throw AddError(msg)
         case Success(deps) =>
            addUnchecked(e)
            deps.map(dependencies += _)
         case Reconstructed(rs, deps) =>
            rs.foreach(addUnchecked)
            deps.map(dependencies += _)
      }} catch {
         case e @ frontend.NotFound(_) => throw e
      }
   }
   // error checks of this method could be moved to Checker
   protected def addUnchecked(e : ContentElement) = (e.path, e) match {
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
   def update(e : ContentElement) {
	   delete(e.path)
	   add(e)
   }
   def clear {
      modules.clear
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
   def toNode : NodeSeq = modules.values.map(_.toNode).toList
   override def toString = modules.values.map(_.toString).mkString("","\n\n","")
}
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