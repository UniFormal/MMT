package info.kwarc.mmt.api.libraries

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.utils._
import utils.MyList._

import scala.collection.mutable.HashSet

/** A read-only abstraction of a library. A Library is a Lookup with write methods */
abstract class Lookup {self =>
   def apply(path : ContentPath) = get(path)

   private def defmsg(path : Path) : String = "element exists but has unexpected type at " + path

   /** for get methods with restricted return type */
   private def as[E <: StructuralElement](cls : Class[E])(code : => StructuralElement) : E = {
      val ce = code
      if (cls.isInstance(ce))
         ce.asInstanceOf[E]
      else
         throw GetError(ce.path, "element exists but has unexpected type " + ce.getClass + " (expected: " + cls + ")")
   }

   /** for get methods with optional return value */
   private def optional[E](code: => E): Option[E] = {
     try {Some(code)}
     catch {case _:GetError|_:BackendError => None}
   }

   /** lookup a path */
   def get(path : Path) : StructuralElement
   /** like get, but returns option */
   def getO(path: Path) = optional {get(path)}
   /**
    * like get but with restricted return type
    * example: getAs(classOf[Constant], path): Constant
    */
   def getAs[E <: StructuralElement](cls : Class[E], path: Path): E = as(cls) {get(path)}

   /** important special case */
   def getModule(path: MPath): Module = getAs(classOf[Module], path)

   /** lookup a declaration in a (possibly complex) module
    *
    * @param home the module in which to look up
    * @param name the name look up; must start with ComplexStep unless it is local to home
    * @param error the continuation to call on the error message
    * @return the declaration
    */
   def get(home: Term, name: LocalName): Declaration
   /** like get but returns optional */
   def getO(home: Term, name: LocalName) = optional {get(home,name)}
   /** like get but with restricted return type */
   def getAs[E <: ContentElement](cls : Class[E], home: Term, name: LocalName): E =
      as(cls){get(home, name)}

   // deprecated, use getAs(classOf[X],...) instead of getX
   def getTheory(path : MPath) : Theory =
     get(path) match {case t: Theory => t case _ => throw GetError(path, "not a theory")}
   def getView(path : MPath) : View =
     get(path) match {case v: View => v case _ => throw GetError(path, "not a view")}
   def getLink(path : ContentPath) : Link =
     get(path) match {case e : Link => e case _ => throw GetError(path, "not a link")}
   def getSymbol(path : GlobalName) : Declaration =
     get(path) match {case e : Declaration => e case _ => throw GetError(path, "not a declaration")}
   def getConstant(path : GlobalName) : Constant =
     get(path) match {case e : Constant => e case _ => throw GetError(path, "not a constants")}
   def getStructure(path : GlobalName) : Structure =
     get(path) match {case e : Structure => e case _ => throw GetError(path, "not a structure")}

   def getComponent(path: CPath) : ComponentContainer = {
      val se = getO(path.parent).getOrElse(throw GetError(path, "parent does not exist"))
      se.getComponent(path.component) getOrElse {
         throw GetError(path, "illegal component")
      }
   }

   def visible(to: Term): Iterable[MPath]
   def getImplicit(from: Term, to: Term) : Option[Term]
   def getImplicit(from: MPath, to: MPath) : Option[Term] = getImplicit(OMMOD(from), OMMOD(to))

   def hasImplicit(from: Term, to: Term): Boolean = getImplicit(from, to).isDefined
   def hasImplicit(from: MPath, to: MPath): Boolean = getImplicit(from, to).isDefined

   /**
    * apply a function to all declarations that are visible (based on what is currently loaded) to a theory
    * @param mod the theory
    * @param f applied to (theory t declaring d, morphism mod->t that makes d visible, d)
    */
   def forDeclarationsInScope(mod: Term)(f: (MPath,Term,Declaration) => Unit): Unit
   
   /** if p is imported by a structure, returns the preimage of the symbol under the outermost structure */
   def preImage(p : GlobalName) : Option[GlobalName]

  /**
    * gets the domain in which a Declaration was declared
    *
    * This can be used to retrieve the source of an assignment declared in a DeclaredLink.
    * It is also the official way to test whether a Constant is an assignment.
    *
    * @param a the Constant declaration/assignment
    * @return if assignment: the source theory and the containing link; if declaration: the containing theory
    */
   def getDomain(a: Declaration) : (AbstractTheory,Option[Link]) = {
      val p = a.home match {
         case OMMOD(p) => p
         case OMS(p) => p
         case _ => throw GetError(a.path, "domain is non-atomic link")
      }
      val l = get(p) match {
         case t: AbstractTheory => return (t, None)
         case l: Link => l
         case nm: NestedModule => nm.module match {
           case t: AbstractTheory => return (t, None)
           case l: Link => l
         }
         case _ => throw GetError(a.path, "unknown parent found")
      }
      val dom = l.from match {
         case OMMOD(t) => getAs(classOf[Theory],t)
         case _ => throw GetError(a.path, "domain of link is not an atomic theory")
      }
      (dom,Some(l))
   }
   
  /** returns the container element that contains a declarations */
  def getParent(d: Declaration): ModuleOrLink = {
    val homePath = d.home match {
      case OMID(p) => p
      case _ => throw GetError(d.path, "declaration has complex home")
    }
    get(homePath) match {
       case m: ModuleOrLink => m
       case nm: NestedModule => nm.module
       case _ => throw GetError(d.path, "parent found but is not a module or link")
    }
  }

  /** resolves a name in a list of theories using any included theory, possibly inserting include steps */
  def resolveName(parents: List[MPath], name: LocalName): List[GlobalName] = {
    val withIncludes = parents.flatMap {p =>
      getO(p).toList.flatMap {
        case thy: Theory => p :: thy.getAllIncludes.mapPartial {i =>
          if (i.df.isEmpty) Some(i.from) else None
        }
        case _ => Nil
      }
    }.distinct
    def resolveIn(p: MPath) = {
      getO(p).toList.flatMap {
        case pMod: Module =>
          val dom = pMod.domain
          if (dom contains name) List(p ? name)
          else {
            dom.mapPartial {n =>
              // drop all complex steps and try again
              val nS = n.dropComplex
              if (nS == name) Some(p ? n)
              else None
            }
          }
        case _ => Nil
      }
    }
    withIncludes flatMap resolveIn
  }

  /** resolves a name in a module using any included or realized theories */
  def resolveRealizedName(parent: ModuleOrLink, name: LocalName): List[GlobalName] = {
    val direct = parent match {
      case t: AbstractTheory =>
        t.getRealizees.map(_.from)
      case l: Link =>
        TheoryExp.getSupport(l.from)
    }
    resolveName(direct, name)
  }
  
  /**
   * all constants for which c is a quasi-alias (recursively)
   * a quasi-alias is defined by a definition c = OMS(d) 
   */
  def quasiAliasFor(c: GlobalName): List[GlobalName] = {
    getO(c) match {
      case Some(c: Constant) =>
        c.df match {
          case Some(OMS(p)) => p :: quasiAliasFor(p)
          case _ => Nil
        }
      case _ => Nil
    }
  }

  /**
    * A Traverser that recursively definition-expands [[OMID]]s.
    *
    * See [[ExpandDefinitions.apply()]].
    */
   object ExpandDefinitions extends Traverser[ContentPath => Boolean] {
    /**
      * Recursively definition-expands all [[OMID]]s in ''t'' as deemed required by ''expand(omid.path)''.
      *
      * It leaves all other [[OMID]]s untouched and also those that reference a constant without definiens.
      *
      * @param t The term
      * @param expand A predicate on [[ContentPath]]s signalling when to expand an [[OMID]].
      * @param con A useless context to agree with the contract of [[Traverser]], you can safely ignore this argument.
      * @return The definition-expanded term.
      */
      override def apply(t: Term, expand: ContentPath => Boolean, con : Context = Context()) : Term = super.apply(t, expand, con)

    /**
      * Convenience method for the case of the other [[apply()]] when the predicate is defined by a sequence.
      * @param t The term
      * @param expand All [[OMID]]s referencing a defined constant in this list will be definition-expanded.
      * @return The definition-expanded term.
      */
      def apply(t: Term, expand: Set[ContentPath]): Term = super.apply(t, expand.contains)

      def traverse(t: Term)(implicit con: Context, expand: ContentPath => Boolean): Term = t match {
         case OMID(p: GlobalName) if expand(p) => getAs(classOf[Constant],p).df match {
            case Some(tm) => traverse(tm)
            case None => OMID(p)
         }
         case t => Traverser(this, t)
      }
   }

   /**
    * A Traverser that recursively eliminates all explicit morphism applications.
    * apply(t,m) can be used to apply a morphism to a term.
    */
   //TODO try to support tail-recursive morphism applications; benchmark example: eval(s(...s(z)...)) where eval is the fold that maps number terms to number values
   object ApplyMorphs extends Traverser[Term] {
     def traverse(t: Term)(implicit con: Context, morph: Term) : Term = {
       t match {
        case OMM(arg, via) =>
          traverse(arg)(con, OMCOMP(via, morph))
        case OMS(path @ (theo ?? ln)) =>
          val (aOpt,errOpt) = try {
            (getAs(classOf[Constant], morph, LocalName(theo) / ln).df, None)
          } catch {case e: GetError => (None,Some(e))}
          aOpt match {
             case Some(df) => df
             case None =>
               // TODO what to do if already included in codomain? need to specify if views have to explicitly include identity
               getAs(classOf[Constant], path).df match {
                case Some(df) =>
                  traverse(df)
                case None =>
                  val error = GetError(path, s"no assignment found for ${path.name} in morphism")
                  errOpt.foreach {e => error.setCausedBy(e)}
                  throw error
             }
          }
        case t => Traverser(this,t)
       }
     }
   }
}

/**
 * delegates all lookup methods to another Lookup and handles the [[NotFound]] exception
 */
abstract class LookupWithNotFoundHandler(lup: Lookup) extends Lookup {
    protected def handler[A](code: => A): A

    def get(path: Path) = handler {lup.get(path)}
    def get(home: Term, name: LocalName) = handler {lup.get(home, name)}
    def visible(to: Term) = handler {lup.visible(to)}
    def getImplicit(from: Term, to: Term) = handler {lup.getImplicit(from, to)}
    def preImage(p: GlobalName) = handler {lup.preImage(p)}
}

/** to be mixed into LookupWithNotFoundHandler for failing on NotFound */
trait FailingNotFoundHandler {
    protected def handler[A](code: => A): A = try {
      code
    } catch {
      case frontend.NotFound(p, _) =>
        throw GetError(p, "not found")
    }
}
