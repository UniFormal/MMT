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
         throw GetError("element at " + ce.path + " exists but has unexpected type " + ce.getClass + " (expected: " + cls + ")")
   }
   
   private type ErrorCont = String => Nothing
   private val defError = (s:String) => throw GetError(s)
   /** for get methods with optional return value */
   private def optional[E](code: ErrorCont => E): Option[E] = try {Some(code(defError))} catch {case _:GetError => None}

   /** lookup a path */
   def get(path : Path) : StructuralElement
   /** like get, but returns option */
   def getO(path: Path) = optional {_ => get(path)}
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
    * @param name the name look up
    * @param error the continuation to call on the error message
    * @return the declaration
    */
   def get(home: Term, name: LocalName, error: String => Nothing): Declaration
   /** like get but returns optional */ 
   def getO(home: Term, name: LocalName) = optional {e => get(home,name,e)}
   /** like get but with restricted return type */ 
   def getAs[E <: ContentElement](cls : Class[E], home: Term, name: LocalName, error: String => Nothing): E =
      as(cls){get(home, name, error)}

   // deprecated, use getAs(classOf[X] instead of getX
   def getTheory(path : MPath, msg : Path => String = defmsg) : Theory =
     get(path) match {case t: Theory => t case _ => throw GetError(msg(path))}
   def getDeclaredTheory(path : MPath, msg : Path => String = defmsg) : DeclaredTheory =
     get(path) match {case t: DeclaredTheory => t case _ => throw GetError(msg(path))}
   def getView(path : MPath, msg : Path => String = defmsg) : View =
     get(path) match {case v: View => v case _ => throw GetError(msg(path))}
   def getLink(path : ContentPath, msg : Path => String = defmsg) : Link =
     get(path) match {case e : Link => e case _ => throw GetError(msg(path))}
   def getSymbol(path : GlobalName, msg : Path => String = defmsg) : Declaration =
     get(path) match {case e : Declaration => e case _ => throw GetError(msg(path))} 
   def getConstant(path : GlobalName, msg : Path => String = defmsg) : Constant =
     get(path) match {case e : Constant => e case _ => throw GetError(msg(path))} 
   def getStructure(path : GlobalName, msg : Path => String = defmsg) : Structure =
     get(path) match {case e : Structure => e case _ => throw GetError(msg(path))} 
   def getPatternAssignment(path : GlobalName, msg : Path => String = defmsg) : PatternAssignment =
     get(path) match {case e : PatternAssignment => e case _ => throw GetError(msg(path))} 
   def getPattern(path : GlobalName, msg: Path => String = defmsg) : Pattern = 
     get(path) match {case e : Pattern => e case _ => throw GetError(msg(path))}
   
   def getComponent(path: CPath) : ComponentContainer = {
      val se = getO(path.parent).getOrElse(throw GetError("parent does not exist: " + path))
      se.getComponent(path.component) getOrElse {
         throw GetError("illegal component: " + path)
      }
   }
   
   def visible(to: Term): HashSet[Term]
   def getImplicit(from: Term, to: Term) : Option[Term]
   def getImplicit(from: MPath, to: MPath) : Option[Term] = getImplicit(OMMOD(from), OMMOD(to))
   def hasImplicit(from: Term, to: Term): Boolean = getImplicit(from, to).isDefined

   //def getDeclarationsInScope(mod : Term) : List[Content]
   
   /** if p is imported by a structure, returns the preimage of the symbol under the outermost structure */
   def preImage(p : GlobalName) : Option[GlobalName]
   
  /**
    * gets the domain in which a Constant was declared
    * 
    * This can be used to retrieve the source of an assignment declared in a DeclaredLink.
    * It is also the official way to test whether a Constant is an assignment.
    *
    * @param a the Constant declaration/assignment
    * @return if assignment: the source theory and the containing link; if declaration: the containing theory
    */
   def getDomain(a: Declaration) : (DeclaredTheory,Option[DeclaredLink]) = {
      val p = a.home match {
         case OMMOD(p) => p
         case OMS(p) => p 
         case _ => throw GetError("non-atomic link")
      }
      val l = get(p) match {
         case t: DeclaredTheory => return (t, None)
         case l: DeclaredLink => l
         case _ => throw GetError("non-declared link") 
      }
      val dom = l.from match {
         case OMMOD(t) => getAs(classOf[Theory],t) match {
           case t: DeclaredTheory => t
           case _ => throw GetError("domain of declared link is not a declared theory")
         }
         case _ => throw GetError("domain of declared link is not a declared theory")
      }
      (dom,Some(l))
   }
   
  /** resolves a LocalName in a theory or in any theory visible to it, returns None if ambiguous */
  def resolve(home: Term, name: LocalName) : Option[StructuralElement] = {
      {
         getO(home, name)  // symbol in the current theory
/*      } orElse {
         home match {
            case OMMOD(p) => getO(p.parent ? name) // module in the namespace of the current theory
            case _ => None
         }*/
      } orElse {
         val incls = visible(home).toList
         val es = incls mapPartial {i => getO(i, name)}
         if (es.length == 1) Some(es(0)) else None  // uniquely resolvable symbol in an included theory
      }
   }

  /**
    * A Traverser that recursively expands definitions of Constants.
    * It carries along a test function that is used to determine when a constant should be expanded. 
    */
   object ExpandDefinitions extends Traverser[ContentPath => Boolean] {
      def traverse(t: Term)(implicit con: Context, expand: ContentPath => Boolean) = t match {
         case OMID(p: GlobalName) if expand(p) => getAs(classOf[Constant],p).df match {
            case Some(t) => traverse(t)
            case None => OMID(p)
         }
         case t => Traverser(this, t)
      }
   }
   
   /**
    * A Traverser that recursively eliminates all explicit morphism applications.
    * apply(t,m) can be used to apply a morphism to a term.
    */
   object ApplyMorphs extends Traverser[Term] {
     def error(s: String) = throw GetError("no assignment found: " + s)
     def traverse(t: Term)(implicit con: Context, morph: Term) : Term = {
       t match {
        case OMM(arg, via) =>
          traverse(arg)(con, OMCOMP(via, morph))
        case OMS(theo ?? ln) =>
          val aOpt = getAs(classOf[Constant], morph, LocalName(theo) / ln, msg => error(msg)).df
          aOpt match {
             case Some(df) => df
             case None => getAs(classOf[Constant], theo ? ln).df match {
                case Some(df) =>
                  traverse(df)
                case None =>
                  error((theo ? ln).toString)
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
    def get(home: Term, name: LocalName, error: String => Nothing) = handler {lup.get(home, name, error)}
    def visible(to: Term) = handler {lup.visible(to)}
    def getImplicit(from: Term, to: Term) = handler {lup.getImplicit(from, to)}
    def preImage(p: GlobalName) = handler {lup.preImage(p)}
}