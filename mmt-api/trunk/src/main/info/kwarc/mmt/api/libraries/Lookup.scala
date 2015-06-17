package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.utils._

import scala.collection.mutable.HashSet

/** A read-only abstraction of a library. A Library is a Lookup with write methods */
abstract class Lookup {
   def apply(path : Path) = get(path)

   def get(path : Path) : ContentElement
   /** Same as get, but returns an option
    * @return Some(content) if get succeeds, None if get throws an error
    */
   def getO(path: Path) : Option[ContentElement] = try {Some(get(path))} catch {case _:GetError => None}
   private def defmsg(path : Path) : String = "element exists but has unexpected type at " + path
   /**
    * as get but with finer return type
    * @tparam E required return type
    * @param cls class object of E
    * @return same as get(path) but cast into E 
    * 
    * usage example: getAs(classOf[Constant], path): Constant
    */
   def getAs[E <: ContentElement](cls : Class[E], path: Path): E = {
      val se = get(path)
      if (cls.isInstance(se))
         se.asInstanceOf[E]
      else
         throw GetError("element at " + path + " exists but has unexpected type " + se.getClass + " (expected: " + cls + ")")
   }
   

   // obsolete
   def getModule(path : MPath, msg : Path => String = defmsg) : Module =
     get(path) match {case m: Module => m case _ => throw GetError(msg(path))}
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
   
   def getComponent(path: CPath, msg: Path => String = defmsg) : ComponentContainer = {
      val se = getO(path.parent).getOrElse(throw GetError(msg(path.parent)))
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
    * @param a the Constant declaration/assignment
    * @return if assignment: the source theory and the containing link; if declaration: the containing theory
    */
   def getDomain(a: Declaration) : (DeclaredTheory,Option[DeclaredLink]) = {
      val p = a.home match {
         case OMMOD(p) => p
         case OMDL(OMMOD(p), name) => OMMOD(p) % name 
         case _ => throw GetError("non-atomic link")
      }
      val l = get(p) match {
         case t: DeclaredTheory => return (t, None)
         case l: DeclaredLink => l
         case _ => throw GetError("non-declared link") 
      }
      val dom = l.from match {
         case OMMOD(t) => getTheory(t) match {
           case t: DeclaredTheory => t
           case _ => throw GetError("domain of declared link is not a declared theory")
         }
         case _ => throw GetError("domain of declared link is not a declared theory")
      }
      (dom,Some(l))
   }
   
   /**
    * A Traverser that recursively expands definitions of Constants.
    * It carries along a test function that is used to determine when a constant should be expanded. 
    */
   object ExpandDefinitions extends Traverser[ContentPath => Boolean] {
      def traverse(t: Term)(implicit con: Context, expand: ContentPath => Boolean) = t match {
         case OMID(p: GlobalName) if expand(p) => getConstant(p).df match {
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
     def traverse(t: Term)(implicit con: Context, morph: Term) : Term = {
       def notmeta(theo:MPath,ln:LocalName) : Term = {
         val aOpt = getAs(classOf[Constant], morph % (LocalName(theo) / ln)).df
         aOpt match {
           case Some(df) => df
           case None => getAs(classOf[Constant], theo ? ln).df match {
             case Some(df) => traverse(df)
             case None => t
           }
         }
       }
       t match {
        case OMM(arg, via) => traverse(arg)(con, OMCOMP(via, morph))
        case OMS(theo ?? ln) => {
          val source = morph match {
            case OMID(p) => Some(get(p))
            case _ => None
          }
          source match {
            case Some(l: DeclaredLink) => {
              val home = get(l.from.toMPath) match {
                case t: DeclaredTheory => t.meta
                case _ => None
              }
              val visibles = (home match {
                case Some(x) => visible(OMMOD(x)).toList
                case None => Nil
              }).map(tm => tm.toMPath)
              if (visibles contains theo) {
                l.metamorph match {
                  case Some(x) => traverse(t)(con,x)
                  case None => Traverser(this,t)
                }
              } else notmeta(theo, ln)
            }
            case _ => notmeta(theo, ln)
          }
        }
       case _ => Traverser(this,t)
     }
     }
   }
}