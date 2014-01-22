package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.utils._

import scala.collection.mutable.HashSet

/** A read-only abstraction of a library. A Library is a Lookup with write methods */
abstract class Lookup(val report : frontend.Report) {
   def apply(path : Path) = get(path)

   def get(path : Path) : ContentElement
   /** Same as get, but returns an option
    * @return Some(content) if get succeeds, None if get throws an error
    */
   def getO(path: Path) : Option[ContentElement] = try {Some(get(path))} catch {case GetError(_) | BackendError(_) => None}
   //typed access methods
   private def defmsg(path : Path) : String = "no element of required type found at " + path
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
      
   /* The above methods should be polymorphic in the return type like this:
      def get[A <: ContentElement](p : Path) : A = {
         get(p) match {
            case a : A => a
            case _ => throw GetError("bad role")
         }
      }
   *  But we cannot case-split over an abstract type parameter due to Scala's compilation-time type erasure.
   */
   
   def visible(to: Term): HashSet[Term]
   def getImplicit(from: Term, to: Term) : Option[Term]
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
    * looks up a qualified LocalName in a context
    * This respects [[StructureVarDecl]] and [[ComplexTheory]]. 
    * @param context the context
    * @param name the name
    * @param prefix should be omitted
    * @return the resulting VarDecl, possibly generated from a named [[Declaration]]
    */
   def deepLookup(context: Context, name: LocalName, prefix: LocalName = LocalName(Nil)) : VarDecl = {
      context.variables.foreach {vd =>
         name.hasPrefix(vd.name) foreach {rest =>
            val fullName = prefix / vd.name
            val vdR = if (rest.length == 0)
               vd.copy(name = fullName)
            else vd match {
               case StructureVarDecl(n, tp, _) => tp match {
                  case OMMOD(p) =>
                     get(p ? rest) match {
                        case c: Constant =>
                           VarDecl(fullName, c.tp, c.df)
                        case s: Structure =>
                           StructureVarDecl(fullName, s.from, None)
                     }
                  case ComplexTheory(body) =>
                     deepLookup(body, rest, fullName)
               }
            }
            return vdR
         }
      }
      throw LookupError(name, context)
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
      def traverse(t: Term)(implicit con: Context, morph: Term) = t match {
         case OMM(arg, via) => traverse(arg)(con, OMCOMP(morph, via))
         case OMS(theo ?? ln) =>
           val aOpt = getConstant(morph % (LocalName(theo) / ln)).df
           aOpt match {
              case None => t
              case Some(df) => traverse(df)
           }
         case t => Traverser(this,t)
      }
   }
}