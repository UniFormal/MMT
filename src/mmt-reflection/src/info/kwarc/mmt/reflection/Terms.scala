package info.kwarc.mmt.reflection
import info.kwarc.mmt.api._
import libraries._
import modules._
import symbols._
import objects._
import utils._
import frontend._
import objects.Conversions._
import info.kwarc.mmt.lf._

/* Meta-variable names
   s, t: Terms
   A, B: Types
   K, L: kinds
   G: contexts
*/

/* Operators:
   v % A  : a context in which v : OMV has type A : Term
   G ++ G': appending G' to G
   G(name): look up type of OMV(name) in G
   v / s  : the substitution that substitutes v : OMV  with s : Term
   s ^ S  : application of the substitution S to s : Term
*/

case class ReflectionError(msg : String) extends java.lang.Throwable(msg)

object Reflection {
   val base = new DPath(utils.URI("http", "cds.omdoc.org") / "foundations" / "reflection" / "terms.omdoc")
   val theory = LF.lfbase ? "Terms"
   def constant(name : String) = OMID(LF.lftheory ? name)
   val intro = constant("introduction")
   val elim = constant("elimination")
   val rtype = constant("refltype")
   val eval = constant("evaluation")
}

/* apply methods and extractor methods for Scala
   constructor and pattern matcher: Lambda("x", tp, scope)
   accordingly for Pi, Apply, Arrow
   Univ(1), Univ(2) are type and kind
 */

object TermRefl {
   def apply(thy: MPath, term: Term) = OMA(Reflection.intro, List(OMMOD(thy), term))
   def unapply(t : Term) : Option[(MPath,Term)] = t match {
	   case OMA(Reflection.intro, List(OMMOD(thy), tm)) => Some((thy, tm))
	   case _ => None
   }
}

object TermEval {
  def apply(thy: MPath, term: Term) = OMA(Reflection.eval, List(OMMOD(thy), term))
  def unapply(t : Term) : Option[(MPath,Term)] = t match {
    case OMA(Reflection.elim, List(OMMOD(thy), tm)) => Some((thy, tm))
    case _ => None
  }
}

object ReflType {
  def apply(thy: MPath, tp: Term)
          ={ OMA(Reflection.rtype, List(OMMOD(thy), tp))     }
  def unapply(t : Term) : Option[(MPath,Term)] = t match {
    case OMA(Reflection.rtype, List(OMMOD(thy), tp)) => Some((thy, tp))
    case _ => None
  }
}

object Elim {
  def apply(t: Term, mor: Term) = OMA(Reflection.elim, List(t, mor))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(Reflection.rtype, List(t, mor)) => Some((t, mor))
    case _ => None
  }
}

/** @param theory : quoted theory
 * @param context : context
 */
case class Frame(theory : MPath, context : Context)


case class Stack(frames : List[Frame]) {
   def push (f : Frame) = f::frames
   def top () = frames.last
}

//TODO: variable capture is not avoided anywhere
/** The LF foundation. Implements type checking and equality */
class ReflectionFoundation extends Foundation {
   private def log(msg : => String) = report("lf", msg)
   val foundTheory = LF.lftheory

  def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit fl : FoundationLookup)  = false
  def refltyping(tm : Option[Term], tp : Option[Term], W : Stack = Frame(thy: MPath, Context()))(implicit fl : FoundationLookup) : Boolean = {
      log("typing\n" + tm.toString + "\n" + tp.toString)
      (tm, tp) match {
         case (Some(s), Some(a)) => check(s, a, W)
         case (Some(s), None) => infer(s, W) != Univ(2)
         case (None, Some(a)) => check(a, Univ(1), W) || check(a, Univ(2), W)
         case (None, None) => false
      }
   }
   def equality(tm1 : Term, tm2 : Term)(implicit fl : FoundationLookup) : Boolean = {
      log("equal\n" + tm1.toString + "\n" + tm2.toString)
      equal(tm1, tm2, Context())
   }
   def inference(tm: Term, context: Context)(implicit lup: Lookup) : Term = infer(tm, context)(new PlainLookup(lup))
 
/**
 * @param path the URI of a constant
 * @param lib  Lookup library
 * @return type of the constant
 * */  
  def lookuptype(path : GlobalName)(implicit fl : FoundationLookup) : Term = fl.getType(path).getOrElse(throw LFError("no type exists for " + path))
  def lookupdef(path : GlobalName)(implicit fl : FoundationLookup) : Option[Term] = fl.getDefiniens(path)

   /**
    * check(s,T,G) iff G |- s : T : U for some U \in {type,kind}
    * checks whether a term s has type T  
    */
   def check(s : Term, T : Term, W : Stack)(implicit fl : FoundationLookup) : Boolean = {
	   s match {
       case TermRefl(thy,tm) => T match {
         case ReflType(thy2,tp) => if (thy == thy2) check(tm, tp, W.push(Frame(thy, Context())))
           else throw ReflectionError("expected: " + thy + "\nfound: " + thy2)
         case _ => throw ReflectionError("invalid type")
       }
	   }
   }
}

  /*
  
  /**
   * if G |- tm1 : A and G |- tm2 : A, then equal(tm1,tm2,G) iff G |- tm1 = tm2 : A
   */
   def equal (tm1 : Term, tm2 : Term, G : Context)(implicit fl : FoundationLookup) : Boolean = {
	   (tm1, tm2) match { 
 	 		case (OMV(x), OMV(y)) => x == y
 	 		case (Const(c), Const(d)) => if (c == d) true else {
 	 			lookupdef(c) match {
 	 				case None => lookupdef(d) match {
 	 					case None => false
 	 					case Some(t) => equal(Const(c), t, G)
 	 				}
 	 				case Some(t) => equal(Const(d), t, G) //flipping the order so that if both c and d have definitions, d is expanded next 
 	 			}
 	 		}
 	 		case (Lambda(x1,a1,t1), Lambda(x2,a2,t2)) => 
 	 			val G2 = G ++ OMV(x1) % a1
 	 			equal(a1, a2, G) && equal(t1, t2^(OMV(x2)/OMV(x1)), G2)
 	 		case (Pi(x1,a1,t1), Pi(x2,a2,t2)) => 
 	 			val G2 = G ++ OMV(x1) % a1
 	 			equal(a1, a2, G) && equal(t1, t2^(OMV(x2)/OMV(x1)), G2)
 	 		case (Apply(f1,arg1), Apply(f2,arg2)) => {
 	 			if (equal(f1, f2, G) && equal(arg1, arg2, G)) true else {
        		//val tm1r = reduce(tm1, Context())  // why empty context?
        		//val tm2r = reduce(tm2, Context())
 	 				val tm1r = reduce(tm1, G)
 	 				val tm2r = reduce(tm2, G)
 	 				if (tm1r != tm1 || tm2r != tm2) {
 	 					equal(tm1r, tm2r, G)
 	 				} else false
 	 			}
 	 		}
 	 		case _ => tm1 == tm2
	   }
   }

 /**
  * if t is a constant or an application of a defined constant, it replaces the constant with its definition.
  * if t is a lambda expression application, it is beta-reduced 
  * removes top-level redex, application of defined constant
  * if t well-formed, then |- reduce(t,G) = t
  * if |- t = Pi x:A. B for some A,B, then reduce(t) of the form Pi x:A.B (?)
  */
   def reduce(t : Term, G : Context)(implicit fl : FoundationLookup) : Term = t match {
	 // t can also be an arbitrary application
   		case Apply(Lambda(x,a,t), s) => if (check(s,a,G)) reduce(t ^ (G.id ++ OMV(x)/s), G) else throw LFError("ill-formed")
   		case Apply(tm1, tm2) =>
   			val tm1r = reduce(tm1, G)
   			tm1r match {
   				case Lambda(x,a,t) => reduce(Apply(tm1r, tm2), G)
   				case _ => Apply(tm1r, tm2)
   			}	 	
   		case ApplySpine(Const(p), args) => lookupdef(p) match {
   			case None => t
   			case Some(d) => reduce(ApplySpine(d, args :_*), G)
   		}
   		case ApplySpine(_,_) => t
   		case Const(p) => lookupdef(p) match {
   		   case Some(d) => reduce(d, G)
   		   case None => t
   		}
   		case t => t
   }
 
 /**
  * if exists A such that G |- s : A, then G |- infer(s,G) = A
  */
   def infer(s : Term, G : Context)(implicit fl : FoundationLookup) : Term = {
	   s match {
	   		case Univ(1) => Univ(2)
	   		case Const(path) => lookuptype(path)
	   		case OMV(name) => G(name).tp.get // modify it as in check
	   		case Lambda(name, tp, body) =>
	   			val G2 = G ++ OMV(name) % tp
	   			Pi(name,tp,infer(body, G2))
	   		case Pi(name, tp, body) =>
	   			val G2 = G ++ OMV(name) % tp
	   			infer(body, G2)
	   		case Apply(f, arg) =>
	   			// need to ensure that type of argument matches with type of functions 
	   			val funtype = infer(f, G)
	   			val argtype = infer(arg, G)
	   			val r = reduce(funtype, G)
	   			r match {
	   				case Pi(x,a,b) =>
	   					if (equal(argtype, a, G))	
	   						b ^ G.id ++ OMV(x)/arg
	   					else throw LFError("ill-formed")	  
	   				case _ => throw LFError("ill-formed")
	   			}
	   		case _ => throw LFError("ill-formed")
	 	}
   }
*/