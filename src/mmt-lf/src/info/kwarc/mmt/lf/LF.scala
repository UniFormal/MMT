package info.kwarc.mmt.api.lf
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import Conversions._

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

case class LFError(msg : String) extends java.lang.Throwable(msg)

object LF {
   val lfbase = new DPath(URI("http", "cds.omdoc.org") / "foundations" / "lf" / "lf.omdoc")
   val lftheory = lfbase ? "lf"
   def constant(name : String) = OMID(lftheory ? name)
   val ktype = constant("type")
   val kind = constant("kind")
   val arrow = constant("arrow")
}

/* apply methods and extractor methods for Scala
   constructor and pattern matcher: Lambda("x", tp, scope)
   accordingly for Pi, Apply, Arrow
   Univ(1), Univ(2) are type and kind
 */

object Lambda {
   def apply(name : String, tp : Term, body : Term) = OMBIND(LF.constant("lambda"), OMV(name) % tp, body)
   def unapply(t : Term) : Option[(String,Term,Term)] = t match {
	   case OMBIND(b, Context(TermVarDecl(n,Some(t),None)), s) if b == LF.constant("lambda") => Some(n,t,s)
	   case _ => None
   }
}

object Pi { 
   def apply(name : String, tp : Term, body : Term) = OMBIND(LF.constant("Pi"), OMV(name) % tp, body)
   def apply(name : String, tp : Sequence, body : Term) = OMBIND(LF.constant("Pi"), SeqVarDecl(name, Some(tp),None), body)
   
   def apply(con: Context, body : Term) = OMBIND(LF.constant("Pi"), con, body) //(?)
   def unapply(t : Term) : Option[(String,Term,Term)] = t match {
	   case OMBIND(b, Context(TermVarDecl(n,Some(t),None), rest @ _*), s) if b == LF.constant("Pi") =>
	      if (rest.isEmpty) Some((n,t,s))
	      else Some((n,t, Pi(rest.toList,s)))
	   case OMA(LF.arrow,FlatSequence(args)) =>
	      val name = "" //TODO: invent fresh name here
	      if (args.length > 2)
	     	 Some((name, args(0), OMA(LF.arrow, args.tail)))
	      else
	     	 Some((name,args(0),args(1)))
	   case _ => None
   }
}

object Arrow {
	def apply(t1 : Term, t2 : Term) = OMA(LF.arrow,List(t1,t2))
	def unapply(t : Term) : Option[(Term,Term)] = t match {
		case OMA(LF.arrow,List(t1: Term,t2:Term)) => Some((t1,t2))
		case _ => None
	}
}

object Apply {
	def apply(f: Term, a: Term) = f(a)
	def unapply(t: Term) : Option[(Term,Term)] = t match {
		case OMA(f, a) => a.last match {
			case t: Term =>
			  if (a.length > 1) Some((OMA(f, a.init), t))
			  else Some(f,t)
			case _ => None
		}
		case _ => None
	}
}

object ApplySpine {
	def apply(f: Term, a: Term*) = OMA(f, a.toList)
	def unapply(t: Term) : Option[(Term,List[Term])] = t match {
		case OMA(f, FlatSequence(args)) =>
		  unapply(f) match {
		 	 case None => Some((f, args))
		 	 case Some((c, args0)) => Some((c, args0 ::: args))
		  }
		case _ => None
	}
}

object Univ {
	def apply(level : Int) : Term = if (level == 1) LF.ktype else LF.kind
	def unapply(t : Term) : Option[Int] =
	   if (t == LF.kind) Some(2) else if (t == LF.ktype) Some(1) else None
}

//TODO: variable capture is not avoided anywhere
/** The LF foundation. Implements type checking and equality */
class LFF extends Foundation {
   private def log(msg : => String) = report("lf", msg)
   val foundTheory = LF.lftheory
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean = {
      log("typing\n" + tm.toString + "\n" + tp.toString)
      (tm, tp) match {
         case (Some(s), Some(a)) => check(s, a, G)
         case (Some(s), None) => infer(s, G) != Univ(2)
         case (None, Some(a)) => check(a, Univ(1), G) || check(a, Univ(2), G)
         case (None, None) => false
      }
   }
   def equality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : Boolean = {
      lib.report("LF: ", "equal\n" + tm1.toString + "\n" + tm2.toString)
      equal(tm1, tm2, Context())
   }
 
/**
 * @param path the URI of a constant
 * @param lib  Lookup library
 * @return type of the constant
 * */  
 def lookuptype(path : GlobalName)(implicit lib : Lookup) : Term = {
    val con = lib.getConstant(path)
    con.tp match {
       case Some(t) => t
       case None =>
          if (con.df.isEmpty) throw LFError("no type exists for " + path)
          infer(con.df.get, Context())
    }
 }
  def lookupdef(path : GlobalName)(implicit lib : Lookup) : Option[Term] = lib.getConstant(path).df

   /**
    * check(s,T,G) iff G |- s : T : U for some U \in {type,kind}
    * checks whether a term s has type T  
    */
   def check(s : Term, T : Term, G : Context)(implicit lib : Lookup) : Boolean = {
	   s match {
	   	case Univ(1) => T == Univ(2)
	   	case OMID(path) => equal(lookuptype(path), T, G)
	   	case OMV(name) =>  equal(T, G(name).asInstanceOf[TermVarDecl].tp.get, G) //TODO; why not equal(T, G(name), G)?; what does G(name) return?  
	   	case Lambda(x, a, t) =>
	   		val G2 = G ++ OMV(x) % a
	   		reduce(T,G) match { //we reduce the type -> dependent type gets instantiated
	   			case Pi(y, av, by) =>
	   				val bx = by ^ G.id ++ OMV(y)/OMV(x)
	   				equal(a, av, G) && check(a, Univ(1), G) && check(t, bx, G2) 
	   			case _ => false
	   		}   
	   	case Pi(x, a, b) =>
	   		val G2 = G ++ OMV(x) % a
	   		(T == Univ(1) || T == Univ(2)) &&
	   		check(a, Univ(1), G) && check(b, T, G2)
	   	case Apply(f,arg) =>
	   		val funtype = infer(f, G)
	   		val argtype = infer(arg, G)
	   		//check(f, funtype, G) && check(arg, argtype, G) && {
	   		reduce(funtype, G) match {
	   			case Pi(x,a,b) =>
	   				equal(argtype, a, G) && equal(T, b ^ (G.id ++ OMV(x)/arg), G)
	   			case _ => false
	   		}
	   		//}
	   	case _ => false
	   }
   }

  
  /**
   * if G |- tm1 : A and G |- tm2 : A, then equal(tm1,tm2,G) iff G |- tm1 = tm2 : A
   */
   def equal (tm1 : Term, tm2 : Term, G : Context)(implicit lib : Lookup) : Boolean = {
	   (tm1, tm2) match { 
 	 		case (OMV(x), OMV(y)) => x == y
 	 		case (OMID(c), OMID(d)) => if (c == d) true else {
 	 			lookupdef(c) match {
 	 				case None => lookupdef(d) match {
 	 					case None => false
 	 					case Some(t) => equal(OMID(c), t, G)
 	 				}
 	 				case Some(t) => equal(OMID(d), t, G) //flipping the order so that if both c and d have definitions, d is expanded next 
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
 	 				if ((!equal(tm1r, tm1, G)) || (!equal(tm2r, tm2, G))) {
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
   def reduce(t : Term, G : Context)(implicit lib : Lookup) : Term = t match {
	 // t can also be an arbitrary application
   		case Apply(Lambda(x,a,t), s) => if (check(s,a,G)) reduce(t ^ (G.id ++ OMV(x)/s), G) else throw LFError("ill-formed")
   		case Apply(tm1, tm2) =>
   			val tm1r = reduce(tm1, G)
   			tm1r match {
   				case Lambda(x,a,t) => reduce(Apply(tm1r, tm2), G)
   				case _ => Apply(tm1r, tm2)
   			}	 	
   		case ApplySpine(OMID(p), args) => lookupdef(p) match {
   			case None => t
   			case Some(d) => reduce(ApplySpine(d, args :_*), G)
   		}
   		case ApplySpine(_,_) => t
   		case OMID(p) => lookupdef(p) match {
   		case Some(d) => reduce(d, G)
   		case None => t}
   		case t => t
   }
 
 /**
  * if exists A such that G |- s : A, then G |- infer(s,G) = A
  */
   def infer(s : Term, G : Context)(implicit lib : Lookup) : Term = {
	   s match {
	   		case Univ(1) => Univ(2)
	   		case OMID(path) => lookuptype(path)
	   		case OMV(name) => G(name).asInstanceOf[TermVarDecl].tp.get // modify it as in check
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
	   			reduce(funtype, G) match {
	   				case Pi(x,a,b) =>
	   					if(equal(argtype, a, G))	
	   						b ^ G.id ++ OMV(x)/arg
	   					else throw LFError("ill-formed")	  
	   				case _ => throw LFError("ill-formed")
	   			}
	   		case _ => throw LFError("ill-formed")
	 	}
   	}
 	
  }

object Run extends frontend.Shell(r => new FoundChecker((new LFF).init(r), r))
