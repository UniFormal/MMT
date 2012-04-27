package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import libraries._
import modules._
import symbols._
//import utils._
import frontend._
import objects.Conversions._

case class UnresolvedConstraints() extends java.lang.Throwable

abstract class Constraint
case class EqualConstraint(t1: Term, t2: Term, t: Term, context: Context)

/*
abstract class TypeConstructor {
   def extensionalityRule(t: Term) : Term
   def extensionalityRule(eq: EqualConstraint) : Option[List[EqualConstraint]]
   def introSymbol: Path
   def computationRule(t: Term): Option[Term]
}

class Unifier {
   val typeCons : List[TypeConstructor]
   def unify(con: Context, t1: Term, t2: Term, t: Term) {
      normal MMT equality check
      if t1 and t2 have the same shape and the same head, and the head is known to be an introSymbol (and thus injective) recurse into components
      otherwise, try extend
   }
   def extend(con: Context, t1: Term, t2: Term, t: Term) {
      (typeCons find {tc => (tc.typeSymbol == t.head)}) match {
        case Some(tc) => unify (... tc.extensionalityRule(...) ...)
        case None =>
           compute(con, t1, t2)
      }
   }
   def compute(con: Context, t1: Term, t2: Term) {
      val t1C = typeCons findMap {tc => tc.computationRule(t1)}
      val t2C = typeCons findMap {tc => tc.computationRule(t1)}
      unify(..)
   }
}
*/

class ConstraintDelay(meta: Context) {
   var solution : Substitution = Substitution()
   private var delayed : List[Constraint] = Nil
   def delay(c: Constraint) {delayed = c :: delayed}
   def solved : Boolean = delayed.isEmpty //TODO: substitution must be total for meta
}

class ObjectChecker(report: Report) {
   private def log(msg : => String) = report("checker", msg)
   //val foundTheory = LF.lftheory
/*   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean = {
      log("typing\n" + tm.toString + "\n" + tp.toString)
      (tm, tp) match {
         case (Some(s), Some(a)) => check(s, a, G)
         case (Some(s), None) => infer(s, G) != Univ(2)
         case (None, Some(a)) => check(a, Univ(1), G) || check(a, Univ(2), G)
         case (None, None) => false
      }
   }
*/   def equality(tm1 : Term, tm2 : Term, meta: Context)(implicit lib : Lookup) : Substitution = {
      lib.report("LF: ", "equal\n" + tm1.toString + "\n" + tm2.toString)
      implicit val cd = new ConstraintDelay(meta)
      equal(tm1, tm2, Context())
      if (cd.solved)
        cd.solution
      else
        throw UnresolvedConstraints()
   }
 
/**
 * @param path the URI of a constant
 * @param lib  Lookup library
 * @return type of the constant
 * */  
/* def lookuptype(path : GlobalName)(implicit lib : Lookup) : Term = {
    val con = lib.getConstant(path)
    con.tp match {
       case Some(t) => t
       case None =>
          if (con.df.isEmpty) throw LFError("no type exists for " + path)
          infer(con.df.get, Context())
    }
 }
*/  def lookupdef(path : GlobalName)(implicit lib : Lookup) : Option[Term] = lib.getConstant(path).df

  /**
   * if G |- tm1 : A and G |- tm2 : A, then equal(tm1,tm2,G) iff G |- tm1 = tm2 : A
   */
   def equal (tm1 : Term, tm2 : Term, G : Context)(implicit lib : Lookup, cd: ConstraintDelay) {
      (tm1, tm2) match { 
         case (OMV(x), OMV(y)) => x == y //TODO: variables with definients
         case (OMS(c), OMS(d)) => if (c == d) true else {
            //TODO: smart strategy for definition expansion
            lookupdef(c) match {
               case None => lookupdef(d) match {
                  case None => false
                  case Some(t) => equal(OMS(c), t, G)
               }
               case Some(t) => equal(OMS(d), t, G) //flipping the order so that if both c and d have definitions, d is expanded next 
            }
         }
/*         case (Lambda(x1,a1,t1), Lambda(x2,a2,t2)) => 
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
*/         case _ => tm1 == tm2
      }
   }

}


object Test {
   val controller = new Controller
   def log(msg : => String) = controller.report("user", msg)

   def main(args : Array[String]) : Unit = {
      controller.handle(ExecFile(new java.io.File("test-init.mmt")))
      val oc = new ObjectChecker(controller.report)
      val t1 = OMV("x")
      val t2 = OMV("x")
      log("solving " + t1 + " = " + t2)
      val sol = oc.equality(t1,t2,Context())(controller.globalLookup)
      log("solution: " + sol.toString)
   }

}

/*class LFF extends Foundation {
   *//**
    * check(s,T,G) iff G |- s : T : U for some U \in {type,kind}
    * checks whether a term s has type T  
    *//*
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

  
 *//**
  * if t is a constant or an application of a defined constant, it replaces the constant with its definition.
  * if t is a lambda expression application, it is beta-reduced 
  * removes top-level redex, application of defined constant
  * if t well-formed, then |- reduce(t,G) = t
  * if |- t = Pi x:A. B for some A,B, then reduce(t) of the form Pi x:A.B (?)
  *//*
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
            case None => t
         }
         case t => t
   }
 
 *//**
  * if exists A such that G |- s : A, then G |- infer(s,G) = A
  *//*
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
}
*/