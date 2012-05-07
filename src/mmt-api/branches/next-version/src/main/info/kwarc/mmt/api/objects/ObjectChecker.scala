package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

import libraries._
import modules._
import symbols._
//import utils._
import frontend._
import objects.Conversions._
import scala.collection.mutable.HashSet

/** the type of object level judgments as used for typing and equality of terms */
abstract class Constraint {
  /** @return the set of names of the meta-variables occurring in this judgment
   *    Constraints must come with a Context binding all object variables occurring freely in any expressions;
   *    therefore, the remaining free variables are meta-variables
   */ 
  def freeVars : HashSet[LocalName]
}
/** represents an equality judgment, optionally at a type
 * context |- t1 = t2 : tp  if t = Some(tp)
 * or
 * context |- t1 = t2       if t = None
 */
case class EqualityConstraint(context: Context, t1: Term, t2: Term, t: Option[Term]) extends Constraint {
   lazy val freeVars = {
     val ret = new HashSet[LocalName]
     val fvs = context.freeVars_ ::: t1.freeVars_ ::: t2.freeVars_ ::: (t.map(_.freeVars_).getOrElse(Nil))
     fvs foreach {n => if (! context.isDeclared(n)) ret += n}
     ret
   }
}
/** represents a typing judgment
 * context |- tm : tp
 */
case class TypingConstraint(context: Context, tm: Term, tp: Term) extends Constraint {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = context.freeVars_ ::: tm.freeVars_ ::: tp.freeVars_
    fvs foreach {n => if (! context.isDeclared(n)) ret += n}
    ret
  }
}

//TODO case class InhabitationConstraint(name: LocalName, tp: Term) extends Constraint

/** A wrapper around a Constraint to maintain meta-information while that Constraint is delayed */
class DelayedConstraint(val constraint: Constraint) {
  private val freeVars = constraint.freeVars
  private var activatable = false
  /** This must be called whenever a variable that may occur free in this constraint has been solved */
  def solved(name: LocalName) {
     if (! activatable && (freeVars contains name)) activatable = true
  }
  /** @return true iff a variable has been solved that occurs free in this Constraint */
  def isActivatable: Boolean = activatable
}

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

/**
 * A Solver is used to solve a system of judgments about Term's, given by Constraint's,
 * by applying typing rules to validate the judgment.
 * The judgments may contain unknown variables (also called meta-variables or logic variables);
 * the solution is a Substitution that provides a closed Term for every unknown variable.
 * A new instance must be created for every system of judgments. 
 * @param controller used for looking up Foundation's and Constant's. No changes are made to controller.
 * @param unknown the list of all unknown variables in dependency order;
 *   unknown variables may occur in or as the types of each other
 */
class Solver(controller: Controller, unknowns: Context) {
   /** tracks the solution, like unknowns but a definiens is added for every solved variable */ 
   private var solution : Context = unknowns
   /** tracks the delayed constraints, in any order */ 
   private var delayed : List[DelayedConstraint] = Nil
   /** true if unresolved constraints are left */
   def hasUnresolvedConstraints : Boolean = ! delayed.isEmpty
   /** true if unsolved variables are left */
   def hasUnsolvedVariables : Boolean = solution.toSubstitution.isEmpty
   /** the solution to the constraint problem
    * @return None if there are unresolved constraints or unsolved variables; Some(solution) otherwise 
    */
   def getSolution : Option[Substitution] = if (delayed.isEmpty) solution.toSubstitution else None

   /** delays a constraint for future processing */
   private def delay(c: Constraint): Boolean = {
      val dc = new DelayedConstraint(c)
      delayed = dc :: delayed
      true
   }
   /** activates a previously delayed constraint if one of its free variables has been solved since */
   private def activate: Boolean = {
      delayed find {_.isActivatable} match {
         case None => true
         case Some(dc) => apply(dc.constraint)
      }
   }
   /** registers the solution for a variable; notifies all delayed constraints */
   //TODO solutions should also be propagated to currently active constraints
   private def solve(name: LocalName, value: Term): Boolean = {
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined)
         checkEquality(value, solved.df.get, solved.tp)(Context())
      else {
         solution = left ::: solved.copy(df = Some(value)) :: right
         delayed foreach {_.solved(name)}
         true
      }
   }
   /** applies this Solver to one constraint
    *  this method can be called multiple times to solve a system of constraints
    *  @param c the constraint
    *  @return false only if the constraints are unsatisfiable; true if constraints have been resolved or delayed  
    */
   def apply(c: Constraint): Boolean = {
     val subs = solution.toPartialSubstitution
     val result = c match {
        case TypingConstraint(con, tm, tp) =>
           checkTyping(tm ^ subs, tp ^ subs)(con ^ subs)
        case EqualityConstraint(con, tm1, tm2, tp) =>
           checkEquality(tm1 ^ subs, tm2 ^ subs, tp map {_ ^ subs})(con ^ subs)
     }
     activate
   }
   /** proves a TypingConstraint by recursively applying rules and solving variables where applicable,
    *  delays a constraint if unsolved variables preclude further processing
    *  checkTyping(tm, tp)(con) solves the judgment con |- tm : tp
    *  @return false only if the judgment does not hold; true if it holds or constraint have been delayed
    */
   private def checkTyping(tm: Term, tp: Term)(implicit con: Context): Boolean = {
      true
   }
   /** handles an EqualityConstraint by recursively applying rules and solving variables where applicable,
    *  delays a constraint if unsolved variables preclude further processing
    *  @param tpOpt if empty, tm1 and tm2 may be ill-typed; if non-empty, they must also type-check at that type
    *  @return false only if the judgment does not hold; true if it holds or constraint have been delayed
    */
   private def checkEquality(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit con: Context): Boolean = {
      // the common case of identical terms
      if (tm1 == tm2) tpOpt match {
         case None => true
         case Some(tp) => checkTyping(tm1, tp)
      } else (tm1, tm2) match {
         // if no case applied, try the remaining cases
         case _ => checkEquality2(tm1, tm2, tpOpt, true)
      }
   }
   /** like checkEquality, but contains all those cases that are tried twice:
    *  if no case applies, we flip tm1 and tm2 and try again; if still no case applies, we delay
    * @param firstTime true if called for the first time, false if called for the second time
    */
   private def checkEquality2(tm1: Term, tm2: Term, tpOpt: Option[Term], firstTime: Boolean)(implicit con: Context): Boolean = {
      (tm1, tm2) match {
         // |- x = t: solve x as t
         case (OMV(x), t) =>
            if (unknowns.isDeclared(x)) {
               if (! t.freeVars.isEmpty) {
                  solve(x, t)
                  //check x.tp=tp? or t:x.tp?
               } else {
                 delay(EqualityConstraint(con, tm1, tm2, tpOpt))
               }
            } else {
               delay(EqualityConstraint(con, tm1, tm2, tpOpt))
            }
         case _ =>
            // if no case applied, ...
            if (firstTime)
               // flip the arguments and try again, or ...
               checkEquality2(tm2, tm1, tpOpt, false)
            else
               // delay if we've tried that already
               delay(EqualityConstraint(con, tm2, tm1, tpOpt))
      }
   }
}

/*
/**
 * @param path the URI of a constant
 * @param lib  Lookup library
 * @return type of the constant
 */  
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

class LFF extends Foundation {
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
            case None => t
         }
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