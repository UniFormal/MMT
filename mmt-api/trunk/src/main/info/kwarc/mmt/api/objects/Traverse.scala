package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import libraries._

/** 
 * A Continuation wraps a function that traverses a Term.
 */
abstract class Continuation[State] {
   /** the traversal function
    *  it does not call itself recursively,
    *  instead another Continuation is taken as an argument and used for the recursive calls
    */
	def apply(cont : Continuation[State], t : Term)(implicit con : Context, state: State) : Term
}

/**
 * OneStep is an auxiliary Continuation that traverses one level into a Term doing nothing else.
 * Thus, OneStep.apply(OneStep, t)(con, state) = t
 * A Continuation can focus on the interesting cases of an induciton and delegate all the boring cases to OneStep.  
 */
class OneStep[State] extends Continuation[State] {
	def apply(cont : Continuation[State], t : Term)(implicit con : Context, state : State) : Term = {
       def rec(t: Term)(implicit con : Context, state : State) = cont(this, t)(con, state)
	   t match {
		   case OMA(fun,args) => OMA(rec(fun), args.map(rec))
		   case OME(err,args) => OMA(rec(err), args.map(rec))
		   case OMM(arg,via) => OMM(rec(arg), via)
		   case OMATTR(arg,key,value) => OMATTR(rec(arg), key, rec(value))
		   case OMBINDC(b,vars,cond,body) =>
		      val newvars = vars
		      val newcon = con ++ vars
		      OMBINDC(rec(b), newvars, cond.map(x => rec(x)(newcon, state)), rec(body)(newcon, state))
		   case OMID(_) => t
		   case OMV(_) => t
		   case OMHID => t
		   case OMFOREIGN(_) => t
		   case OMI(_) => t
		   case OMSTR(_) => t
		   case OMF(_) => t
       }
   }
}

/**
 * A Traverser is a function on Term defined by context-sensitive induction.
 * 
 * Some auxiliary classes are used to hide the actual traversal and permit authors to implement only the important cases.
 * During the traversion, a value of type State is maintained that may be used to carry along state.  
 */
abstract class Traverser[State] extends Continuation[State] {
   /** the main method to call the traverser, context defaults to empty */
	def apply(t: Term, init : State, con : Context = Context()) : Term = apply(new OneStep[State], t)(con, init)
}

/** A Traverser that moves all morphisms to the inside of a term. */
object PushMorphs extends Traverser[Morph] {
   // morph is the composition of all morphisms encountered so far 
	def apply(cont : Continuation[Morph], t: Term)(implicit con : Context, morph : Morph) : Term = t match {
	   // change state: via is added to the morphisms
		case OMM(arg, via) => cont(this, arg)(con, morph * via)
		// apply the morphism to symbols
		case OMID(path) => OMM(t, morph)
		// in all other cases, traverse
		case t => cont(this,t)
	}
	def apply(t: Term, thy : info.kwarc.mmt.api.MPath) : Term = apply(t, OMIDENT(OMMOD(thy)))
}

/** A Traverser that applies a fixed subsitution to a Term */
object Substitute extends Traverser[Substitution] {
	def apply(cont : Continuation[Substitution], t: Term)(implicit con : Context, subs: Substitution) : Term = t match {
		// substitute the free but not the bound variables
	   case OMV(n) => if (con.isDeclared(n)) OMV(n) else subs(n)
	   // in all other cases, traverse
		case t => cont(this,t)
	}
}

