package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import libraries._
import Conversions._

/**
 * A Traverser is a function on Term defined by context-sensitive induction.
 * 
 * The auxiliary methods in the companion object can be used to handle all cases that traverse the object without any change.
 * During the traversal, a value of type State may be used to carry along state.  
 */
abstract class Traverser[State] {
   /** the main method to call the traverser, context defaults to empty */
   def apply(t: Term, init : State, con : Context = Context()) : Term = apply(t)(con, init)
   def apply(t: Term)(implicit con : Context, init : State) : Term
}

/**
 * A StatelessTraverser is like a Traverser but does not carry a state during the traversal.  
 */
abstract class StatelessTraverser extends Traverser[Unit]

object Traverser {
   /**
    * This method traverses one level into a Term without changing anything and recursively calling a given Traverser.  
    */
   def apply[State](trav : Traverser[State], t : Term)(implicit con : Context, state : State) : Term = {
      def rec(t: Term)(implicit con : Context, state : State) = trav.apply(t)(con, state)
      def recCon(c: Context)(implicit con : Context, state : State) : Context =
         c.zipWithIndex map {
            case (VarDecl(n, t, d, attv @ _*), i) =>
               val conci = con ++ c.take(i+1)
               val newt = t.map(rec( _)(conci, state))
               val newd = d.map(rec( _)(conci, state))
               VarDecl(n, newt, newd, attv :_*)  //TODO traversal into attributions
         }
      def recSub(s: Substitution)(implicit con : Context, state : State) : Substitution =
         s map {
           case Sub(n,t) => Sub(n, rec(t))
         }
	   t match {
		   case OMA(fun,args) => OMA(rec(fun), args.map(rec))
		   case OME(err,args) => OME(rec(err), args.map(rec))
		   case OMM(arg,via) => OMM(rec(arg), via)
		   case OMATTR(arg,key,value) => OMATTR(rec(arg), key, rec(value)) //TODO traversal into key
		   case OMBINDC(b,vars,cond,body) => OMBINDC(rec(b), recCon(vars), cond.map(x => rec(x)(con ++ vars, state)), rec(body)(con ++ vars, state))
		   case OMID(_) => t
		   case OMV(_) => t
		   case OMHID => t
		   case OMREF(uri, value, under) => OMREF(uri, value map rec, recSub(under))
		   case OMFOREIGN(_) => t
		   case OMI(_) => t
		   case OMSTR(_) => t
		   case OMF(_) => t
		   case OMSemiFormal(tokens) => 
		      val newtokens = tokens map {
		         case Formal(t) => Formal(rec(t))
		         case i => i  // One might want to recurse into informal objects here as well
		      }
		      OMSemiFormal(newtokens)
		   
       }
   }
}

/** A Traverser that moves all morphisms to the inside of a term. */
object PushMorphs extends Traverser[Term] {
   // morph is the composition of all morphisms encountered so far 
	def apply(t: Term)(implicit con : Context, morph : Term) : Term = t match {
	   // change state: via is added to the morphisms
		case OMM(arg, via) => apply(arg)(con, morph * via)
		// apply the morphism to symbols
		case OMID(path) => OMM(t, morph)
		// in all other cases, traverse
		case t => Traverser.apply(this,t)(con, morph)
	}
	def apply(t: Term, thy : info.kwarc.mmt.api.MPath) : Term = apply(t, OMIDENT(OMMOD(thy)))
}

/** A Traverser that applies a fixed substitution to a Term */
object Substitute extends Traverser[Substitution] {
	def apply(t: Term)(implicit con : Context, subs: Substitution) : Term = t match {
		// substitute the free but not the bound variables
	   case OMV(n) => if (con.isDeclared(n)) OMV(n) else subs(n) match {
	  	   case Some(t: Term) => t
	  	   case None => OMV(n)
	  	   case Some(_) => throw SubstitutionUndefined(n, "substitution is applicable but does not provide a term")
	   }
	   // in all other cases, traverse
		case t => Traverser.apply(this,t)(con, subs)
	}
}

/** a Traverser that replaces all references with their resolution if available */
object Flatten extends StatelessTraverser {
   def apply(t: Term)(implicit con : Context, init: Unit) : Term = t match {
      case r: OMREF if (r.isDefined) => apply(r.get.get)
      case _ => Traverser(this,t)
   }
}