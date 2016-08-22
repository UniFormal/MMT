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
abstract class Traverser[A] {
   protected type State = A
   /** the main method to call the traverser, context defaults to empty */
   def apply(t: Term, init : State, con : Context = Context()) : Term = traverse(t)(con, init)
   def traverse(t: Term)(implicit con : Context, init : State) : Term
}

/**
 * A StatelessTraverser is like a Traverser but does not carry a state during the traversal.  
 */
abstract class StatelessTraverser extends Traverser[Unit] {
   def apply(t: Term, con : Context) : Term = traverse(t)(con, ())
}

object Traverser {
   /**
    * This method traverses one level into a Term without changing anything and recursively calling a given Traverser.  
    */
   def apply[State](trav : Traverser[State], t : Term)(implicit con : Context, state : State) : Term = {
      def rec(t: Term)(implicit con : Context, state : State) = trav.traverse(t)(con, state)
      def recCon(c: Context)(implicit con : Context, state : State) : Context =
         c.mapVarDecls {
            case (before, vd @ VarDecl(n, t, d, _)) =>
               val curentContext = con ++ before
               val newt = t.map(rec(_)(curentContext, state))
               val newd = d.map(rec(_)(curentContext, state))
               vd.copy(tp = newt, df = newd)
         }
	   t match {
	      case ComplexTerm(op, subs, bound, args) =>
	         val newSubs = subs.map(s => s.copy(target = rec(s.target)))
	         val newArgs = args.map(a => rec(a)(con ++ bound, state))
	         ComplexTerm(op, newSubs, recCon(bound), newArgs).from(t)
	      case OMA(f, args) => 
	         val newArgs = args.map(a => rec(a))
	         OMA(rec(f), newArgs).from(t)
	      case OMPMOD(p, args) =>
	         val newArgs = args.map(rec)
	         OMPMOD(p, newArgs).from(t)
		   case OMID(_) => t
		   case OMV(_) => t
			 case OML(VarDecl(n, tp, df, nt)) => OML(VarDecl(n, tp map rec, df map rec, nt)).from(t)
		   case t: OMLITTrait => t
		   case OMFOREIGN(_) => t
		   case OMATTR(arg,key,value) => OMATTR(rec(arg), key, rec(value)).from(t) //TODO traversal into key
		   case OMSemiFormal(tokens) => 
		      val newtokens = tokens map {
		         case Formal(t) => Formal(rec(t))
		         case i => i  // One might want to recurse into informal objects here as well
		      }
		      OMSemiFormal(newtokens).from(t)
       }
   }
}

/** A Traverser that moves all morphisms to the inside of a term. */
object PushMorphs extends Traverser[Term] {
   // morph is the composition of all morphisms encountered so far 
	def traverse(t: Term)(implicit con : Context, morph : Term) : Term = t match {
	   // change state: via is added to the morphisms
		case OMM(arg, via) => traverse(arg)(con, OMCOMP(via, morph))
		// apply the morphism to symbols
		case OMID(path) => OMM(t, morph)
		// in all other cases, traverse
		case t => Traverser(this,t)
	}
	def apply(t: Term, thy : MPath) : Term = apply(t, OMIDENT(OMMOD(thy)))
}