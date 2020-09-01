
package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.symbols.UniformTranslator

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
   def traverse(t: Term)(implicit con : Context, state : State) : Term
   def traverseContext(cont: Context)(implicit con : Context, state : State): Context = {
      cont.mapVarDecls {case (before, vd) =>
         val curentContext = con ++ before
         vd map {t => traverse(t)(curentContext, state)}
      }
   }

   /** traverses any object by mapping all the terms in it */
   def traverseObject(obj: Obj)(implicit con: Context, state: State): obj.ThisType = {
     val result = obj match {
       case t: Term => traverse(t)
       case c: Context => traverseContext(c)
       case vd: VarDecl => traverseContext(Context(vd)).variables.head
       case s: Substitution =>
         s.map {case Sub(x,t) => Sub(x, traverse(t))}
       case s: Sub => Sub(s.name, traverse(s.target))
     }
     // this is statically well-typed, but we need a cast because Scala does not see it
     result.asInstanceOf[obj.ThisType]
   }

   /** this traverser as a translator
    *  @param newInit creates a fresh initial state
    */
   def toTranslator(newInit: () => A): UniformTranslator = new symbols.UniformTranslator {
     def apply(c: Context, t: Term): Term = traverse(t)(c, newInit())
   }

  /** diagrammatic composition (first this, then that) */
  def compose(that: Traverser[A]): Traverser[A] = {
    val self = this

    new Traverser[A] {
      def traverse(t: Term)(implicit con: Context, state: State): Term = {
        that.traverse(self.traverse(t)(con, state))(con, state.asInstanceOf[A])
      }
    }
  }
}

/**
 * A StatelessTraverser is like a Traverser but does not carry a state during the traversal.
 */
abstract class StatelessTraverser extends Traverser[Unit] {
   def apply(t: Term, con : Context) : Term = traverse(t)(con, ())

   def toTranslator(): UniformTranslator = toTranslator(() => ())
}

object Traverser {
   /**
    * This method traverses one level into a Term without changing anything and recursively calling a given Traverser.
    */
   def apply[State](trav : Traverser[State], t : Term)(implicit con : Context, state : State) : Term = {
      def rec(t: Term)(implicit con : Context, state : State) = trav.traverse(t)(con, state)
      def recCon(c: Context)(implicit con : Context, state : State) : Context =
         c.mapVarDecls {case (before, vd) =>
            val curentContext = con ++ before
            vd.map(t => rec(t)(curentContext, state))
         }
      t match {
        case OMA(f, args) =>
            val newArgs = args.map(a => rec(a))
            OMA(rec(f), newArgs).from(t)
        case OMBINDC(b,bound,args) =>
            val newB = rec(b)
           val newArgs = args.map(a => rec(a)(con ++ bound, state))
           val newBound = trav.traverseContext(bound)
            OMBINDC(newB, newBound, newArgs).from(t)
        case OMPMOD(p, args) =>
            val newArgs = args.map(rec)
            OMPMOD(p, newArgs).from(t)
         case OMID(_) => t
         case OMV(_) => t
          case OML(n, tp, df,not,ft) => OML(n, tp map rec, df map rec,not,ft).from(t)
         case t: OMLITTrait => t //TODO also traverse synType?
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
