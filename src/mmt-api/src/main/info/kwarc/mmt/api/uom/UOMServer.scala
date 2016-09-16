package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._

case class UOMState(t : Term, context: Context, rules: RuleSet, path : List[Int]) {
  def enter(i : Int) : UOMState = new UOMState(t, context, rules, i :: path)
  def exit(i : Int) : UOMState = new UOMState(t, context, rules, path.tail)
  override def toString = t.toString + "@" + path.mkString("_")
  /** precomputes the available rules */
  val depthRules = rules.get(classOf[DepthRule])
  /** precomputes the available rules */
  val breadthRules = rules.get(classOf[BreadthRule])
  /** precomputes the available rules */
  val compRules = rules.get(classOf[ComputationRule])
  /** precomputes the available rules */
  val abbrevRules = rules.get(classOf[AbbrevRule])
  /** precomputes the available rules */
  val matchRules = rules.get(classOf[InverseOperator])
}

/** A RuleBasedSimplifier applies DepthRule's and BreadthRule's exhaustively to simplify a Term */
// This class used to be called UOM
class RuleBasedSimplifier extends ObjectSimplifier {
  override val logPrefix = "uom"

  private lazy val StrictOMA = controller.pragmatic.StrictOMA
   
   /** the main simplification method
    * @param t the term to simplify
    * @param context its context, if non-empty
    * @return the simplified Term (if a sensible collection of rules is used that make this method terminate)
    * 
    * The input term must be fully strictified, and so will be the output term.
    * Applicability of rules is determined based on the pragmatic form (using StrictOMA).
    * Rules are passed strict terms and are expected to return strict terms.
    * 
    * The code uses [[Simple]] and [[SimplificationResult]] to remember whether a term has been simplified.
    * Therefore, structure sharing or multiple calls to this method do not cause multiple traversals. 
    */
   def apply(obj: Obj, context: Context, rules: RuleSet): obj.ThisType = {
      log("called on " + controller.presenter.asString(obj) + " in context " + controller.presenter.asString(context))
      val result = obj match {
         case t: Term =>
            val initState = new UOMState(t, context, rules, Nil)
            val tS: Term = traverse(t,initState, context)
            tS
         case c: Context =>
            c.mapTerms {case (sofar, t) => apply(t, context ++ sofar, rules)}
         case s: Substitution =>
            s.map {case Sub(x,t) => Sub(x, apply(t, context, rules))}
      }
      // this is statically well-typed, but we need a cast because Scala does not see it
      result.asInstanceOf[obj.ThisType]
  }
   
   /** the code for traversing a term and recursively simplifying */
   private val traverse = new Traverser[UOMState] {
      // by marking with and testing for Simplified(_), we avoid traversing a term twice
      // Note that certain operations remove the simplified marker: changing the toplevel, substitution application 
      def traverse(t: Term)(implicit con : Context, init: UOMState) : Term = t match {
         // this term is the result of simplification
         /*case Simple(t) =>
            log("already simplified " + controller.presenter.asString(t))
            t*/
         // this term was simplified before resulting in tS
         case SimplificationResult(tS) => tS
         case OMM(t, mor) =>
            val tM = controller.globalLookup.ApplyMorphs(t, mor)
            traverse(tM)
         case OMA(OMS(_), _) =>
            log("trying to simplify " + controller.presenter.asString(t))
            logGroup {
               //log("state is" + init.t + " at " + init.path.toString)
               val (tS, globalChange) = logGroup {
                  applyAux(t)
               }
               //log("simplified to " + controller.presenter.asString(tS))
               val tSM = Simple(tS.from(t))
               SimplificationResult.put(t, tSM) // store result to recall later in case of structure sharing
               if (globalChange)
                  Changed(tSM)
               else
                  tSM
            }
         case OMS(p) =>
            applyAbbrevRules(p) match {
              case GlobalChange(tS) => tS.from(t)
              case NoChange => Simple(t)
              // LocalChange impossible
            }
         case _ =>
            val tS = Simple(Traverser(this, t))
            SimplificationResult.put(t, tS)
            tS
      }

     /** an auxiliary method of apply that applies simplification rules
       * This method exhaustively applies rules as follows:
       *  (1) --depth rules--> (2) --simplify arguments--> (3) --breadth rules--> (return)
       * If any of the operations causes changes, the automaton goes back to state (1),
       * but some optimizations are used to avoid traversing a previously-simplified term again.
       * @param t the term to simplify (rules apply only to terms OMA(OMS(_),_)) 
       * @param globalChange true if there has been a GlobalChange so far
       * @return the simplified term and a Boolean indicating whether a GlobalChange occurred
       */
      def applyAux(t: Term, globalChange: Boolean = false)(implicit con : Context, init: UOMState) : (Term, Boolean) = t match {
         case StrictOMA(strictApps, outer, args) =>
            // state (1)
            //log("applying depth rules to   " + t)
            applyDepthRules(outer, Nil, args) match {
               case GlobalChange(tS) =>
                  // go back to state (1), remember that a global change was produced
                  applyAux(tS, true)
               case NoChange =>
                  // applyDepthRules returns a LocalChange even if no depth rule was applicable
                  throw ImplementationError("impossible case")
               case LocalChange(argsS) =>
                  // state (2) 
                  val argsSS = argsS.zipWithIndex map {
                     case (a,i) => traverse(a)(con, init.enter(i + 1)) // +1 adjusts for the f in OMA(f, args)
                  }
                  val tS = StrictOMA(strictApps, outer, argsSS)
                  // if any argument changed globally, go back to state (1)
                  if (argsSS exists {
                      case Changed(t) => Changed.erase(t); true
                      case _ => false
                   })
                      applyAux(tS, globalChange)
                  else {
                     //state (3)
                     //log("applying breadth rules to " + outer(argsSS))
                     applyBreadthRules(outer, argsSS) orelse applyCompRules(t) match {
                        case GlobalChange(tSS) =>
                           // go back to state (1), remember that a global change was produced
                           applyAux(tSS, true)
                        case LocalChange(argsSSS) =>
                           // go back to state (1)
                           applyAux(StrictOMA(strictApps, outer, argsSSS), globalChange)
                        case NoChange =>
                           // state (4)
                           (tS, globalChange)
                     }
                  }
            }
         // no rules applicable
         case _ => (t, globalChange)
      }
   }
  
  /** object for matching the inner term in a depth rule */
  class InnerTermMatcher(controller: frontend.Controller, matchRules: List[InverseOperator]) {
     /**
      * unifies matching OMA, strict OMS, OMS, literals that can be the result of applying a realized operator
      * @return list of matches: tuples of operator, arguments, flag signalling whether the inner term is an OMS
      */
     def matches(t: Term): List[(GlobalName, List[Term], Boolean)] = t match {
        case StrictOMA(strApps, p, args) => List((p, args, false))
        case OMS(p) => List((p, Nil, true))
        case l: OMLIT =>
           matchRules.flatMap {m =>
              m.unapply(l) match {
                 case None => Nil
                 case Some(args) => List((m.head, args, args.isEmpty)) 
              }
           }
           Nil
        case _ => Nil
     } 
  }

   /** applies all DepthRule's that are applicable at toplevel of an OMA
    * for each arguments, all rules are tried
    * if a rule leads to a GlobalChange, we stop; otherwise, we go to the next argument
    * @param outer the toplevel symbol
    * @param before the arguments that have been checked already
    * @param after the arguments that still need to be checked
    * @return GlobalChange if a rule led to it; LocalChange otherwise (even if no rule was applicable)
    */
   private def applyDepthRules(outer: GlobalName, before: List[Term], after: List[Term])(implicit state: UOMState): Change = {
      val itm = new InnerTermMatcher(controller, state.matchRules.toList)
      after match {
         case Nil => LocalChange(before) //we don't know if 'before' has a change; but if not, returning NoChange would not actually help in applyAux anyway
         case arg::afterRest =>
            itm.matches(arg) foreach {
               case (inner, inside, isOMS) =>
                  state.depthRules.filter(r => r.outer == outer && r.inner == inner) foreach {rule =>
                     val ch = rule.apply(before, inside, afterRest)
                     ch match {
                        case NoChange =>
                        case LocalChange(args) =>
                           return applyDepthRules(outer, before, args ::: afterRest)
                        //return immediately upon GlobalChange
                        case GlobalChange(tS) =>
                           log("simplified to " + controller.presenter.asString(tS))
                           return GlobalChange(tS)
                     }
                  }
            }
            applyDepthRules(outer, before ::: List(arg), afterRest)
      }
   }
   
  /** applies all BreadthRule's that are applicable at toplevel of an OMA
    * @param outer the toplevel symbol
    * @param args the arguments
    * */
   private def applyBreadthRules(op: GlobalName, inside: List[Term])(implicit state: UOMState): Change = {
      var insideS = inside
      var changed = false
      state.breadthRules.filter(_.head == op) foreach {rule =>
         val ch = rule.apply.apply(insideS)
         log("rule " + rule + ": " + ch)
         ch match {
            case NoChange =>
            case LocalChange(args) =>
               insideS = args
               changed = true
            case GlobalChange(t) =>
              log("simplified to " + controller.presenter.asString(t))
              return GlobalChange(t)
         }
      }
      //we have to check for insideS == inside here in case a BreadthRule falsely thinks it changed the term (such as commutativity when the arguments are already in normal order)
      if (! changed || insideS == inside) NoChange else LocalChange(insideS)
   }
   
   private def applyAbbrevRules(p: GlobalName)(implicit state: UOMState): Change = {
      state.abbrevRules.filter(_.head == p) foreach {rule =>
         return GlobalChange(rule.term)
      }
      NoChange
   }

   /** callback for calling checking rules, used in applyCompRules */
   private def callback(state: UOMState) = new CheckingCallback {
      def check(j: Judgement)(implicit history: History) = j match {
         case j: Equality =>
            apply(j.tm1, j.context, state.rules) == apply(j.tm2, j.context, state.rules)
         case j: EqualityContext =>
            apply(j.context1, j.context, state.rules) == apply(j.context2, j.context, state.rules)
         case _ => false
      }
      def simplify(t: Term)(implicit stack: Stack, history: History) =
         apply(t, stack.context, state.rules)
   }

   /** applies all computation rules */
   private def applyCompRules(tm: Term)(implicit context: Context, state: UOMState): Change = {
      val cb = callback(state)
      state.compRules.foreach {rule =>
         if (tm.head.contains(rule.head)) {
           rule(cb)(tm, true)(Stack(context), NoHistory).foreach { tmS =>
             return GlobalChange(tmS)
           }
         }
      }
      NoChange
   }
}

/**
 * UOM uses this to remember that a Term has been simplified already to avoid recursing into it again 
 */
object Simple extends BooleanTermProperty(utils.mmt.baseURI / "clientProperties" / "uom" / "simplified")

/**
 * UOM uses this to remember whether a Term underwent a GlobalChange during simplification
 * this information is passed upwards during recursive simplification
 */
object Changed extends BooleanTermProperty(utils.mmt.baseURI / "clientProperties" / "uom" / "changed")

/**
 * UOM uses this to store the result of simplifying a Term in the original term so that it can be reused 
 */
object SimplificationResult extends TermProperty[Term](utils.mmt.baseURI / "clientProperties" / "uom" / "result") {
   def unapply(t: Term) = get(t)
}