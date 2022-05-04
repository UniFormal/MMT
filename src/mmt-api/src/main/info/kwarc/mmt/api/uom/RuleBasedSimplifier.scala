package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import checking._
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.symbols.{Constant, Declaration, PlainInclude}
import objects._
import objects.Conversions._

import scala.util.Try

case class SimplifierState(t : Term, unit: SimplificationUnit, rules: RuleSet, path: List[Int]) {
  def enter(i : Int) : SimplifierState = copy(path = i :: path)
  def exit(i : Int) : SimplifierState = copy(path = path.tail)
  override def toString = t.toString + "@" + path.mkString("_")
  /** precomputes the available simplification rules */
  val simpRules = rules.get(classOf[MatchingSimplificationRule])
  /** precomputes the available rules */
  val compRules = rules.getOrdered(classOf[ComputationRule])
  /** precomputes the available rules */
  val abbrevRules = rules.get(classOf[AbbrevRule])
  /** precomputes the available rules */
  val matchRules = rules.get(classOf[InverseOperator])
}

import RuleBasedSimplifier._

/** */

/**
  * A RuleBasedSimplifier applies DepthRule's and BreadthRule's exhaustively
  * to simplify a Term.
  *
  * ### Invariants
  *
  * Simplifying transforms a welltyped term into another welltyped term.
  * This especially means that [[Rule rules]] have to retain the type.
  *
  * Only [[AbbrevRule]] rules will be applied on [[OMID]] (sub)terms. This
  * makes sense given that OMIDs reference constants, for which other
  * reference to other constants could never take place given the welltyping
  * constraint above. The only way for an [[OMID]] to be rewritten is by
  * definitorial expansion, which can be enabled in [[SimplificationUnit]].
  *
  * TODO Add other invariants.
  */
class RuleBasedSimplifier extends ObjectSimplifier {self =>
  override val logPrefix = "object-simplifier"

   /** the main simplification method
    * @param obj the object to simplify
    * @param su additional arguments
    * @param rules rules to use (precomputed for efficiency)
    * @return the simplified Term (if a sensible collection of rules is used that make this method terminate)
    *
    * The input term must be fully strictified, and so will be the output term.
    * Applicability of rules is determined based on the pragmatic form (using StrictOMA).
    * Rules are passed strict terms and are expected to return strict terms.
    *
    * The code uses [[Simple]] and [[SimplificationResult]] to remember whether a term has been simplified.
    * Therefore, structure sharing or multiple calls to this method do not cause multiple traversals.
    */
   def apply(obj: Obj, su: SimplificationUnit, rules: RuleSet): obj.ThisType = {
      val context = su.context
      //log("called on " + controller.presenter.asString(obj) + " in context " + controller.presenter.asString(context))
      val result: Obj = obj match {
         case t: Term =>
            val initState = new SimplifierState(t, su, rules, Nil)
            val tS: Term =
              try {
                traverse(t,initState, context)
              } catch {case e: Exception =>
                // this should never happen; but if there is a bug, it's easier to locate this way
                throw GeneralError("error while simplifying " + controller.presenter.asString(obj) + "\n" + obj.toStr(true)).setCausedBy(e)
              }
            tS
         case c: Context =>
            c.mapTerms {case (sofar, t) => apply(t, su ++ sofar, rules)}
         case s: Substitution =>
            s.map {case Sub(x,t) => Sub(x, apply(t, su, rules))}
      }
      SimplificationResult.eraseDeep(result)
      // this is statically well-typed, but we need a cast because Scala does not see it
      result.asInstanceOf[obj.ThisType]
  }

   /** the code for traversing a term and recursively simplifying */
   private val traverse = new Traverser[SimplifierState] {
      // by marking with and testing for Simplified(_), we avoid traversing a term twice
      // Note that certain operations remove the simplified marker: changing the toplevel, substitution application
      def traverse(t: Term)(implicit context : Context, state: SimplifierState) : Term = {
       if (state.unit.isKilled) return t
       val Stability = state.unit.solverO.map(_.stability)
       def setStable(o: Obj) {
         Stability.foreach {s => s.set(o)}
       }
       def getStable(o: Obj) = {
         Stability.map(s => s.is(o)).getOrElse(false)
       }
       def isStable(o: Obj) = o match {
         case t: Term => getStable(t)
         case Sub(x,t) => getStable(t)
         case vd: VarDecl => vd.tp.forall(getStable) && vd.df.forall(getStable)
       }
       //log("traversing into " + controller.presenter.asString(t))
       //log("in context " + controller.presenter.asString(context))
       t match {
         //TODO strangely, taking the optimization out introduces a checking error in mizar.mmt
         // this term was simplified before resulting in tS
         case SimplificationResult(tS) =>
           log("structure-shared term was already simplified")
           tS
         case OMAorAny(Free(cont,bd), args) if cont.length == args.length =>
           // MMT-level untyped beta-reduction using 'free' as 'lambda'
           // should only be needed if we expand the definition of an unknown variable
           val sub = cont /! args // defined due to guard
           val tC = bd ^? sub
           traverse(tC.from(t))
         // apply morphisms TODO should become computation rule once module expressions are handled properly
         case OMM(tt, mor) =>
            val tM = controller.globalLookup.ApplyMorphs(tt, mor)
            traverse(tM.from(t))
         // the main case
         case ComplexTerm(op,subs,cont,args) => logGroup {
          //log("state is" + init.t + " at " + init.path.toString)
          var recPosComp: CannotSimplify = Simplifiability.NoRecurse
          val cb = state.unit.solverO.getOrElse(callback(state))
          state.compRules.foreach {rule =>
            if (rule.applicable(t)) {
              val ret = rule(cb)(t, true)(Stack(context), NoHistory)
              ret match {
                case Simplify(tmS) =>
                  log(rule.toString + ": " + t + " ~> " + tmS)
                  return traverse(tmS.from(t))
                case cannot: CannotSimplify =>
                  recPosComp = recPosComp join cannot
              }
            }
          }
          var recPosSimp: CannotSimplify = Simplifiability.NoRecurse
          state.simpRules foreach {rule =>
            val ret = rule(context, state.rules, t)
            ret match {
              case Simplify(tmS) =>
                // redundancy to catch subtle errors
                if (tmS == t) {
                  throw ImplementationError("rule " + rule + " simplified term to itself: " + tmS)
                }
                log(rule.toString + ": " + t + " ~> " + tmS)
                return traverse(tmS.from(t))
              case cannot: CannotSimplify =>
                recPosSimp = recPosSimp join cannot
            }
          }
          // no applicable rule: recurse according to simp
          val top = t.subobjects.length
          val stabilityCriticalPos = (recPosSimp join recPosComp).getPositions(top)
          val recursePositions = if (state.unit.fullRecursion) 1 to top else stabilityCriticalPos
          var changed = false
          var stable = true
          // we go through all arguments and try to simplify each one of them
          val subobjsNew = t.subobjects.zipWithIndex.tail.map {case ((c,o),i) =>
            if (!recursePositions.contains(i)) {
              o // only recurse if this is one of the recurse positions and no previous subobject has changed
            } else {
              val oN = traverseObject(o)(context++c,state)
              val ch = oN != o
              changed ||= ch
              if (stabilityCriticalPos contains i) {
                 // we only make terms unstable if a computation rule wanted to recurse
                 // that's not always correct, but using all rules often makes the intended solution of an unknown not-unique
                 stable &&= isStable(oN)
              }
              if (ch) oN else o
            }
          }
          val tS = if (changed) {
            ComplexTerm(op,subobjsNew).from(t)
          } else {
            t
          }
          if (changed) {
            traverse(tS)
          } else {
            if (stable) {
              setStable(tS)
            }
            tS.from(t)
            SimplificationResult.put(t, tS) // store result to recall later in case of structure sharing
            tS
          }
          //log("simplified to " + controller.presenter.asString(tS))
        }
        // expand abbreviations but not definitions
        case OMS(p) =>
          state.abbrevRules.filter(_.head == p) foreach {rule =>
            log(rule.toString + ": " + t.toStr(true) + " ~> " + rule.term.toStr(true))
            return traverse(rule.term.from(t))
          }
          // TODO does not work yet; how does definition expansion interact with other steps?
          if (state.unit.expandDefinitions) {
            val pCons = controller.globalLookup.getO(ComplexTheory(context), p.toLocalName)
            val pDf = pCons.flatMap {
              case c: Constant if c.df.isDefined =>
                if (c.getOrigin.transient) {
                  c.df
                } else {
                  // if this declaration could come up again, we normalize it at its origin and cache the normalization
                  normalizeConstant(c, state.unit.fullRecursion)
                  c.dfC.normalized
                }
              case _ =>
                None
            }
            pDf match {
              // TODO d must be traversed in a smaller context: d may refer to parameters of c.home that may be shadowed in the current context  
              case Some(d) =>
                traverse(d) // need to traverse normalized term because the present context might have more rules than the one of c
              case None =>
                setStable(t)
                t
            }
          } else {
            // no definition expansion; but we can still mark this as stable if there is no definition anyway
            /*val stable = controller.localLookup.getO(p) match {
              case Some(c: Constant) =>
                c.df.isEmpty
              case _ =>
                false
            }
            if (stable) setStable(t)*/
            t
          }
         // expand definitions of variables
         case OMV(n) =>
           val vdO = try {
             context(n).df
           } catch {case le: LookupError =>
             // this should be impossible, but implementations errors are hard to trace if not caught here
             throw ImplementationError("simplification was called on ill-formed context-object pair").setCausedBy(le)
           }
           vdO match {
             case Some(d) =>
               log("expanding and simplifying definition of variable " + n)
               traverse(d)(context.before(n), state)
             case None =>
               //special case to avoid marking an unknown as stable
               val isUnknown = state.unit.solverO match {
                 case Some(solver) => solver.Unknown.unapply(t).isDefined
                 case _ => false
               }
               if (!isUnknown)
                 setStable(t)
               t
           }
         // literals read from XML may not be recognized yet
         case u: UnknownOMLIT =>
           val uR = controller.recognizeLiteral(state.rules, u).getOrElse(u)
           setStable(uR)
           uR
         case _ =>
            val tS = Traverser(this, t)
            if (tS != t) {
              traverse(tS)
            } else {
              SimplificationResult.put(t,tS)
              tS
            }
       }
      }
   }

  /** fully normalizes the definiens of a constant and stores the result with the Constant
   *  
   *  Because the result is stored within the constant and to avoid shadowing problems,
   *  this only uses the context of where the constant is declared, not where it is referenced.
   *  This may under-normalize occasionally.
   */
  private def normalizeConstant(c: Constant, fullRec: Boolean) {
    c.dfC.normalize {u =>
      val cont = controller.getContext(c)
      val rs = RuleSet.collectRules(controller, cont)
      self.apply(u, SimplificationUnit(cont, true, fullRec), rs)
    }
  }
  
  /** callback for calling checking rules, used in applyCompRules */
  private def callback(state: SimplifierState) = new CheckingCallback {
    def check(j: Judgement)(implicit history: History) = j match {
      case j: Equality =>
        val tm1 = apply(j.tm1, state.unit++j.context, state.rules)
        val tm2 = apply(j.tm2, state.unit++j.context, state.rules)
        (tm1,tm2) match {
          case (OMLIT(v1,_),OMLIT(v2,_)) => v1 == v2
          case _ => tm1 == tm2
        }
      case j: EqualityContext =>
        apply(j.context1, state.unit++j.context, state.rules) == apply(j.context2, state.unit++j.context, state.rules)
      case _ => false
    }

    def lookup = controller.globalLookup
    def simplify(t: Obj)(implicit stack: Stack, history: History) = {
      apply(t, state.unit++stack.context, state.rules)
    }
    def outerContext = Context.empty

    def getTheory(tm : Term)(implicit stack : Stack, history : History) : Option[AnonymousTheory] = simplify(tm) match {
       case AnonymousTheoryCombinator(at) =>
         Some(at)
       // add include of codomain of mor
       case OMMOD(mp) =>
         val th = Try(controller.globalLookup.getTheory(mp)).getOrElse(return None)
         val ds = th.getDeclarationsElaborated.map({
           case c: Constant =>
             OML(c.name, c.tp, c.df, c.not)
           case PlainInclude(from, _) =>
             IncludeOML(from, Nil, None)
           case _ => ??? //TODO
         })
         Some(new AnonymousTheory(th.meta, ds))
       case _ =>
         return None
    }

    private val sO = state.unit.solverO

    // TODO specify if this is allowed. Some ComputationRules call safeSimplifyUntil even if covered=true and may be surprised that all rules are applied.
    override def safeSimplifyUntil[A](tm: Term)(simple: Term => Option[A])(implicit stack: Stack, history: History): (Term, Option[A]) = {
      if (sO.isDefined) sO.get.safeSimplifyUntil(tm)(simple) else {
        val s = simplify(tm)
        (s,simple(s))
      }
    }

    override def inferType(t: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      if (sO.isDefined) sO.get.inferType(t,covered) else super.inferType(t, covered)
    }
  }
}

object RuleBasedSimplifier {
  /**
   * used to store the result of simplifying a Term in the original term so that it can be reused
   */
  object SimplificationResult extends TermProperty[Term](utils.mmt.baseURI / "clientProperties" / "uom" / "result") {
     def unapply(t: Term) = get(t)
  }
}
