package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import checking._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import objects._
import objects.Conversions._

import scala.util.Try

case class SimplifierState(t : Term, unit: SimplificationUnit, rules: RuleSet, path: List[Int]) {
  def enter(i : Int) : SimplifierState = copy(path = i :: path)
  def exit(i : Int) : SimplifierState = copy(path = path.tail)
  override def toString = t.toString + "@" + path.mkString("_")
  /** precomputes the available simplification rules */
  val simpRules = rules.get(classOf[SimplificationRule])
  /** precomputes the available rules */
  val compRules = rules.getOrdered(classOf[ComputationRule])
  /** precomputes the available rules */
  val abbrevRules = rules.get(classOf[AbbrevRule])
  /** precomputes the available rules */
  val matchRules = rules.get(classOf[InverseOperator])
}

//TODO Simplifier should not be pragmatics-aware, instead each rule should have 'under' field

import RuleBasedSimplifier._

/** A RuleBasedSimplifier applies DepthRule's and BreadthRule's exhaustively to simplify a Term */
class RuleBasedSimplifier extends ObjectSimplifier {self =>
  override val logPrefix = "object-simplifier"

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

   private def isStable(o: Obj) = o match {
     case t: Term => Stability.is(t)
     case Sub(x,t) => Stability.is(t)
     case vd: VarDecl => vd.tp.forall(Stability.is(_)) && vd.df.forall(Stability.is(_))
   }
   /** the code for traversing a term and recursively simplifying */
   private val traverse = new Traverser[SimplifierState] {
      // by marking with and testing for Simplified(_), we avoid traversing a term twice
      // Note that certain operations remove the simplified marker: changing the toplevel, substitution application
      def traverse(t: Term)(implicit context : Context, state: SimplifierState) : Term = {
       //log("traversing into " + controller.presenter.asString(t))
       //log("in context " + controller.presenter.asString(context))
       t match {
         /* TODO this optimization saves 10-20% on examples (One would expect it to save more time.) by avoiding retraversal of simplified terms.
            However, there is a subtle problem: When looking up a type and simplifying it, the marker may be reintroduced on the previously checked term.
            Then later lookups will consider it simple even if they occur in a context with more simplification rules.
            The RuleBasedChecker already removes these markers after each CheckingUnit, but that is not enough.
         // this term is already the result of simplification, so return as is
         case Simple(t) =>
            log("term is already simple")
            t*/
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
          val cb = callback(state)
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
            val ret = rule(context, t)
            ret match {
              case Simplify(tmS) =>
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
          val recursePositions = if (state.unit.fullRecursion)
            1 to top
          else (recPosSimp join recPosComp).getPositions(top)
          val stabilityCriticalPos = recPosComp.getPositions(top)
          var changed = false
          var stable = true
          // we go through all arguments and try to simplify one of them
          val subobjsNew = t.subobjects.zipWithIndex.tail.map {case ((c,o),i) =>
            if (!recursePositions.contains(i)) {
              o // only recurse if this is one of the recurse positions and no previous subobjects has changed 
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
              Stability.set(tS)
            }
            Simple(tS.from(t))
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
            controller.globalLookup.getO(p) flatMap {
              case c: Constant =>
                normalizeConstant(c)
                c.dfC.normalized
              case _ =>
                None
            } match {
               // TODO d must be traversed in a smaller context: d may refer to parameters of c.home that may be shadowed in the current context  
               case Some(d) => traverse(d) // need to traverse normalized term because the present context might have more rules than the one of c
               case None =>
                 Stability.set(t)
                 Simple(t)
            }
           } else {
             // no definition expansion; but we can still mark this as stable if there is no definition anyway
             val stable = controller.localLookup.getO(p) match {
               case Some(c: Constant) => c.df.isEmpty
               case _ => false
             }
             if (stable) Stability.set(t)
             Simple(t)
           }
         // expand definitions of variables (Simple(_) prevents this case if the definiens is added later, e.g., when solving an unknown)
         case OMV(n) =>
           val vdO = try {
             context(n).df}
           catch {case le: LookupError =>
             // this should be impossible, but implementations errors are hard to trace if not caught here
             throw ImplementationError("simplification was called on ill-formed context-object pair").setCausedBy(le)
           }
           vdO match {
             case Some(d) =>
               log("expanding and simplifying definition of variable " + n)
               traverse(d)(context.before(n), state)
             case None =>
               //TODO awkward special case to avoid marking an unknown as stable
               val isUnknown = n.steps.length > 1 && (n.steps.head match {
                 case SimpleStep(s) => s.startsWith("/")
                 case _ => false
               })
               if (isUnknown)
                 Stability.set(t)
               t
           }
         // literals read from XML may not be recognized yet
         case u: UnknownOMLIT =>
           val uR = u.recognize(state.rules).getOrElse(u)
           Stability.set(uR)
           uR
         case _ =>
            val tS = Simple(Traverser(this, t))
            SimplificationResult.put(t, tS)
            tS
       }
      }
   }

  /** fully normalizes the definiens of a constant */
  private def normalizeConstant(c: Constant) {
    c.dfC.normalize {u =>
      val cont = controller.getContext(c)
      val rs = RuleSet.collectRules(controller, cont)
      self.apply(u, SimplificationUnit(cont, true, true), rs)
    }
  }
   
     /* old code
     /** an auxiliary method of apply that applies simplification rules
       * This method exhaustively applies rules as follows:
       *  (1) --depth rules--> (2) --simplify arguments--> (3) --breadth rules--> (return)
       * If any of the operations causes changes, the automaton goes back to state (1),
       * but some optimizations are used to avoid traversing a previously-simplified term again.
       * @param t the term to simplify (rules apply only to terms OMA(OMS(_),_))
       * @param globalChange true if there has been a GlobalChange so far
       * @return the simplified term and a Boolean indicating whether a GlobalChange occurred
       */
      private def applyAux(t: Term)(implicit con : Context, init: SimplifierState) : (Term, Boolean) = t match {
         case StrictOMA(strictApps, outer, args) =>
            // state (1)
            log("applying depth rules to   " + controller.presenter.asString(t))
            applyDepthRules(outer, Nil, args) match {
               case GlobalChange(tS) =>
                  // go back to state (1), remember that a global change was produced
                  applyAux(tS, true)
               case NoChange =>
                  // applyDepthRules returns a LocalChange even if no depth rule was applicable
                  throw ImplementationError("impossible case")
               case LocalChange(argsS) =>
                  // state (2)
                  log("simplifying function and arguments")
                  val funArgsSS : List[Term] = logGroup {
                    (OMS(outer) :: argsS).zipWithIndex map {
                      case (a,i) => traverse(a)(con, init.enter(i))
                    }
                  }
                  val outerS = funArgsSS.head
                  val argsSS = funArgsSS.tail
                  val tS = StrictOMA(strictApps, outerS, argsSS)
                  // if function or any argument changed globally, go back to state (1)
                  if (funArgsSS exists {
                      case Changed(tm) => Changed.erase(tm.asInstanceOf[Term]); true
                      case _ => false
                   })
                      applyAux(tS, globalChange)
                  else {
                     //state (3)
                     log("applying breadth and computation rules")
                     applyBreadthRules(outer, argsSS) orelse applyCompRules(tS) match {
                        case GlobalChange(tSS) =>
                           // go back to state (1), remember that a global change was produced
                           applyAux(tSS, true)
                        case LocalChange(argsSSS) =>
                           // go back to state (1)
                           applyAux(StrictOMA(strictApps, outerS, argsSSS), globalChange)
                        case NoChange =>
                           // state (4)
                           (tS, globalChange)
                     }
                  }
            }
         // traverse and apply computation rules for any other complex term
         case _: OMBINDC | _: OMA =>
           log("applyAux with " + controller.presenter.asString(t))
           val tS = Traverser.apply(this, t)
           applyCompRules(tS) match {
             case GlobalChange(tSS) =>
               applyAux(tSS, true)
             case _ =>
               // LocalChange is impossible
              (tS, globalChange)
            }
         // no rules applicable
         case _ => (t, globalChange)
      }

   /** applies all DepthRule's that are applicable at toplevel of an OMA
    * for each arguments, all rules are tried
    * if a rule leads to a GlobalChange, we stop; otherwise, we go to the next argument
    * @param outer the toplevel symbol
    * @param before the arguments that have been checked already
    * @param after the arguments that still need to be checked
    * @return GlobalChange if a rule led to it; LocalChange otherwise (even if no rule was applicable)
    */
   private def applyDepthRules(outer: GlobalName, before: List[Term], after: List[Term])(implicit state: SimplifierState): Change = {
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
   private def applyBreadthRules(op: GlobalName, inside: List[Term])(implicit state: SimplifierState): Change = {
      var insideS = inside
      var changed = false
      state.breadthRules.filter(_.head == op) foreach {rule =>
         val ch = rule.apply(insideS)
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
*/
  
  /** callback for calling checking rules, used in applyCompRules */
  private def callback(state: SimplifierState) = new CheckingCallback {
    def check(j: Judgement)(implicit history: History) = j match {
      case j: Equality =>
        apply(j.tm1, state.unit++j.context, state.rules) == apply(j.tm2, state.unit++j.context, state.rules)
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
       case AnonymousTheory(mt, ds) =>
         Some(new AnonymousTheory(mt, Nil))
       // add include of codomain of mor
       case OMMOD(mp) =>
         val th = Try(controller.globalLookup.getTheory(mp)).toOption match {
           case Some(th2: DeclaredTheory) => th2
           case _ => return None
         }
         val ds = th.getDeclarationsElaborated.map({
           case c: Constant =>
             OML(c.name, c.tp, c.df, c.not)
           case PlainInclude(from, to) =>
             IncludeOML(from, Nil)
           case _ => ??? //TODO
         })
         Some(new AnonymousTheory(th.meta, ds))
       case _ =>
         return None
    }

    override def safeSimplifyUntil[A](tm: Term)(simple: Term => Option[A])(implicit stack: Stack, history: History): (Term, Option[A]) = {
      val s = simplify(tm)
      (s,simple(s))
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

  /**
   * used to remember that a Term is the result of simplification to avoid recursing into it again
   */
  object Simple extends BooleanTermProperty(utils.mmt.baseURI / "clientProperties" / "uom" / "simplified")
}
