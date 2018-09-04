package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import frontend._
import symbols._
import uom._
import utils._
import modules._
import objects.Conversions._
import objects._
import proving._
import parser.ParseResult

import scala.collection.mutable.HashSet

/** the essential subalgorithms of a bidirectional type-checking,
 *  factored out from the [[Solver]] class, which holds all bureaucracy
 */ 
trait SolverAlgorithms {self: Solver =>
  import state._
  private implicit val sa = new MemoizedSubstitutionApplier
  
   /** precomputes relevant rule sets, ordered by priority */
   private lazy val computationRules = rules.getOrdered(classOf[ComputationRule])
   private lazy val inferenceRules = rules.getOrdered(classOf[InferenceRule])
   private lazy val subtypingRules = rules.getOrdered(classOf[SubtypingRule])
   private lazy val typingRules = rules.getOrdered(classOf[TypingRule])
   private lazy val typebasedsolutionRules = rules.getOrdered(classOf[TypeBasedSolutionRule])
   private lazy val universeRules = rules.getOrdered(classOf[UniverseRule])
   private lazy val inhabitableRules = rules.getOrdered(classOf[InhabitableRule])
   private lazy val termBasedEqualityRules = rules.getOrdered(classOf[TermBasedEqualityRule])
   private lazy val termHeadBasedEqualityRules = rules.getOrdered(classOf[TermHeadBasedEqualityRule])
   private lazy val typeBasedEqualityRules = rules.getOrdered(classOf[TypeBasedEqualityRule])
   private lazy val solutionRules = rules.getOrdered(classOf[ValueSolutionRule])
   private lazy val typeSolutionRules = rules.getOrdered(classOf[TypeSolutionRule])
   private lazy val forwardSolutionRules = rules.getOrdered(classOf[ForwardSolutionRule])
   private lazy val abbreviationRules = rules.getOrdered(classOf[AbbrevRule])
   /* convenience function for going to the next rule after one has been tried */
   private def dropTill[A](l: List[A], a: A) = l.dropWhile(_ != a).tail
   private def dropJust[A](l: List[A], a:A) = l.filter(_ != a)

  // **********************************************************************
   
   /** infers the type of a term by applying InferenceRule's
    *
    * @param tm the term
    * @param stack its Context
    * @return the inferred type, if inference succeeded
    *
    * This method should not be called by users (instead, use object Solver.check).
    * It is only public because it serves as a callback for Rule's.
    *
    * pre: context is covered
    *
    * post: if result, typing judgment is covered
    */
   override def inferType(tm: Term, covered: Boolean = false)(implicit stack: Stack, history: History): Option[Term] = {
     log("inference: " + presentObj(tm) + " : ?")
     history += "inferring type of " + presentObj(tm)
     // return previously inferred type, if any (previously unsolved variables are substituted)
     InferredType.get(tm) match {
        case Some((bp,tmI)) if getCurrentBranch.descendsFrom(bp) =>
           return Some(tmI ^^ solution.toPartialSubstitution)
        case _ =>
     }
     val res = logAndHistoryGroup {
        log("in context: " + presentObj(stack.context))
        val resFoundInd = tm match {
          //foundation-independent cases
          case Free(con,t) =>
            inferType(t, covered)(stack++con, history) map {tp => Free(con, tp)}
          case Unknown(u,args) =>
            getVar(u).tp map {tp => OMA(tp, args)}
          case OMV(x) =>
             history += "lookup in context"
             val vd = getVar(x)
             vd.tp orElse {
                vd.df flatMap {df =>
                  val con = stack.context.before(x)
                  inferType(df, true)(Stack(con), history + "inferring type of definiens")
                }
             }
          case OMS(p) =>
             history += "lookup in theory"
             getType(p) orElse {
               getDef(p) match {
                 case None => None
                 case Some(d) => inferType(d) // expand defined constant
               }
             }
          // note that OML's do not have a generally-defined type
          case l: OMLIT =>
             history += "lookup in literal"
             // structurally well-formed literals carry their type
             Some(l.rt.synType) // no need to use InferredType.put on literals
          case l : UnknownOMLIT =>
            if (!covered) {
              // TODO check literal
            }
            Some(l.synType)
          case OMMOD(p) =>
             // types of theories and views are formed using meta-level operators
             history += "lookup in library"
             getModule(p) match {
                case Some(t: Theory) => Some(TheoryType(t.parameters))
                case Some(v: View)   => Some(MorphType(v.from,v.to))
                case _ =>
                   error("not found")
                   None
             }
          case _ =>
             None
        }
        //foundation-dependent cases if necessary
        //syntax-driven type inference
        //rules may trigger backtracking, in which case the next rule is tried until one yields a result
        resFoundInd orElse {
           var activerules = inferenceRules
           var tmS = tm
           var tp: Option[Term] = None
           var tryAgain = true  // normally, we run the loop only once; but if a rule triggers Backtrack, we try again with a different rule
           while (tryAgain) {
              tryAgain = false
              val (tmp, ruleOpt) = safeSimplifyUntilRuleApplicable(tmS, activerules)
              tmS = tmp
              ruleOpt match {
                 case Some(rule) =>
                    history += ("applying inference rule " + rule.toString)
                    try {
                      tp = rule(this)(tmS, covered)
                    } catch {
                       case t : MaytriggerBacktrack#Backtrack =>
                          history += t.getMessage
                          activerules = dropJust(activerules, rule)
                          tryAgain = true
                    }
                 case None =>
                    if (tmS hashneq tm) {
                      history += "no applicable rule, trying again with simplified term"
                      tp = inferType(tmS, covered)
                    } else {
                      history += "no applicable rule, giving up"
                    }
              }
           }
           tp
        }
     }
     log("inferred: " + presentObj(tm) + " : " + res.map(presentObj).getOrElse("failed"))
     history += "inferred: " + presentObj(tm) + " : " + res.map(presentObj).getOrElse("failed")
     //remember inferred type
     if (!isDryRun) {
       res foreach {r => InferredType.put(tm, (getCurrentBranch,r))}
     }
     res map {r => substituteSolution(r)} // this used to call simplify (without defnition expansion)
   }

  /**
    * performs a type inference and calls a continuation function on the inferred type
    *
    * If type inference is not successful, this is delayed.
    */
  def inferTypeAndThen(tm: Term)(stack: Stack, history: History)(cont: Term => Boolean): Boolean = {
      implicit val (s,h) = (stack, history)
      inferType(tm) match {
         case Some(tp) =>
            cont(tp)
         case None =>
            val bi = new BranchInfo(history + "(inference delayed)", getCurrentBranch)
            addConstraint(new DelayedInference(stack, bi, tm, cont))
            true
      }
   }

   /**
    * tries to find a unique term of a given type by applying [[TypeBaseSolutionRule]]s
    *
    * @param tp the type
    * @param stack its Context
    * @return the found term, if search succeeded
    *
    * pre: context and tp are covered
    * post: if result, typing judgment is covered
    */
   def findUniqueInhabitant(tp: Term, covered: Boolean = false)(implicit stack: Stack, history: History): Option[Term] = {
      typebasedsolutionRules.foreach {rule =>
        if (rule.applicable(tp)) {
          history += "calling type-based solution rule " + rule.getClass
          val res = rule.solve(this)(tp)
          res foreach {r => return res}
        }
      }
      None
   }

   /**
    * checks a judgement
    *
    * This is the method that should be used to recurse into a hypothesis.
    * It handles logging and error reporting and delegates to specific methods based on the judgement.
    *
    * The apply method is similar but additionally simplifies the judgment.
    *
    * @param j the judgement
    * @return like apply
    */
   def check(j: Judgement)(implicit history: History): Boolean = {
      if (checkingUnit.isKilled) {
        return error("checking was cancelled by external signal")
      }
      if (report.groups.contains("debug")) {
        val i = {Solver.checkId+=1; Solver.checkId}
        history += "check id " + i 
      }
      JudgementStore.getOrElseUpdate(j) {
        history += j
        log("checking: " + j.presentSucceedent + "\n  in context: " + j.presentAntecedent)
        logAndHistoryGroup {
          j match {
            case j: Typing   => checkTyping(j)
            case j: Subtyping => checkSubtyping(j)
            case j: Equality => checkEquality(j)
            case j: Universe => checkUniverse(j)
            case j: Inhabitable => checkInhabitable(j)
            case j: Inhabited => checkInhabited(j)
            case j: IsContext => checkContext(j)
            case j: EqualityContext => checkEqualityContext(j)
          }
        }
     }
   }

   /** caches the results of judgements to avoid duplicating work */
   // judgements are not cached if we are in a dry run to make sure they are run again later to solve unknowns
   private object JudgementStore {
     private val store = new scala.collection.mutable.HashMap[Judgement,Boolean]
     def get(j : Judgement) = store.get(j)
     /** lookup up result for j; if not known, run f to define it */
     def getOrElseUpdate(j : Judgement)(f: => Boolean): Boolean = {
       store.find {case (k,_) => k implies j} match {
         case Some((_,r)) =>
           r
         case None =>
           val r = f
           if (!isDryRun) {
             store(j) = r
           }
           r
       }
    }
  }

   /** proves a Universe Judgment
    *
    * @param j the judgment
    * @return true if succeeded or delayed
    *
    * pre: context is covered
    *
    * post: j is covered
    */
   // TODO this should be removed; instead LF should use low-priority InhabitableRules that apply to any term
   private def checkUniverse(j : Universe)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     safeSimplifyUntilRuleApplicable(j.univ, universeRules) match {
        case (uS, Some(rule)) =>
          history += "Applying UniverseRule " + rule.toString
          rule(this)(uS)
        case (uS, None) => delay(Universe(stack, uS))
     }
   }

   /** proves an Inhabitable Judgment
 *
    * @param j the judgment
    * @return true if succeeded or delayed
    *
    * pre: context is covered
    *
    * post: j is covered
    */
   private def checkInhabitable(j : Inhabitable)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     safeSimplifyUntilRuleApplicable(j.wfo, inhabitableRules) match {
        case (uS, Some(rule)) =>
          history += "Applying InhabitableRule " + rule.toString
          rule(this)(uS)
        case (uS, None) =>
           history += "inferring universe"
           inferType(j.wfo)(stack, history) match {
             case None =>
                delay(Inhabitable(stack, j.wfo))
             case Some(univ) =>
                check(Universe(stack, univ))
          }
     }
   }

  /** proves a Typing Judgement by bidirectional type checking
    *
    * In particular, uses TypingRule's to branch and InferenceRule's followed by Equality check as fallback.
    *
    * pre: context and type are covered
    *
    * post: typing judgment is covered
    */
   private def checkTyping(j: Typing)(implicit history: History) : Boolean = {
     val tm = j.tm
     val tp = j.tp
     implicit val stack = j.stack
     // try to solve the type of an unknown
     val solved = solveTyping(j)
     if (solved) return solved
     def checkByInference(tpS: Term): Boolean = {
        log("Checking by inference")
         val hisbranch = history.branch
         inferType(tm)(stack, hisbranch) match {
            case Some(itp) =>
               check(Subtyping(stack, itp, tpS))(history + ("inferred type must conform to expected type; the term is: " + presentObj(tm)))
            case None =>
               delay(Typing(stack, tm, tpS, j.tpSymb))(hisbranch + "type inference failed")
         }
     }
     // the foundation-independent cases
     tp match {
       case Free(con,t) =>
         return checkTyping(Typing(stack++con, OMA(tm, con.map(_.toTerm)), t))
       case _ =>
     }
     tm match {
       //TODO if the type has a typing rule and the term is OMV/OMS or if Infered.get is defined, we have two conflicting strategies
       //it's unclear which one works better
       case OMV(x) =>
         val vd = getVar(x)
         vd.tp match {
           case None =>
              // x cannot be an unknown because solveTyping would have applied
              if (solution.isDeclared(x))
                solveType(x, tp) //TODO: check for occurrences of bound variables?
              else {
                vd.df match {
                  case None =>
                    error("untyped, undefined variable type-checks against nothing: " + x)
                  case Some(d) =>
                    check(Typing(stack, d, tp))
                }
              }
           case Some(t) =>
               check(Subtyping(stack, t, tp))
         }
       case OMS(p) =>
         getType(p) match {
           case None => getDef(p) match {
             case None =>
                checkByInference(tp)
                //error("untyped, undefined constant type-checks against nothing: " + p.toString)
             case Some(d) => check(Typing(stack, d, tp, j.tpSymb)) // expand defined constant
           }
           case Some(t) => check(Subtyping(stack, t, tp))
         }
       // note that OMLs do not have a generally-defined type
       case l: OMLIT => check(Subtyping(stack, l.rt.synType, tp))
       // the foundation-dependent cases
       // bidirectional type checking: first try to apply a typing rule (i.e., use the type early on), if that fails, infer the type and check equality
       case tm =>
         log("finding applicable typing rule")
         logAndHistoryGroup{safeSimplifyUntilRuleApplicable(tp,typingRules)} match {
           case (tpS, Some(rule)) =>
             try {
                log("Applying TypingRule " + rule.toString)
                history += "Applying TypingRule " + rule.toString
                rule(this)(tm, tpS)
             } catch {
               case TypingRule.SwitchToInference =>
                 checkByInference(tpS)
               case rule.DelayJudgment(msg) =>
                 log("Delaying judgment " + j)
                 delay(Typing(stack, tm, tpS, j.tpSymb))(history + msg)
             }
           case (tpS, None) =>
              // either this is an atomic type, or no typing rule is known
              checkByInference(tpS)
         }

     }
   }

   /** proves an Equality Judgment by recursively applying in particular EqualityRule's.
    *
    * @param j the judgement
    * @return false if the Judgment is definitely not provable; true if it has been proved or delayed
    *
    * pre: context, terms, and (if given) type are covered; if type is given, both typing judgments are covered
    *
    * post: equality is covered
    */
   private def checkEquality(j: Equality)(implicit history: History): Boolean = {
      val tm1 = j.tm1
      val tm2 = j.tm2
      val tpOpt = j.tpOpt
      implicit val stack = j.stack
      val tm1S = simplify(tm1)
      val tm2S = simplify(tm2)
      // this is a bit awkward, but we need to marks stable terms as such so that rules can use the information 
      safeSimplifyOne(tm1S)
      safeSimplifyOne(tm2S)
      // 1) foundation-independent cases, e.g., identical terms, solving unknowns
      (tm1S, tm2S) match {
         case (l1: OMLIT, l2: OMLIT) =>
           if (l1.value != l2.value)
             return error(s"$l1 and $l2 are inequal literals")
           else {
             // TODO return true if a common value is a literal of the two distinct syntactic types
           }
         case (OML(n1, tp1, df1, _, f1), OML(n2, tp2, df2, _, f2)) =>
            if (n1 != n2)
              return error("OML's have different names, so cannot be equal")
            if (f1 != f2)
              return error("OML's have different features, so cannot be equal")
            return List((tp1,tp2),(df1,df2)) forall {
              case (None,None) => true
              case (Some(x1),Some(x2)) => checkEquality(Equality(j.stack, x1, x2, None))(history + "checking component of OML")
              case _ => error("OML's have different structure, so cannot be equal")
            }
         // comes up when solutions are expanded during simplification or when checking multiple solutions for consistency
         case (Free(c1,t1), Free(c2,t2)) =>
           val eqCon = check(EqualityContext(j.stack, c1, c2, true))(history + "checking equality of free variables")
           val subs = (c2 alpha c1).getOrElse(return false)
           return eqCon && check(Equality(j.stack++c1, t1, t2 ^? subs, None))
         case _ =>
           if (tm1S hasheq tm2S) return true
           // there is not much else that is definitely inequal: e.g., even two distinct variables could be equal if their type is term-irrelevant
      }
      // solve an unknown
      val jS = j.copy(tm1 = tm1S, tm2 = tm2S)
      val solved = solveEquality(jS) || solveEquality(jS.swap)
      if (solved) return true

      // 2) find a TermBasedEqualityRule
      termBasedEqualityRules.filter(r => r.applicable(tm1S,tm2S)) foreach {rule =>
         // we apply the first applicable rule
         history += "trying term-based equality rule " + rule.toString
         val contOpt = rule(this)(tm1S,tm2S,tpOpt)
         contOpt match {
           case Some(cont) =>
             return cont.apply
           case None =>
             history += "rule not applicable"
         }
      }

      // 3) find a TypeBasedEqualityRule based on the head of the type
      // first infer the type if it has not been given in tpOpt
      val tp = tpOpt match {
        case Some(tp) => tp
        case None =>
           // TODO switching to unsafe type inference should only make things faster, but actually causes failures
           val itp = inferType(tm1S)(stack, history + "inferring omitted type") orElse
                     inferType(tm2S)(stack, history + "inferring omitted type")
           itp.getOrElse(return delay(Equality(stack, tm1S, tm2S, None)))
     }
      // try to simplify the type until an equality rule is applicable
      val (tpS, rOpt) = safeSimplifyUntil(tp) {t =>
        typeBasedEqualityRules find {r =>
          r.applicable(t) && (r.applicableToTerm(tm1S) || r.applicableToTerm(tm2S))
        }
      }
      rOpt match {
         case Some(rule) =>
            history += "applying type-based equality rule " + rule.toString
            val res = try {
              rule(this)(tm1S, tm2S, tpS)
            } catch {
              case rule.DelayJudgment(msg) =>
                history += msg
                return delay(Equality(stack, tm1S, tm2S, Some(tpS)))
            }
            res match {
               case Some(b) => b
               case None =>
                  checkEqualityTermBased(tm1S, tm2S)(stack, history, tp)
            }
         case None =>
            // this is either a base type or an equality rule is missing
            history += " no applicable type-based rule, trying term-based equality"
            checkEqualityTermBased(tm1S, tm2S)(stack, history, tp)
      }
   }

   /**
    * checks equality t1 = t2 by using congruence reasoning
    *
    * @return true if equal
    */
   private def checkEqualityTermBased(t1: Term, t2: Term)(implicit stack : Stack, history: History, tp: Term) : Boolean = {
     log("equality (trying congruence): " + t1 + " = " + t2)
     val List(t1S,t2S) = List(t1, t2) map headNormalize
     if (t1S hasheq t2S) {
       true
     } else if (!Stability.is(t1S) || !Stability.is(t2S)) {
       history += "terms not stable"
       delay(Equality(stack,t1S,t2S,Some(tp)))
     } else {
       history += "both terms are stable"
       def differentShape = error("terms have different shape")
       (t1S,t2S) match {
         case (ComplexTerm(c1,subs1,cont1,args1), ComplexTerm(c2,subs2,cont2,args2)) =>
            if (c1 != c2 || subs1.length != subs2.length || cont1.length != cont2.length || args1.length != args2.length) {
              differentShape
            } else {
              val subeq = (subs1 zip subs2) forall {case (s1,s2) =>
                checkEquality(Equality(stack, s1.target, s2.target,None))(history + "comparing arguments")
              }
              if (!subeq) return subeq
              val conteq = checkEqualityContext(EqualityContext(stack, cont1, cont2, true))(history + "comparing bindings")
              if (!conteq) return conteq
              val sub2to1 = (cont2 alpha cont1).get // defined due to guard above 
              var i = 0
              val argseq = (args1 zip args2) map {case (a1,a2) =>
                i += 1
                checkEquality(Equality(stack++cont1, a1, a2 ^? sub2to1, None))(history + ("comparing argument " + i))
              }
              argseq.forall(_ == true) // comparing all arguments is inefficient if an early argument has an error, but may help make sense of the error
            }
         case _ =>
           differentShape
       }
     }
   }

       /* old version with dubious congruence reasoning
       // if we failed to prove equality, we have multiple options:
       (changed, t2Final) match {
          // t1 expanded, t2 cannot be expanded anymore --> keep expanding t1
          case (true, true)      => checkEqualityTermBased(t1S::terms1, terms2, true, flipped)
          // t1 expanded, t2 can still be expanded --> continue expanding t2
          case (true, false)     => checkEqualityTermBased(terms2, t1S::terms1, false, ! flipped)
          // t1 cannot be expanded anymore but t2 can ---> continue expanding t2
          case (false, false)    => checkEqualityTermBased(terms2, terms1, true, ! flipped)
          // neither term can be expanded --> try congruence rule as last resort
          case (false, true)     =>
             // if necessary, we undo the flipping of t1 and t2
             val s1 = if (flipped) terms2.head else t1S
             val s2 = if (flipped) t1S else terms2.head
             val s12Vars = s1.freeVars ::: s2.freeVars
             val minCont = stack.context.minimalSubContext(s12Vars)
             val j = Equality(Stack(minCont), s1, s2, Some(tp))
             val unknownsLeft = j.freeVars.toList intersect solution.map(_.name)
             // only incomplete congruence reasoning is left, we delay it if there is any hope to avoid it
             val nextStep = () => checkEqualityCongruence(TorsoForm.fromHeadForm(s1,unknownsLeft), TorsoForm.fromHeadForm(s2,unknownsLeft), unknownsLeft.nonEmpty)
             if (unknownsLeft.nonEmpty) {
               delay(j, Some(nextStep))
             } else {
               nextStep()
             }
       }*/

  /* ********************** end of equality checking methods ***************************/

  /** proves a subtyping judgement
    *
    * MMT does not natively implement any subtyping and delegates to equality checking by default.
    * However, this behavior can customized by providing [[SubtypingRule]]s.
    *
    * pre: context and both types are covered
    * post: subtyping judgment is covered
    */
   private def checkSubtyping(j: Subtyping)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     val tp1 = headNormalize(j.tp1)
     val tp2 = headNormalize(j.tp2)
     // optimization for reflexivity
     if (tp1 hasheq tp2) return true
     // old attempt, catches more reflexivity, but is too aggressive
     // val congruenceLeafs = makeCongClos.apply(Equality(j.stack,j.tp1,j.tp2,None))
     // val obviouslyEqual = tryToCheckWithoutDelay(congruenceLeafs :_*)
     // if (obviouslyEqual contains true) {
     //   return true
     // }
     val r = solveSubtyping(j)
     if (r) return true
     // foundation-independent cases
     (tp1, tp2) match {
        case (Free(c1,t1), Free(c2,t2)) =>
          val eqCon = check(EqualityContext(j.stack, c1, c2, true))(history + "checking equality of free variables")
          val subs = (c2 alpha c1).getOrElse(return false)
          return eqCon && check(Subtyping(j.stack++c1, t1, t2 ^? subs))
        case _ =>
          //TODO eventually we need cases to handle sub/supertype bounds of constants and variables
     }
     // otherwise, apply a subtyping rule
     history += "not obviously equal, trying subtyping"
     subtypingRules foreach {r =>
       history += "trying " + r.toString
       if (r.applicable(tp1,tp2)) {
          history += "applying subtyping rule " + r.toString + " to " + presentObj(tp1) + " <: " + presentObj(tp2)
          try {
            r(this)(tp1,tp2) foreach {r => return r}
          } catch {case e: MaytriggerBacktrack#Backtrack =>
             history += e.getMessage
          }
       }
     }
     // otherwise, we default to checking equality
     // in the absence of subtyping rules, this is the only option anyway
     history += "no applicable subtyping rules, falling back to checking equality"
     check(Equality(j.stack, tp1, tp2, None))
  }

   /* ********************** simplification ******************************************************/

   /**
    * unsafe via ObjectSimplifier
    * this subsumes substituting for solved unknowns before simplifier expands defined variables
    */
   def simplify(t : Obj)(implicit stack: Stack, history: History): t.ThisType = {
      val solutionNoDefs = solution map {vd => vd.copy(df = None)} // avoid expanding solved unknowns
      val tS = controller.simplifier(t, constantContext ++ solutionNoDefs ++ stack.context, rules, expDef = false)
      if (tS != t)
        history += ("simplified: " + presentObj(t) + " ~~> " + presentObj(tS))
      if (Stability.is(t))
        Stability.set(tS)
      tS
   }

   /** simplifies one step overall */
   private def safeSimplifyOne(tm: Term)(implicit stack: Stack, history: History): Term = {
     if (checkingUnit.isKilled) {
       return tm
     }
     val tmS = tm match {
       case OMS(p) =>
         val d = getDef(p)
         d match {
           case Some(tD) =>
             tD
           case None =>
             // TODO apply abbrev rules?
             Stability.set(tm) // undefined constants are stable 
             tm             
         }
       case OMV(n) =>
         val vd = outerContext.getO(n) getOrElse stack.context(n)
         if (solution.isDeclared(n)) {
           // unknowns are unstable, even if solved, their definiens should be expanded via SubstituteUnknowns
           tm
         } else vd.df match {
           case Some(tD) =>
             tD
           case _ =>
             Stability.set(tm) 
             tm
         }
       case OMAorAny(Free(cont,bd), args) if cont.length == args.length =>
           // should be done in SubstituteUnknowns, but sometimes the definitions of unknowns get expanded anyway
           val sub = (cont / args).get // defined due to guard
           bd ^? sub
       case t: OMLITTrait =>
         Stability.set(t) // literals are always stable
         t
       case ComplexTerm(op,subs,con,args) =>
          // use first applicable rule
          var simp: CannotSimplify = Simplifiability.NoRecurse
          computationRules foreach {rule =>
            if (rule.applicable(tm)) {
              val ret = rule(this)(tm, false)(stack,history)
              ret match {
                case Simplify(tmS) =>
                  log("applied computation rule " + rule.toString + " to " + presentObj(tm))
                  history += "applied computation rule " + rule.toString
                  history += ("simplified: " + presentObj(tm) + " ~~> " + presentObj(tmS))
                  return tmS
                case cannot: CannotSimplify =>
                  simp = simp join cannot
              }
            }
          }
          // no applicable rule: check stability
          val recursePositions = simp match {
            case RecurseOnly(p) => p.distinct
            case Recurse => 1 to tm.subobjects.length
          }
          // invariant: tm simplifies to result
          var subobjsLeft: List[Obj] = subs ::: con ::: args
          var subobjsNew: List[Obj] = Nil
          var simplified: Boolean = false // true if tm is not identical to result anymore
          var stable: Boolean = true // true if the subterms in all recurse positions are stable
          def result = ComplexTerm(op, subobjsNew.reverse ::: subobjsLeft)
          // we go through all subobjects and try to simplify one of them
          while (subobjsLeft.nonEmpty) {
            val o = subobjsLeft.head
            subobjsLeft = subobjsLeft.tail
            val i = subobjsNew.length + 1 // position of o
            val h = history + ("recursing into subobject " + i) 
            val sNew = if (!simplified && !recursePositions.contains(i)) {
              o // only recurse if this is one of the recurse positions and no previous subobjects has changed 
            } else {
              val oN = o match {
                case s: Sub =>
                  val sN = s.map(t => safeSimplifyOne(t)(stack,h))
                  if (Stability.is(sN.target))
                    Stability.set(sN)
                  sN
                case vd: VarDecl =>
                  val vdN = vd.map {t => safeSimplifyOne(t)(stack ++ con.take(i-subs.length), h)}
                  if ((vdN.tp.toList ::: vdN.df.toList).forall {t => Stability.is(t)})
                    Stability.set(vdN)
                  vdN
                case t: Term =>
                  safeSimplifyOne(t)(stack ++ con, h) 
              }
              if (o hasheq oN) {
                stable &&= Stability.is(oN)
                o // if no change, retain the old pointer
              } else {
                simplified = true
                stable = false // redundant
                oN
              }
            }
            subobjsNew ::= sNew
          }
          if (simplified) {
            result
          } else {
            // mark original term as head-stable if all relevant subterms are 
            if (stable)
              Stability.set(tm)
            tm
          }
       case t => t
     }
     if (tm hashneq tmS) {
       log("simplified: " + presentObj(tm) + " ~~> " + presentObj(tmS))
       history += ("simplified: " + presentObj(tm) + " ~~> " + presentObj(tmS))
     }
     tmS
   }


   /** applies [[ComputationRule]]s expands definitions until a condition is satisfied;
    *  A typical case is transformation into weak head normal form.
 *
    *  @param tm the term to simplify (It may be simple already.)
    *  @param simple a term is considered simple if this function returns a non-None result
    *  @param stack the context of tm
    *  @return (tmS, Some(a)) if tmS is simple and simple(tm)=tmS; (tmS, None) if tmS is not simple but no further simplification rules are applicable
    */
   override def safeSimplifyUntil[A](tm: Term)(simple: Term => Option[A])(implicit stack: Stack, history: History): (Term,Option[A]) = {
      simple(tm) match {
         case Some(a) => (tm,Some(a))
         case None =>
            val tmS = safeSimplifyOne(tm)
            if (tmS hashneq tm)
               safeSimplifyUntil(tmS)(simple)
            else
               (tm, None)
      }
   }
   /** like the other method but simplifies two terms in parallel */
   @scala.annotation.tailrec
   final def safeSimplifyUntil[A](tm1: Term, tm2: Term)(simple: (Term,Term) => Option[A])
                           (implicit stack: Stack, history: History): (Term,Term,Option[A]) = {
      simple(tm1,tm2) match {
         case Some(a) => (tm1,tm2,Some(a))
         case None =>
            val tm1S = safeSimplifyOne(tm1)
            val tm2S = safeSimplifyOne(tm2)
            if ((tm1S hashneq tm1) || (tm2S hashneq tm2))
                  safeSimplifyUntil(tm1S, tm2S)(simple)
               else
                  (tm1S,tm2S,None)
      }
   }

   /** special case where we simplify until an applicable rule is found
    *
    *  @param tm the term to simplify
    *  @param hs the set of rules one of which is to be used
    *  @param stack the context of tm
    *  @param history the current history
    *  @return (tmS, Some(r)) where tmS = tm and r from hs is applicable to tmS; (tmS, None) if tm = tmS and no further simplification rules are applicable
    */
   private def safeSimplifyUntilRuleApplicable[R <: CheckingRule](tm: Term, hs: Iterable[R])(implicit stack: Stack, history: History): (Term,Option[R]) =
      safeSimplifyUntil[R](tm)(t => t.head flatMap {h => hs.find(_.heads contains h)})

      
  /** alternates safeSimplifyOne and simplify until head-normal; does not necessarily terminate */
  private def headNormalize(t: Term)(implicit stack: Stack, history: History): Term = {
    if (Stability.is(t))
      return t
    val tS = simplify(safeSimplifyOne(t))
    if (tS hasheq t)
      tS
    else {            
      headNormalize(tS)
    }
  }
      
  /* ********************** methods for contexts ***************************/

  /** checks contexts */
   private def checkContext(j: IsContext)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     val con = simplify(j.wfo)
     con.declsInContext.forall {case (pre,vd) =>
       val newCon = stack ++ pre
       (vd.tp,vd.df) match {
         case (None,None) => true
         case (Some(tp),Some(df)) => check(Typing(newCon, df, tp, None))(history + "defined declaration must be well-typed")
         case (Some(tp), None) => check(Inhabitable(newCon, tp))(history + "type in contexts must be inhabitable")
         case (None, Some(df)) => inferTypeAndThen(df)(newCon, history + "definiens in context must be well-formed") {_ => true}
       }
     }
   }

  /** checks equality of contexts (up to either alpha-renaming or reordering) */
   private def checkEqualityContext(j: EqualityContext)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     if (j.context1.length != j.context2.length) {
       return error("contexts do not have the same length")
     }
     val c1 = simplify(j.context1)
     val c2 = simplify(j.context2)
     var sub = Substitution.empty
     def checkOptTerm(c: Context, tO1: Option[Term], tO2: Option[Term]) = (tO1,tO2) match {
       case (None,None) =>
         true
       case (Some(t1),Some(t2)) =>
         check(Equality(j.stack ++ c, t1, t2 ^? sub, None))(history + "component-wise equality of contexts")
       case _ =>
         error("contexts do not have the same shape")
     }
     val c2R = if (j.uptoAlpha) {
       c2
     } else {
       // c2New is the reordering of c2 such that corresponding variables have the same name
       // invariant: c2New ++ c2Rest == c2  up to reordering
       var c2Rest = c2
       var c2New = Context.empty
       c1.foreach {vd1 =>
         val (skip, vd2rest) = c2Rest.span(_.name != vd1.name)
         if (vd2rest.isEmpty) {
           return error("contexts do not declare the same names")
         }
         val vd2 :: rest = vd2rest
         // c2Rest == skip ++ vd2 ++ rest  and  vd1.name == vd2.name
         // TODO vd2 may refer to skipped variables if they have a definition, in which case we should substitute the definition
         if (! utils.disjoint(skip.map(_.name), vd2.freeVars)) {
           return error("cannot reorder contexts to make them declare the same variables names equal")
         }
         c2Rest = skip ::: rest
         c2New = c2New ++ vd2
       }
       c2New
     }
     (c1.declsInContext.toList zip c2.zipWithIndex) forall {case ((c, vd1), (vd2,i)) =>
       if (j.uptoAlpha) {
         sub = sub ++ (vd2.name -> OMV(vd1.name))
       } else {
         //if (vd1.name != vd2.name) return error("contexts do not declare the same variables") else // redundant now
       }
       checkOptTerm(c, vd1.tp, vd2.tp) && checkOptTerm(c, vd1.df, vd2.df)
     }
   }

  /* ************************************************************ */

   /** tries to solve an unknown occurring in tm1 in terms of tm2
    *
    *  returns true if the unknowns were solved and the equality proved
    *  otherwise, returns false without state change (returning false here does not signal that the equality is disproved)
    */
   private def solveEquality(j: Equality)(implicit history: History): Boolean = {
      implicit val stack = j.stack
      j.tm1 match {
         //foundation-independent case: direct solution of an unknown variable
        case Unknown(m, as) =>
          val vars = isDistinctVarList(as).getOrElse(return false)
          // solve m with j.tm2
          val mSol = Free(vars, j.tm2)
          moveToRight(m)
          val remainingFreeVars = notAllowedInSolution(m, mSol)
          if (remainingFreeVars.isEmpty) {
             // we can solve m already, the remaining unknowns in tm2 can be filled in later
             val res = solve(m, mSol)
             j.tpOpt foreach {case tp => solveType(m, Free(vars, tp))}
             res
          } else {
             history += ("free variables remain: " + remainingFreeVars.mkString(", "))
             history += presentObj(solution)
             delay(j)
             // meta-variable solution has free variable --> type error unless free variable disappears later
             // better: we can rearrange the unknown variables (which ultimately may not depend on each other anyway)
             // note: tm2 should be simplified - that may make a free variable disappear
          }
         //apply a foundation-dependent solving rule selected by tm1
         case _ => Solver.findSolvableVariable(solutionRules, solution, j.tm1) match {
            case Some((rs, m)) =>
              rs.foreach {sr =>
                sr(j) match {
                  case Some((j2,msg)) =>
                    history += "Using solution rule " + rs.head.toString
                    return solveEquality(j2)((history + msg).branch)
                  case _ =>
                }
              }
              false
            case _ => false
         }
      }
   }

   def solveTyping(j: Typing)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     val tm = j.tm
     val tp = j.tp
     j.tm match {
         //foundation-independent case: direct solution of an unknown variable
         case Unknown(m, args) =>
            val vars = isDistinctVarList(args).getOrElse(return false)
            val mTp = Free(vars, tp)
            moveToRight(m)
            val remainingFreeVars = notAllowedInSolution(m, mTp)
            if (remainingFreeVars.isEmpty) {
               // we can solve m already, the remaining unknowns in tm2 can be filled in later
               val res = solveType(m, mTp)
               res
            } else {
               history += ("bound variables or unknowns remain in potential solution: " + remainingFreeVars.mkString(", "))
               delay(j)
               // meta-variable solution has free variable --> type error unless free variable disappears later
               // better: we can rearrange the unknown variables (which ultimately may not depend on each other anyway)
               // note: tm2 should be simplified - that may make a free variable disappear
            }
         //apply a foundation-dependent solving rule selected by the head of tm1
         case _ => Solver.findSolvableVariable(typeSolutionRules, solution, tm) match {
            case Some((rs, m)) =>
              rs.foreach {sr =>
                sr(j) match {
                  case Some((j2,msg)) =>
                    history += "Using solution rule " + rs.head.toString
                    return solveTyping(j2)((history + msg).branch)
                  case _ =>
                }
              }
              false
            case _ => false
         }
      }
   }
   
   def solveSubtyping(j: Subtyping)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     val (unk, bound, below) = (j.tp1,j.tp2) match {
       case (u@Unknown(_), b) => (u,b,true)
       case (b, u@Unknown(_)) => (u,b,false)
       case _ => return false
     }
     unk match {
         //foundation-independent case: direct solution of an unknown variable
         case Unknown(m, args) =>
            val vars = isDistinctVarList(args).getOrElse(return false)
            val mBd = Free(vars, bound)
            moveToRight(m)
            val remainingFreeVars = notAllowedInSolution(m, mBd)
            if (remainingFreeVars.isEmpty) {
              // check if the bound is subsumed by an existing one
              val redundant = bounds(m) exists {case TypeBound(tp, b) =>
                if (b == below) {
                  val subsumption = subOrSuper(below)(Stack.empty, tp, mBd) 
                  val r = tryToCheckWithoutEffect(subsumption)
                  r contains true
                } else
                  false
              }
              if (redundant) return true
              // otherwise, add the new bound
              val res = solveBound(m, mBd, below)
              res
            } else {
               history += ("bound variables or unknowns remain in potential solution: " + remainingFreeVars.mkString(", "))
               delay(j)
            }
         //TODO apply a foundation-dependent solving rule selected by the head of tp1 or tp2
         case _ => false
      }
   }

   /**
    * proves an inhabitation judgment by theorem proving
    *
    * pre: context and type are covered
    *
    * post: inhabitation judgment is covered
    */
   private def checkInhabited(j : Inhabited)(implicit history: History): Boolean = {
      val tp = simplify(substituteSolution(j.tp))(j.stack, history)
      val res = prove(tp)(j.stack, history)
      if (res.isDefined)
         true
      else
         delay(j)
   }

   /** tries to prove a goal, @return the proof term if successful */
   def prove(conc: Term)(implicit stack: Stack, history: History): Option[Term] = {
      prove(constantContext ++ solution ++ stack.context, conc)
   }

   private def prove(context: Context, conc: Term)(implicit history: History): Option[Term] = {
      val msg = "proving " + presentObj(context) + " |- _ : " + presentObj(conc)
      log(msg)
      history += msg
      val pu = ProvingUnit(checkingUnit.component, context/*simplify(context)(Stack(context),history)*/, conc, logPrefix).diesWith(checkingUnit)
      controller.extman.get(classOf[Prover]) foreach {prover =>
         val (found, proof) = prover.apply(pu, rules, 3) //Set the timeout on the prover
         if (found) {
            val p = proof.get
            history += "proof: " + presentObj(p)
            log("proof: " + presentObj(p))
            return Some(p)
         } else {
            log("no proof found with prover " + prover.toString) //goal.present(0)(presentObj, None, None))
         }
      }
      log("giving up")
      None
   }


   /**
    * applies all ForwardSolutionRules of the given priority
    *
    * @param priority exactly the rules with this Priority are applied
    */
   //TODO call this method at appropriate times
   private def forwardRules(priority: ForwardSolutionRule.Priority)(implicit stack: Stack, history: History): Boolean = {
      val results = solution.zipWithIndex map {
         case (vd, i) => vd.tp match {
           case Some(tp) =>
              implicit val con : Context = solution.take(i)
              safeSimplifyUntilRuleApplicable(tp, forwardSolutionRules) match {
                 case (tpS, Some(rule)) if rule.priority == priority =>
                   history += "Applying ForwardSolutionRule " + rule.toString
                   rule(this)(vd)
                 case _ => false
              }
           case None => false
         }
         case _ => false
      }
      results.contains(true)
   }  
}