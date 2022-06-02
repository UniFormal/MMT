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
import scala.runtime.NonLocalReturnControl

/** the essential subalgorithms of a bidirectional type-checking,
 *  factored out from the [[Solver]] class, which holds all bureaucracy
 */ 
trait SolverAlgorithms {self: Solver =>
  import currentStateObj._
  private implicit val sa = new MemoizedSubstitutionApplier
  
   /** precomputes relevant rule sets, ordered by priority */
   private lazy val computationRules = rules.getOrdered(classOf[ComputationRule])
   private lazy val inferenceRules = rules.getOrdered(classOf[InferenceRule])
   private lazy val subtypingRules = rules.getOrdered(classOf[SubtypingRule])
   private lazy val typingRules = rules.getOrdered(classOf[TypingRule])
   private lazy val inferAndTypingRules = rules.getOrdered(classOf[InferenceAndTypingRule])
   private lazy val typebasedsolutionRules = rules.getOrdered(classOf[TypeBasedSolutionRule])
   private lazy val universeRules = rules.getOrdered(classOf[UniverseRule])
   private lazy val inhabitableRules = rules.getOrdered(classOf[InhabitableRule])
   private lazy val termBasedEqualityRules = rules.getOrdered(classOf[TermBasedEqualityRule])
   private lazy val typeBasedEqualityRules = rules.getOrdered(classOf[TypeBasedEqualityRule])
   private lazy val solutionRules = rules.getOrdered(classOf[ValueSolutionRule])
   private lazy val typeSolutionRules = rules.getOrdered(classOf[TypeSolutionRule])
   private lazy val typeCoercionRules = rules.getOrdered(classOf[TypeCoercionRule])
   private lazy val forwardSolutionRules = rules.getOrdered(classOf[ForwardSolutionRule])

  // ******************************************************************************************
  // *** algorithms for checking a judgment, returning booleans
  // ******************************************************************************************

  /**
    * general entry point for checking a judgement
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
        log("checking: " + j.presentSuccedent + "\n  in context: " + j.presentAntecedent)
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
     implicit val stack : Stack = j.stack
     history += "trying universe rules"
     val res = tryAllUnaryRules(universeRules,j.univ)
     res.getOrElse(delay(Universe(stack,j.univ)))
     /*
     safeSimplifyUntilRuleApplicable(j.univ, universeRules) match {
        case (uS, Some(rule)) =>
          history += "Applying UniverseRule " + rule.toString
          rule(this)(uS)
        case (uS, None) => delay(Universe(stack, uS))
     }
     */
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
     /*
     implicit val stack = j.stack
     safeSimplifyUntilRuleApplicable(j.wfo, inhabitableRules) match {
        case (uS, Some(rule)) =>
          log(rule.toString)
          history += "applying inhabitable rule: " + rule.toString
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
     */
     implicit val stack : Stack = j.stack
     history += "trying inhabitability rules"
     val res = tryAllUnaryRules(inhabitableRules,j.wfo)
     res.getOrElse {
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

     // continuation if we resort to infering the type of tm
     def checkByInference(tpS: Term, h: History): Boolean = {
       log("checking by inference")
       val hI = h + "inferring type"
       inferType(tm)(stack, hI) match {
         case Some(tmI) =>
           checkAfterInference(tmI, tpS, h)
         case None =>
           delay(Typing(stack, tm, tpS, j.tpSymb))(hI + "no applicable typing rule and type inference failed")
       }
     }

     // continuation if we have inferred the type of tm
     def checkAfterInference(tmI: Term, tpS: Term, h: History): Boolean = {
       check(Subtyping(stack, tmI, tpS))(h + ("inferred type must conform to expected type; the term is: " + presentObj(tm)))
     }
     // the foundation-independent cases
     tp match {
       case Free(con, t) =>
         //TODO this would require type-checking when reducing OMA(free(G,B),args)
         return checkTyping(Typing(stack ++ con, OMA(tm, con.map(_.toTerm)), t))
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
               checkByInference(tp, history)
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
         logAndHistoryGroup {
           history += "trying typing rules"
           //TODO something is weird here: only the first applicable rule is ever tried; is tryAllRules redundant?
           val tmT = tryAllRules(typingRules,tp) {(rule,tpS,h) =>
             try {
               rule(this)(tm,tpS)(stack,h)
             } catch {
               case TypingRule.SwitchToInference =>
                 return checkByInference(tpS, history + "switching to inference")
               case rule.DelayJudgment(msg) =>
                 return delay(Typing(stack, tm, tpS, j.tpSymb))(history + msg)
             }
           }
           history += "trying inference/typing rules"
           tmT.getOrElse {
             val res = tryAllRules(inferAndTypingRules,tm) { (rule,tmS,h) =>
               try {
                 rule(this, tmS, Some(tp), false)(stack,h) match {
                   case (_, Some(b)) => Some(b)
                   case (Some(tI), None) => return checkAfterInference(tI, tp, history)
                   case (None, None) => return checkByInference(tp, history)
                 }
               } catch {
                 case TypingRule.SwitchToInference =>
                   return checkByInference(tp, history)
               }
             }
             res.getOrElse(checkByInference(tp, history))
           }
         }
         /*
         logAndHistoryGroup {
           safeSimplifyUntilRuleApplicable(tp, typingRules)
         } match {
           case (tpS, Some(rule)) =>
             try {
               history += "applying typing rule " + rule.toString
               rule(this)(tm, tpS)
             } catch {
               case TypingRule.SwitchToInference =>
                 checkByInference(tpS)
               case rule.DelayJudgment(msg) =>
                 delay(Typing(stack, tm, tpS, j.tpSymb))(history + msg)
             }
           case (tpS, None) =>
             //  either this is an atomic type, or no typing rule is known
             // try finding a rule based on the term
             safeSimplifyUntilRuleApplicable(tm, inferAndTypingRules) match {
               case (tmS, Some(rule)) =>
                 try {
                   rule(this, tmS, Some(tpS), false) match {
                     case (_, Some(b)) => b
                     case (Some(tI), None) => checkAfterInference(tI, tpS)
                     case (None, None) => checkByInference(tpS)
                   }
                 } catch {
                   case TypingRule.SwitchToInference =>
                     checkByInference(tpS)
                 }
               case (_, None) =>
                 checkByInference(tpS)
             }
         }
         */
     }
   }

   /** proves an Equality Judgment by recursively applying in particular EqualityRule's.
    *
    * @param j the judgement
    * @return false if the Judgment is definitely not provable; true if it has been proved or delayed
    *
    * pre: context, terms, and (if given) type are covered; if type is given, both typing judgments are covered;
    *   even if type is not given, both terms have the same type
    *
    * post: equality is covered
    */
   private def checkEquality(j: Equality)(implicit history: History): Boolean = {
      val tm1 = j.tm1
      val tm2 = j.tm2
      val tpOpt = j.tpOpt
      implicit val stack = j.stack
      val tm1N = headNormalize(tm1)
      val tm2N = headNormalize(tm2)
      val tm1S = simplify(tm1N)
      val tm2S = simplify(tm2N)
      // 1) foundation-independent cases, e.g., identical terms, solving unknowns
      (tm1S, tm2S) match {
         case (l1: OMLIT, l2: OMLIT) =>
           if (l1.value != l2.value)
             return error(s"$l1 and $l2 are inequal literals")
           else {
             // precondition guarantees that arguments have the same type even if they have different semantic types (i.e., due to subtyping of semantic types)
             return true
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
      Solver.breakAfter(139)
      val solved = solveEquality(jS,Nil) || solveEquality(jS.swap,Nil)
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
          r.applicable(t) && (r.applicableToTerm(this, tm1S) || r.applicableToTerm(this, tm2S))
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
            history += " no applicable type-based rule, trying congruence"
            checkEqualityTermBased(tm1S, tm2S)(stack, history, tp)
      }
   }

  private def isDirectlySolvable(j: Equality)(implicit stack: Stack) = {
    j.tm1 match {
      case Unknown(_,args) => isDistinctVarList(args).isDefined
      case _ => false
    }
  }

   /**
    * checks equality t1 = t2 by using congruence reasoning
    *
    * @return true if equal
    */
   private def checkEqualityTermBased(t1S: Term, t2S: Term)(implicit stack : Stack, history: History, tp: Term) : Boolean = {
     log("equality (trying congruence): " + presentObj(t1S) + " = " + presentObj(t2S))
     if (t1S hasheq t2S) {
       true
     } else if (!stability.is(t1S) || !stability.is(t2S)) {
       history += "terms not stable"
       val j = Equality(stack,t1S,t2S,Some(tp))
       val equalities = CongruenceClosure(j)
       val suffices = if (equalities.exists(_.forall(e => isDirectlySolvable(e) || isDirectlySolvable(e.swap))))
         equalities else None
       delay(j, suffices)
     } else {
       history += "both terms are stable"
       def differentShape = error("terms have different shape")
       (t1S,t2S) match {
         case (ComplexTerm(c1,subs1,cont1,args1), ComplexTerm(c2,subs2,cont2,args2)) =>
            if (c1 != c2 || subs1.length != subs2.length || cont1.length != cont2.length || args1.length != args2.length) {
              differentShape
            } else {
              val subeq = (subs1 zip subs2) forall {case (s1,s2) =>
                check(Equality(stack, s1.target, s2.target,None))(history + "comparing arguments")
              }
              if (!subeq) return subeq
              val conteq = check(EqualityContext(stack, cont1, cont2, true))(history + "comparing bindings")
              if (!conteq) return conteq
              val sub2to1 = (cont2 alpha cont1).get // defined due to guard above 
              var i = 0
              val argseq = (args1 zip args2) map {case (a1,a2) =>
                i += 1
                check(Equality(stack++cont1, a1, a2 ^? sub2to1, None))(history + ("comparing argument " + i))
              }
              argseq.forall(_ == true) // comparing all arguments is inefficient if an early argument has an error, but may help make sense of the error
            }
         case _ =>
           differentShape
       }
     }
   }

  /* ********************** end of equality checking methods ***************************/

  private def coerceToType(tm: Term)(implicit stack: Stack, history: History): Term = {
    inferType(tm, true).flatMap {tp =>
      safeSimplifyUntilRuleApplicable(tp, typeCoercionRules) match {
        case (tpS, Some(rule)) => rule(tm, tpS)
        case _ => None
      }
    }.getOrElse(tm)
  }
   
  /** proves a subtyping judgement
    *
    * MMT does not natively implement any subtyping and delegates to equality checking by default.
    * However, this behavior can customized by providing [[SubtypingRule]]s.
    *
    * pre: context and both types are covered
    * post: subtyping judgment is covered
    */
   private def checkSubtyping(j: Subtyping)(implicit history: History): Boolean = {
     if (j.tp1 hasheq j.tp2) return true
     implicit val stack = j.stack
     val tp1L = coerceToType(j.tp1)
     val tp2L = coerceToType(j.tp2)
     val tp1 = headNormalize(tp1L)
     val tp2 = headNormalize(tp2L)
     // optimization for reflexivity
     if (tp1 hasheq tp2) return true
     val r = solveSubtyping(j.copy(tp1=tp1, tp2=tp2))
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
     history += "not obviously equal, trying subtyping rules"
     val res = tryAllRules(subtypingRules,tp1,tp2){(r,t1,t2) => r.applicable(t1,t2)} { (rule,tp1S,tp2S,h) =>
       try { rule(this)(tp1S,tp2S)(stack,h) } catch {
         case d : rule.DelayJudgment =>
           return delay(j)
       }
     }
     res.getOrElse {
       history += "falling back to checking equality"
       check(Equality(j.stack, tp1, tp2, None))
     }
     /*
     subtypingRules foreach {r =>
       if (r.applicable(tp1,tp2)) {
          history += "applying subtyping rule " + r.toString + " to " + presentObj(tp1) + " <: " + presentObj(tp2)
          try {
            r(this)(tp1,tp2) foreach {r => return r}
          } catch {case e: MaytriggerBacktrack#Backtrack =>
             history += e.getMessage
          }
       }
     }
     */
     // otherwise, we default to checking equality
     // in the absence of subtyping rules, this is the only option anyway
  }


  // ******************************************************************************************
  // *** algorithms for finding a term that makes a judgment true
  // ******************************************************************************************

  // *********************************************** type inference
   
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
        case Some(tmI)  =>
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
            getVar(u).tp map {tp => OMAorAny(tp, args)}
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
                 case None =>
                   history += "no type or definiens"
                   None
                 case Some(d) =>
                   history += "no type, inferring from definiens"
                   inferType(d) // expand defined constant
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
                   log("applying inference rule " + rule.toString)
                    history += ("applying inference rule " + rule.toString)
                    try {
                      tp = rule(this)(tmS, covered)
                      tp match {
                        case None =>
                          activerules = dropJust(activerules, rule)
                          tryAgain = true
                        case _ =>
                      }
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
       res foreach {r => InferredType.put(tm, r)}
     }
     res map {r => substituteSolution(r)} // this used to call simplify (without defnition expansion)
   }

  /**
    * performs a type inference and calls a continuation function on the inferred type
    *
    * If type inference is not successful, this is delayed.
    */
  def inferTypeAndThen(tm: Term, covered: Boolean = false)(stack: Stack, history: History)(cont: Term => Boolean): Boolean = {
      implicit val (s,h) = (stack, history)
      inferType(tm, covered) match {
         case Some(tp) =>
            cont(tp)
         case None =>

            addConstraint(new DelayedInference(stack, h , tm, cont))
            true
      }
   }

  // *********************************************** inhabitant search
  
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
          history += "calling type-based solution rule " + rule
          val res = rule.solve(this)(tp)
          res foreach {r => return res}
        }
      }
      None
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

   /** tries to prove a goal by finding a term of type conc
    *  If conc contains unknowns, this is unlikely to succeed unless an appropriate assumption is in the context.
    */
   def prove(conc: Term, allowUnknowns: Boolean = true)(implicit stack: Stack, history: History): Option[Term] = {
      val unknCont = if (allowUnknowns) solution else Context.empty
      prove(constantContext ++ unknCont ++ stack.context, conc)
   }

   private def prove(context: Context, conc: Term)(implicit history: History): Option[Term] = {
      val msg = "proving " + presentObj(context) + " |-- _  ::  " + presentObj(conc)
      history += msg
      val pu = ProvingUnit(checkingUnit.component, context, conc, logPrefix).diesWith(checkingUnit)
      controller.extman.get(classOf[AutomatedProver]) foreach {prover =>
         val (found, proof) = prover.apply(pu, rules, 3) //Set the timeout on the prover
         if (found) {
            val p = proof.getOrElse(UnknownTerm())
            history += "proof: " + presentObj(p)
            return Some(p)
         } else {
            history += "no proof found with prover " + prover.toString
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

  
  // *********************************************** simplification

  /**
    * unsafe via ObjectSimplifier
    * this subsumes substituting for solved unknowns before simplifier expands defined variables
    */
  private def simplify(t : Obj, expDef: Boolean, fullRec: Boolean)(implicit stack: Stack, history: History): t.ThisType = {
      val su = SimplificationUnit(constantContext ++ solution ++ stack.context, expDef, fullRec,Some(this)).diesWith(checkingUnit)
      // TODO even when called with expDef=false, the rule Beta may expand defined function symbols; it's unclear if this is desirable, especially when fullRec=true
      val tS = controller.simplifier(t, su, rules)
      if (tS != t)
        history += ("simplified: " + presentObj(t) + " ~~> " + presentObj(tS))
      tS
  }
   
  /** special case simplify: no expansion, full recursion */
  def simplify(t : Obj)(implicit stack: Stack, history: History): t.ThisType = simplify(t, false, true)
   
  /** special case of simplify: expansion, limited recursion */
  private def headNormalize(t: Term)(implicit stack: Stack, history: History): Term = {
    if (stability.is(t)) t
    else simplify(t, true, false)
  }

   /** simplifies one step overall */
   private def safeSimplifyOne(tm: Term)(implicit stack: Stack, history: History): Term = {
     if (checkingUnit.isKilled) {
       return tm
     }
     history += "trying to simplify " + presentObj(tm)
     val tmS = tm match {
       case OMS(p) =>
         val d = getDef(p)
         d match {
           case Some(tD) =>
             history += "expanding definition of " + p
             tD
           case None =>
             // TODO apply abbrev rules?
             stability.set(tm) // undefined constants are stable 
             tm             
         }
       case OMV(n) =>
         val vd = outerContext.getO(n) getOrElse stack.context(n)
         if (solution.isDeclared(n)) {
           // unknowns are unstable, even if solved, their definiens should be expanded via SubstituteUnknowns
           tm
         } else vd.df match {
           case Some(tD) =>
             history += "expanding definition of " + n
             tD
           case _ =>
             stability.set(tm)
             tm
         }
       case OMAorAny(Free(cont,bd), args) if cont.length == args.length =>
           // should be done in SubstituteUnknowns, but sometimes the definitions of unknowns get expanded anyway
           val sub = (cont / args).get // defined due to guard
           bd ^? sub
       case t: OMLITTrait =>
         stability.set(t) // literals are always stable
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
                  log("~~>" + presentObj(tmS))
                  history += "applied computation rule " + rule.toString
                  history += "simplified: " + presentObj(tm) + " ~~> " + presentObj(tmS)
                  return tmS
                case cannot: CannotSimplify =>
                  simp = simp join cannot
              }
            }
          }
          // no applicable rule: check stability
          val recursePositions = simp.getPositions(tm.subobjects.length)
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
                  if (stability.is(sN.target))
                    stability.set(sN)
                  sN
                case vd: VarDecl =>
                  val vdN = vd.map {t => safeSimplifyOne(t)(stack ++ con.take(i-subs.length), h)}
                  if ((vdN.tp.toList ::: vdN.df.toList).forall {t => stability.is(t)})
                    stability.set(vdN)
                  vdN
                case t: Term =>
                  safeSimplifyOne(t)(stack ++ con, h) 
              }
              if (o hasheq oN) {
                stable &&= stability.is(oN)
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
              stability.set(tm)
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
   @scala.annotation.tailrec
   final override def safeSimplifyUntil[A](tm: Term)(simple: Term => Option[A])(implicit stack: Stack, history: History): (Term,Option[A]) = {
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
   private def safeSimplifyUntilRuleApplicable[R <: SingleTermBasedCheckingRule](tm: Term, hs: Iterable[R])(implicit stack: Stack, history: History): (Term,Option[R]) =
      safeSimplifyUntil[R](tm)(t => hs.find(_.applicable(t)))

      
  // ******************************************************************************************
  // *** algorithms for contexts
  // ******************************************************************************************

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

  // ******************************************************************************************
  // *** auxiliary functions for solving
  // ******************************************************************************************

   /** tries to solve an unknown occurring in tm1 in terms of tm2
    *
    *  returns true if the unknowns were solved and the equality proved
    *  otherwise, returns false without state change (returning false here does not signal that the equality is disproved)
    */
   private def solveEquality(j: Equality, seenSofar: List[Equality])(implicit history: History): Boolean = {
      if (seenSofar contains j) return false // cycle in recursive simplification, so give up
      implicit val stack = j.stack
      log("Solving " + j.present)
      j.tm1 match {
         //foundation-independent case: direct solution of an unknown variable
        case Unknown(m, as) =>
          var vars = isDistinctVarList(as).getOrElse(return false)
          // solve m with j.tm2
          var mSol = FreeOrAny(vars, j.tm2)
          // depending on he order in which variables are solved, it's possible m already occurs in vars
          // so we have to substitute it before checking for a cycle
          if (vars.freeVars contains m) {
            vars = new SubstituteUnknowns(m -> mSol).traverseObject(vars)(Context.empty, ())
            mSol = FreeOrAny(vars, j.tm2)
          }
          moveToRight(m)
          val remainingFreeVars = notAllowedInSolution(m, mSol)
          if (remainingFreeVars.isEmpty) {
             // we can solve m already, the remaining unknowns in tm2 can be filled in later
             val res = solve(m, mSol)
             j.tpOpt foreach {case tp => solveType(m, FreeOrAny(vars, tp))}
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
                    log("Using solution rule " + rs.head.toString)
                    return solveEquality(j2,j::seenSofar)((history + msg).branch)
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
            val mTp = FreeOrAny(vars, tp)
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
       case (u@Unknown(_, _), b) => (u,b,true)
       case (b, u@Unknown(_, _)) => (u,b,false)
       case _ => return false
     }
     unk match {
         //foundation-independent case: direct solution of an unknown variable
         case Unknown(m, args) =>
            val vars = isDistinctVarList(args).getOrElse(return false)
            val mBd = FreeOrAny(vars, bound)
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

   
  // ******************************************************************************************
  // *** auxiliary functions for iterating over rules
  // ******************************************************************************************

  /* convenience function for going to the next rule after one has been tried */
  private def dropTill[A](l: List[A], a: A) = l.dropWhile(_ != a).tail
  private def dropJust[A](l: List[A], a:A) = l.filter(_ != a)
  
  /** like below but for the special case where the rules can be applied generically */
  private def tryAllUnaryRules[A <: UnaryTermRule](rules : List[A], term : Term)(implicit stack : Stack, history : History) : Option[Boolean] = {
    tryAllRules(rules, term)((r,t,h) => r.apply(this)(t)(stack, h))
  }
  
  /** like below but for a single term */
  private def tryAllRules[A <: SingleTermBasedCheckingRule,B](rules: List[A],term: Term)(rulecheck : (A,Term,History) => Option[B])(implicit stack: Stack, history: History) : Option[B] = {
    var rulesV = rules
    var tmS = term
    var ret : Option[B] = None
    While (ret.isEmpty) {
      safeSimplifyUntilRuleApplicable(tmS, rulesV) match {
        case (tS,Some(rule)) =>
          tmS = tS
          log("Trying " + rule.toString)
          ret = rulecheck(rule, tmS, history + ("trying " + rule.toString))
          if (ret.isEmpty) {
            log("Rule " + rule.toString + " not applicable")
            rulesV = dropJust(rulesV, rule)
          }
        case _ =>
          log("no rule applicable")
          history += "no rule applicable"
          While.break
      }
    }
    ret
  }

  /** simplifies two terms until a rule is applicable and then applies that rule
   *  @tparam A the type of rules
   *  @param rules the rules to try
   *  @param tm1 the first term
   *  @param tm2 the second term
   *  @param condition an applicability condition that is used to filter rules
   *  @param rulecheck the function that applies the rule; if this returns None, the rule is removed and we try again
   *  @return the result of applying the rule if any rule was applicable 
   */
  private def tryAllRules[A <: CheckingRule,B](rules: List[A], tm1: Term, tm2: Term)
       (condition: (A,Term,Term) => Boolean)(rulecheck: (A,Term,Term,History) => Option[B])(implicit stack: Stack, history: History) : Option[B] = {
    var rulesV = rules
    var ret : Option[B] = None
    var (tm1S,tm2S) = (tm1,tm2)
    While (ret.isEmpty) {
      safeSimplifyUntil(tm1S,tm2S)((t1,t2) => rulesV.find(condition(_,t1,t2))) match {
        case (t1S,t2S,Some(rule)) =>
          tm1S = t1S
          tm2S = t2S
          log("Trying rule " + rule.toString)
          ret = rulecheck(rule,tm1S,tm2S,history + ("trying " + rule.toString))
          if (ret.isEmpty) {
            log("rule " + rule.toString + " not applicable")
            rulesV = dropJust(rulesV, rule)
          }
        case _ =>
          log("no rule applicable")
          history += "no rule applicable"
          While.break
      }
    }
    ret
  }
}