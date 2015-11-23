package info.kwarc.mmt.LFX.LFWTypes

import info.kwarc.mmt.api.{objects, LocalName}
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import objects.Conversions._
import info.kwarc.mmt.lf._

object Common {
  /** convenience function for recursively checking the judgement |- a: type */
  def isType(solver: Solver, a: Term)(implicit stack: Stack, history: History) =
    solver.check(Typing(stack, a, OMS(Typed.ktype), Some(OfType.path)))(history + "type of bound variable must be a type")


  def pickFresh(solver: Solver, x: LocalName)(implicit stack: Stack) =
    Context.pickFresh(solver.constantContext ++ solver.getPartialSolution ++ stack.context, x)
}

/** Formation: the type inference rule x:A:type|-B(x) type  --->  W x:A.B : type  * */
object WTypeTerm extends FormationRule(WType.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case WType(x,a,b) =>
        if (!covered) Common.isType(solver,a)
        val (xn,sub) = Common.pickFresh(solver,x)
        val tpb = solver.inferType(b ^? sub)(stack ++ xn%a, history).getOrElse(return None)
        if (solver.check(Equality(stack,tpb,OMS(Typed.ktype),None))) Some(OMS(Typed.ktype)) else None
    }
  }
}


/** Introduction: the type inference rule |-t1:A, |-g:B(t1) -> W x:A.B(x)  --->  sup(t1,g) : W x:A.B(x)  * */
object SupTerm extends IntroductionRule(sup.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case sup(t1,t2) =>
        val tpA = solver.inferType(t1)(stack, history).getOrElse{
          history+="could not infer type of "+solver.presentObj(t1); return None}
        // val (xn,sub) = Common.pickFresh(solver,LocalName("x"))
        history+=("type of "+solver.presentObj(t1)+" is "+solver.presentObj(tpA))
        val tpg = info.kwarc.mmt.lf.Common.makePi(solver,solver.inferType(t2)(stack,history).getOrElse{
          history+="could not infer type of "+solver.presentObj(t2); return None
        })
        history+=("type of "+solver.presentObj(t2)+" is "+solver.presentObj(tpg))
        tpg match {
          case Pi(x,tpBt1,w) =>
            val w2 = solver.safeSimplifyUntil(w)(WType.unapply)._1
            w2 match {
              case WType(y,tpA2,tpB) =>
                if (solver.check(Equality(stack,tpA,tpA2,Some(OMS(Typed.ktype)))) &&
                  solver.check(Equality(stack,tpBt1,tpB ^? y / t1,Some(OMS(Typed.ktype))))
                  ) Some(w) else None
              case _ => None
            }
            //
          case _ => None
        }
      case _ => None // should be impossible
    }
  }
}

/** Equality rule for W-types */

object WEquality extends TermBasedEqualityRule {
  val head = WType.path
  private val heads = List(Some(WType.path))
  def applicable(tm1: Term, tm2: Term) = heads.contains(tm1.head) && heads.contains(tm2.head)
  def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
    (tm1,tm2) match {
      case (WType(x1,a1,t1), WType(x2,a2,t2)) =>
        val cont = Continue {
          history += "congruence for Sigma types"
          val res1 = checker.check(Equality(stack,a1,a2,None))(history + "equality of domain types")
          val (xn,_) = Context.pickFresh(stack.context, x1)
          val t1sub = t1 ^? (x1 / OMV(xn))
          val t2sub = t2 ^? (x2 / OMV(xn))
          val res2 = checker.check(Equality(stack ++ xn % a1, t1sub, t2sub, None))(history + "equality of scopes")
          res1 && res2
        }
        Some(cont)
      case _ => None
    }
  }
}

/** Elimination: |- C: W x:A.B -> type , |- p: Pi a:A. Pi (u: B(a) -> W x:A.B). [ (Pi y:B(a). C(u(y)) ) -> C(sup(a,u)) ]
  * ---> rec(C,p) : Pi z: (W x:A.B) . C(z)                                       -------------------------------------g
  * */
object recTerm extends EliminationRule(rec.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case rec(c,p) =>
      val tpC = info.kwarc.mmt.lf.Common.makePi(solver,
        solver.inferType(c)(stack,history).getOrElse{history+="could not infer type of "+solver.presentObj(c); return None})
      val (x,_) = Context.pickFresh(stack.context, LocalName("x"))
      tpC match {
        case Pi(x1,some,tp) =>
          if(!solver.check(Equality(stack,tp,OMS(Typed.ktype),None))) {
            history+=solver.presentObj(tp)+" is not type"
            None
          } else solver.safeSimplifyUntil(some)(WType.unapply)._1 match {
            case WType(x2,tpA,b) =>
            val tpB = b ^? x2 / OMV(x)
            val wtp = WType(x,tpA,tpB)
            val tpp = info.kwarc.mmt.lf.Common.makePi(solver,
              solver.inferType(p)(stack,history).getOrElse{history+="could not infer type of "+solver.presentObj(p); return None})
            tpp match {
              case Pi(a,tpA2,q) =>
                if(!solver.check(Equality(stack,tpA2,tpA,Some(OMS(Typed.ktype))))) {
                  history+="not equal: "+solver.presentObj(tpA2)+" and "+solver.presentObj(tpA)
                  None
                } else {
                  info.kwarc.mmt.lf.Common.makePi(solver,q) match {
                    case Pi(u,batoW,g) =>
                      info.kwarc.mmt.lf.Common.makePi(solver,batoW) match {
                        case Pi(x3,ba,w) =>
                          if(!solver.check(Equality(stack,ba,tpB ^? x/OMV(a),Some(OMS(Typed.ktype))))) {
                            history+="not equal: "+solver.presentObj(ba)+" and "+solver.presentObj(tpB ^? x/OMV(a))
                            None
                          } else if(!solver.check(Equality(stack,w,wtp,Some(OMS(Typed.ktype))))) {
                            history+="not equal: "+solver.presentObj(w)+" and "+solver.presentObj(wtp)
                            None
                          } else info.kwarc.mmt.lf.Common.makePi(solver,g) match {
                            case Pi(x4,batoC,csupau) =>
                              if(!solver.check(Equality(stack,csupau,ApplySpine(c,sup(a,u)),Some(OMS(Typed.ktype))))) {
                                history+="not equal: "+solver.presentObj(csupau)+" and "+solver.presentObj(Apply(c,sup(a,u)))
                                None
                              } else info.kwarc.mmt.lf.Common.makePi(solver,batoC) match {
                                case Pi(y,supba,supcuy) =>
                                  if(!solver.check(Equality(stack,supba,tpB ^? x/OMV(a),Some(OMS(Typed.ktype))))) {
                                    history+="not equal: "+solver.presentObj(supba)+" and "+solver.presentObj(tpB ^? x/OMV(a))
                                    None
                                  } else if(!solver.check(Equality(stack,supcuy,ApplySpine(c,ApplySpine(u,OMV(y))),Some(OMS(Typed.ktype))))) {
                                    history+="not equal: "+solver.presentObj(supcuy)+" and "+solver.presentObj(ApplySpine(c,ApplySpine(u,OMV(y))))
                                    None
                                  } else {
                                    val (z,_) = Common.pickFresh(solver,LocalName("x"))
                                    Some(Pi(z,w,ApplySpine(c,OMV(z))))
                                  }
                                case _ =>
                                  history+=solver.presentObj(batoC)+" has wrong type"
                                  None
                              }
                            case _ =>
                              history+=solver.presentObj(g)+" is not a function"
                              None
                          }
                        case _ =>
                          history+=solver.presentObj(batoW)+" is not a function"
                          None
                      }
                    case _ =>
                      history+=solver.presentObj(q)+" is not a function"
                      None
                  }
                }
              case _ =>
                history+=solver.presentObj(tpp)+" is not a function"
                None
            }
            case _ =>
              history+=solver.presentObj(some)+" is not a W-Type"
              None
          }
        case _ =>
          history+=(solver.presentObj(c)+" has wrong type "+solver.presentObj(tpC))
          None
      }
    case _ => None
  }
}

/** Computation: the rule rec(C,p)(sup(a,u))=p(a,u,[y]rec(C,p)(u(y))) */

object recApply extends ComputationRule(Apply.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term]
  = {
    def eliminaterec(c:Term,p:Term,a:Term,u:Term,stack1:Stack) : Term = {
      val input=Apply(rec(c,p),sup(a,u))
      val domu = solver.simplify(solver.inferType(u).getOrElse(return input)) match {
        case Pi(_, d, _) => d
        case Arrow(d, _) => d
        case _ => return input
      }
      val (y, _) = Context.pickFresh(stack.context, LocalName("r"))
      history += "Expanding recursive definition"
      val out = solver.simplify(Apply(u,OMV(y)))(stack1++y%domu,history) match {
        case sup(a1,u1) => eliminaterec(c,p,a1,u1,stack1++y%domu)
        case _ => Apply(rec(c,p),Apply(u,OMV(y)))
      }
      solver.simplify(ApplySpine(p, a, u, Lambda(y, domu, out)))
    }
    solver.simplify(tm) match {
      case Apply(rec(c, p), t) =>
        solver.simplify(t)(stack, history) match {
          case sup(a, u) => Some(eliminaterec(c,p,a,u,stack))
          case _ => None
        }
      case _ => None
    }
  }
}

/** equality-checking: the eta rule |- a = b : A, |- f = g : B(a)->W ---> |-sup(a,f) = sup(b,g) */
object supEquality extends TypeBasedEqualityRule(Nil, WType.path) {
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val (a1,u1,a2,u2) = (solver.safeSimplifyUntil(tm1)(sup.unapply)._1,solver.safeSimplifyUntil(tm2)(sup.unapply)._1) match {
      case (sup(a,f),sup(b,g)) => (a,f,b,g)
      case _ => return None
    }
    Some(solver.check(Equality(stack,a1,a2,None))&&solver.check(Equality(stack,u1,u2,None)))
  }
}