package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import objects._
import libraries._
import symbols._

/**
 * c(args) where c is a constant with a certain role
 */
case class AtomicJudgement(rl: String, operator: GlobalName, arguments: List[Term]) {
   def ^(sub: Substitution) = AtomicJudgement(rl, operator, arguments map (_ ^ sub))
}
/**
 * {params} Atomic1 -> ... -> Atomicn -> Atomic
 */
case class ComplexJudgement(parameters: Context, hypotheses: List[AtomicJudgement], thesis: AtomicJudgement) {
   def ^(sub: Substitution) = {
      val pS = parameters ^ sub
      val hS = hypotheses map (_ ^ sub)
      val tS = thesis ^ sub
      ComplexJudgement(pS, hS, tS)
   }
}
/**
 * simplest case: {params} Complex1 -> ... -> Complexn -> Atomic
 * general case: parameters and assumptions may alternate
 */
case class DeclarativeRule(arguments: List[RuleArgument], conclusion: AtomicJudgement) {
  def parameters : Context = arguments.collect {
    case RuleParameter(n,tp) => VarDecl(n,tp)
  }
  def assumptions : List[ComplexJudgement] = arguments.collect {
    case RuleAssumption(j) => j
  }
}

/** argument of a declarative rule: a parameter or an assumption */
abstract class RuleArgument
case class RuleParameter(name: LocalName, tp: Term) extends RuleArgument
case class RuleAssumption(judgment: ComplexJudgement) extends RuleArgument

/**
 *  defines pattern matchers on LF types for [[DeclarativeRule]]s
 *  @param lup needed to lookup the roles of constants
 *  @param roles the roles that form atomic judgements
 */
class RuleMatcher(lup: Lookup, roles: List[String]) {
   private def checkRole(p: GlobalName, allowedRoles: List[String]): Option[String] = {
      lup.get(p) match {
         case c: Constant => c.rl match {
            case Some(rl) if allowedRoles contains rl => Some(rl)
            case _ =>
              None
         }
         case _ => None
      }
   }
   /** matches an AtomicJudgement whose role is in roles */
   object Atomic {
      def unapply(t: Term): Option[AtomicJudgement] = {
        t match {
            case ApplyGeneral(OMS(f), args) =>
               checkRole(f, roles) map {rl => AtomicJudgement(rl, f,args)}
            case _ =>
              None
        }
      }
   }

   /** matches a ComplexJudgement */
   object Complex {
      def unapply(t: Term): Option[ComplexJudgement] = t match {
         case FunType(args, Atomic(thesis)) =>
            val params = args.takeWhile(_._1.isDefined)
            val hyps = args.drop(params.length).map {
               case (None, Atomic(a)) => a
               case _ => return None
            }
            Some(ComplexJudgement(FunType.argsAsContext(params), hyps, thesis))
         case _ => None
      }
   }

   /** matches a DeclarativeRule */
   object Rule {
      def unapply(t: Term): Option[DeclarativeRule] = t match {
         case FunType(args, Atomic(conc)) =>
            val ruleArgs = args map {
              case (Some(n), t) => RuleParameter(n,t)
              case (None, Complex(cj)) => RuleAssumption(cj)
              case (None, _) => return None
            }
            Some(DeclarativeRule(ruleArgs, conc))
         case _ => None
      }
   }
}
