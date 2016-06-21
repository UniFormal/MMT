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
 * {params} Complex1 -> ... -> Complexn -> Atomic
 */
case class DeclarativeRule(parameters: Context, assumptions: List[ComplexJudgement], conclusion: AtomicJudgement)

/** 
 *  defines pattern matchers on terms for [[InferenceRule]]s
 *  @param lup needed to lookup the roles of constants
 *  @param roles the roles that form atomic judgements 
 *  @param dedTag role of truth judgements
 */
class RuleMatcher(lup: Lookup, roles: List[String], dedTag: String) {
   private def checkRole(p: GlobalName, allowedRoles: List[String]): Option[String] = {
      lup.get(p) match {
         case c: Constant => c.rl match {
            case Some(rl) if roles contains rl => Some(rl)
            case _ => None
         }
         case _ => None
      }
   }
   /** matches an AtomicJudgement whose role is in roles */
   object Atomic {
      def unapply(t: Term): Option[AtomicJudgement] = {
         val t2 = t match {
            case Apply(OMS(ded), a) if checkRole(ded, List(dedTag)).isDefined => a 
            case _ => t
         }
         t2 match {
            case ApplySpine(OMS(f), args) =>
               checkRole(f, roles) map {rl => AtomicJudgement(rl, f,args)}
            case _ => None
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
   
   /** matches an InferenceRule */
   object Rule {
      def unapply(t: Term): Option[DeclarativeRule] = t match {
         case FunType(args, Atomic(conc)) => 
            val params = args.takeWhile(_._1.isDefined)
            val assumps = args.drop(params.length).map {
               case (None, Complex(c)) => c
               case _ => return None
            }
            Some(DeclarativeRule(FunType.argsAsContext(params), assumps, conc))
         case _ => None
      }
   }
}