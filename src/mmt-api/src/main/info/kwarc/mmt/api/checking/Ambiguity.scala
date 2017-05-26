package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import parser._
import uom._
import OMLiteral.OMI

/**
 * oneOf(OMI(i),a_1,...a_n)  --->  a_i
 */
object Disambiguation extends ComputationRule(ObjectParser.oneOf) {
   def apply(checker: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      val OMA(OMS(ObjectParser.oneOf), choice::alternatives) = tm
      choice match {
         case OMI(i) => Some(alternatives(i.toInt))
         case _ => None
      }
   }
}

/**
 * solves unknown X in oneOf(X,a_1,...a_n) as i iff a_i is the only alternative whose type can be inferred
 */
object InferAmbiguous extends InferenceRule(ObjectParser.oneOf,ObjectParser.oneOf) {
   def apply(checker: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
      val OMA(OMS(ObjectParser.oneOf), choice::alternatives) = tm
      def choose(i: BigInt) = checker.inferType(alternatives(i.toInt), covered)
      /* TODO do this differently: OMA(ambiguous, unknown, option*, arg*)
       * options are pairs of GlobalName and number of leading implicit arguments, args are the explicit arguments
       * now infer the explicit arguments for real, then try to check in dryRuns
       */
      choice match {
         case OMI(i) =>
            history += "already disambiguated"
            choose(i)
         case OMV(n) =>
            history += "trying to disambiguate: inferring type of all alternatives" 
            // try type inference on every alternative
            val results = alternatives.zipWithIndex.map {case (a,i) =>
               history += s"trying number $i"
               val res = checker.dryRun(false) {
                  checker.inferType(a, covered)(stack, NoHistory)
               }
               history += res.toString
               (res,i)
            }
            val nonFailures = results.filter(_._1!=WouldFail)
            if (nonFailures.length == 1) {
               // we can solve for OMV(n) if type inference failed for all but one alternative
               val theOne = nonFailures.head._2.toInt
               checker.check(Equality(stack, choice, OMI(BigInt(theOne)), None))(history + "disambiguated")
               // after solving the next application of inferType will succeed
               // but we can return the right result immediately
               choose(theOne)
            } else if (nonFailures.length == alternatives.length) {
               history += "unable to disambiguate"
               // we learned nothing new
               None
            } else if (nonFailures.count(_._1.isInstanceOf[Success[_]]) > 1) {
              checker.error("ambiguous: more than one alternative is well-typed")
              None
            } else {
               history += "unable to disambiguate"
               None
               // TODO: drop the failing alternatives
               /* history += "removed some ill-typed alternatives"
               val nonFailingAlternatives = nonFailures.map{case(_,i) => alternatives(i)}
               Some(ObjectParser.oneOf(nonFailingAlternatives))*/
            }
      }
   }
}