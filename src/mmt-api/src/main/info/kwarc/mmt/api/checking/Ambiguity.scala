package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import parser._
import uom._
import OMLiteral.OMI

/**
 * oneOf(OMI(i),a_0,...a_n)  --->  a_i
 */
object Disambiguation extends ComputationRule(ObjectParser.oneOf) {
   def apply(checker: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = tm match {
      case ComplexTerm(ObjectParser.oneOf, _, namedParts, choice :: alternatives) =>
         choice match {
            case OMI(i) =>
               val subs = namedParts.toPartialSubstitution
               Simplify(alternatives(i.toInt) ^? subs)
            case _ => Recurse
         }
      case _ => Simplifiability.NoRecurse
   }
}

/**
 * solves unknown X in oneOf(X,a_0,...a_n) as i iff a_i is the only alternative whose type can be inferred
 */
object InferAmbiguous extends InferenceRule(ObjectParser.oneOf,ObjectParser.oneOf) {
   def apply(checker: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = tm match {
      case ComplexTerm(ObjectParser.oneOf, _, namedParts, choice :: alternatives) =>
         val subs = namedParts.toPartialSubstitution // always total if the term comes from NotationBasedParser
         def choose(i: BigInt) = {
           checker.inferType(alternatives(i.toInt) ^? subs, covered)
         }
         
         choice match {
            case OMI(i) =>
               history += "already disambiguated"
               choose(i)
            case checker.Unknown(_, _) =>
               // first (try to) infer the types of the shared terms, that can solve unknowns that are needed for disambiguation
               val namedPartsI = subs map { sub =>
                  history += "infering type of the named part " + checker.presentObj(sub.target)
                  val tp = checker.inferType(sub.target)(stack, history.branch)
                  tp match {
                     case Some(t) => history += checker.presentObj(t)
                     case None => history += "failed"
                  }
                  VarDecl(sub.name, None, tp, Some(sub.target), None)
               }
               history += "trying to disambiguate: inferring type of all alternatives"
               // now try dryRun type inference on every alternative
               val results = alternatives.zipWithIndex.map { case (a, i) =>
                  history += s"trying number $i"
                  val res = checker.dryRun(true, (_: Any) => false) {
                     history.indented {
                        checker.inferType(a, covered)(stack ++ namedPartsI, history)
                     }
                  }
                  history += res.toString
                  (res, i)
               }
               val nonFailures = results.filter(_._1 != WouldFail)
               if (nonFailures.length == 0) {
                  checker.error("all alternatives ill-typed")
                  None
               } else if (nonFailures.length == 1) {
                  // we can solve for OMV(n) if type inference failed for all but one alternative
                  val theOne = nonFailures.head._2.toInt
                  // resolve the ambiguous term
                  checker.check(Equality(stack, choice, OMI(BigInt(theOne)), None))(history + "disambiguated")
                  // after solving the next application of inferType will succeed
                  // but we can return the right result immediately
                  choose(theOne)
               } else if (nonFailures.count(_._1.isInstanceOf[Success[_]]) > 1) {
                  history += "more than one alternative could be well-typed at this point"
                  None
               } else if (nonFailures.length == alternatives.length) {
                  history += "unable to disambiguate"
                  // we learned nothing new
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
      case _ => None
   }
}
