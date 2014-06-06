package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import symbols._

/**
 * A Validator is a stateless convenience data structure that provides a function for a validating [[ValidationUnit]]s.
 * It can manage errors and dependencies.  
 */
class RuleBasedChecker extends ObjectChecker {
      override val logPrefix = "validator"
      /**
       * @param v the validation unit to validate
       * @param errorCont a continuation that will be called on every validation error
       * @param depCont a continuation that will be called on every component that the validation depends on
       * @return (b,t) where b is true iff validation succeeded and
       * t is the result of substituting for the solved variables in the validated term 
       */
      def apply(cu: CheckingUnit)(implicit errorCont: ErrorHandler, relCont: RelationHandler) {
         log("checking unit " + cu.component + ": " + cu.judgement.present(o => controller.presenter.asString(o)))
         val tc = controller.globalLookup.getComponent(cu.component) match {
            case tc: TermContainer => tc
            case _ => throw ImplementationError("not a TermContainer")
         }
         tc.dependsOn.clear
         // ** checking **
         val solver = new Solver(controller, cu.judgement.stack.theory, cu.unknowns)
         solver.logPrefix = cu.component.toString
         val mayHold = logGroup {
            solver.apply(cu.judgement)
         }
         // if solved, we can substitute all unknowns; if not, we still substitute partially
         val psol = solver.getPartialSolution
         val remUnknowns = solver.getUnsolvedVariables 
         val subs = psol.toPartialSubstitution
         val tI = cu.judgement.wfo ^ subs //fill in inferred values
         val tIS = SimplifyInferred(tI,cu.judgement.stack.theory,remUnknowns) //substitution may have created redexes
         val result = if (remUnknowns.variables.isEmpty) tIS else OMBIND(OMID(parser.ObjectParser.unknown), remUnknowns, tIS)
         //now report results, dependencies, errors
         val solution = solver.getSolution
         val success = solver.checkSucceeded
         // ** logging and error reporting **
         if (success) {
            log("success")
            solver.getDependencies foreach {d =>
               relCont(ontology.DependsOn(cu.component, d))
               tc.dependsOn += d
            }
         } else {
            log("failure" + (if (mayHold) " (not proved)" else " (disproved)"))
            logGroup {
               solver.logState(logPrefix)
               val errors = solver.getErrors
               errors foreach {e =>
                  errorCont(InvalidUnit(cu, e.narrowDownError))
               }
               if (errors.isEmpty) {
                  solver.getConstraints foreach {dc =>
                     val h = dc.history + "unresolved constraint"
                     errorCont(InvalidUnit(cu, h))
                  }
               }
            }
         }
         // ** change management **
         val changed = Some(result) != tc.analyzed
         tc.analyzed = result // set it even if unchanged so that dirty flag gets cleared
         if (! success)
            tc.setAnalyzedDirty // revisit failed declarations
         if (changed) {
            log("changed")
            controller.memory.content.notifyUpdated(cu.component) //TODO: this could be cleaner if taken care of by the onCheck method
         } else {
            log("not changed")
         }
      }
      /**
       * A Traverser that simplifies all subterms that are the result of inference (recognized by the lack of a SourceRef)
       */
      object SimplifyInferred extends Traverser[Term] {
         def traverse(t: Term)(implicit con : Context, theory: Term) : Term = {
            if (parser.SourceRef.get(t).isEmpty)
               controller.simplifier(t, theory, con)
            else
               Traverser(this, t)
         }
      }
}