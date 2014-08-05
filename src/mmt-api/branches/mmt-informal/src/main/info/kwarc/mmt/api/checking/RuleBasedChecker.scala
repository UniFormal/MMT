package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import symbols._

/**
 * the primary object level checker of MMT
 * 
 * It checks [[CheckingUnit]]s by invoking [[Solver]]s and updates the checked objects with the solutions.
 * It manages errors and dependencies.  
 */
class RuleBasedChecker extends ObjectChecker {
      override val logPrefix = "validator"
      def apply(cu: CheckingUnit)(implicit errorCont: ErrorHandler, relCont: RelationHandler) {
         log("checking unit " + cu.component + ": " + cu.judgement.present(o => controller.presenter.asString(o)))
         val tc = controller.globalLookup.getComponent(cu.component) match {
            case tc: TermContainer => tc
            case _ => throw ImplementationError("not a TermContainer")
         }
         tc.dependsOn.clear
         // ** checking **
         val solver = new Solver(controller, cu.context, cu.unknowns)
         solver.logPrefix = cu.component.toString
         val mayHold = logGroup {
            solver.apply(cu.judgement)
         }
         // if solved, we can substitute all unknowns; if not, we still substitute partially
         val psol = solver.getPartialSolution
         val remUnknowns = solver.getUnsolvedVariables 
         val subs = psol.toPartialSubstitution
         val tI = cu.judgement.wfo ^? subs //fill in inferred values
         val tIS = SimplifyInferred(tI,cu.context ++ remUnknowns) //substitution may have created redexes
         TermProperty.eraseAll(tIS) // reset term properties (whether a term is, e.g., simplified, depends on where it is used)
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
                  errorCont(InvalidUnit(cu, e.narrowDownError, cu.present(solver.presentObj)))
               }
               if (errors.isEmpty) {
                  solver.getConstraints foreach {dc =>
                     val h = dc.history + "unresolved constraint"
                     errorCont(InvalidUnit(cu, h, cu.present(solver.presentObj)))
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
      object SimplifyInferred extends StatelessTraverser {
         def traverse(t: Term)(implicit con : Context, u: Unit) : Term = {
            if (parser.SourceRef.get(t).isEmpty)
               controller.simplifier(t, con)
            else
               Traverser(this, t)
         }
      }
}