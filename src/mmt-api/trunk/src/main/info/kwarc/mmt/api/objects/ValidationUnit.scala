package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects._
import frontend._

case class ValidationUnit(component: CPath, unknowns: Context, judgement: WFJudgement)

class Validator(controller: Controller) extends Logger {
      val logPrefix = "validator"
      val report = controller.report
      def apply(v: ValidationUnit)(errorCont: Invalid => Unit) {
         log("validation unit " + v.component + ": " + v.judgement.present(controller.presenter.asString))
         val solver = new Solver(controller, v.judgement.stack.theory, v.unknowns)
         val result = logGroup{
            solver.apply(v.judgement)
         }
         val tc = controller.globalLookup.getComponent(v.component)
         // if solved, this substitutes all unknowns; if not, we still substitute partially
         val psol = solver.getPartialSolution
         val remUnknowns = solver.getUnsolvedVariables 
         val subs = psol.toPartialSubstitution
         val tI = v.judgement.wfo ^ subs //fill in inferred values
         val tIS = SimplifyInferred(tI,remUnknowns) //substitution may have created redexes
         tc.analyzed = if (remUnknowns.variables.isEmpty) tIS else OMBIND(OMID(parser.AbstractObjectParser.unknown), remUnknowns, tIS)
         //now report result/errors
         val solution = solver.getSolution
         val success = result && solution.isDefined
         if (success)
            log("success")
         else {
            log("failure" + (if (result) " (not proved)" else " (disproved)"))
            logGroup {
               solver.logState(logPrefix)
               val errors = solver.getErrors
               errors foreach {e =>
                  errorCont(InvalidJudgement(e.narrowDownError))
               }
               if (errors.isEmpty) {
                  solver.getConstraints foreach {dc =>
                     val h = dc.history.narrowDownError + "unresolved constraint"
                     errorCont(InvalidJudgement(h))
                  }
               }
            }
         }
      }
      
      /**
       * A Traverser that simplifies all subterms that are the result of inference (recognized by the lack of a SourceRef)
       */
      object SimplifyInferred extends StatelessTraverser {
         def traverse(t: Term)(implicit con : Context, init : Unit) : Term = {
            if (parser.SourceRef.get(t).isEmpty)
               controller.uom.simplify(t, con)
            else
               Traverser(this, t)
         }
      }
}