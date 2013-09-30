package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects._
import frontend._

case class ValidationUnit(component: CPath, unknowns: Context, judgement: WFJudgement)

class Validator(controller: Controller) extends Logger {
      val logPrefix = "validator"
      val report = controller.report
      def apply(v: ValidationUnit)(errorCont: Invalid => Unit, depCont: CPath => Unit): (Boolean,Term) = {
         log("validation unit " + v.component + ": " + v.judgement.present(controller.presenter.asString))
         val solver = new Solver(controller, v.judgement.stack.theory, v.unknowns)
         val mayHold = logGroup {
            solver.apply(v.judgement)
         }
         // if solved, this substitutes all unknowns; if not, we still substitute partially
         val psol = solver.getPartialSolution
         val remUnknowns = solver.getUnsolvedVariables 
         val subs = psol.toPartialSubstitution
         val tI = v.judgement.wfo ^ subs //fill in inferred values
         val tIS = SimplifyInferred(tI,v.judgement.stack.theory,remUnknowns) //substitution may have created redexes
         val result = if (remUnknowns.variables.isEmpty) tIS else OMBIND(OMID(parser.AbstractObjectParser.unknown), remUnknowns, tIS)
         //now report results, dependencies, errors
         val solution = solver.getSolution
         val success = mayHold && solution.isDefined
         if (success) {
            log("success")
            solver.getDependencies foreach depCont
         } else {
            log("failure" + (if (mayHold) " (not proved)" else " (disproved)"))
            logGroup {
               solver.logState(logPrefix)
               val errors = solver.getErrors
               errors foreach {e =>
                  errorCont(InvalidUnit(v, e.narrowDownError))
               }
               if (errors.isEmpty) {
                  solver.getConstraints foreach {dc =>
                     val h = dc.history + "unresolved constraint"
                     errorCont(InvalidUnit(v, h))
                  }
               }
            }
         }
         (success,result)
      }
      
      /**
       * A Traverser that simplifies all subterms that are the result of inference (recognized by the lack of a SourceRef)
       */
      object SimplifyInferred extends Traverser[Term] {
         def traverse(t: Term)(implicit con : Context, theory: Term) : Term = {
            if (parser.SourceRef.get(t).isEmpty)
               controller.uom.simplify(t, theory, con)
            else
               Traverser(this, t)
         }
      }
}