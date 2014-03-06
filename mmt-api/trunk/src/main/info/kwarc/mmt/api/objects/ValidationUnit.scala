package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects._
import frontend._

/**
 * A validation unit encapsulates the proof obligations produced by the [[libraries.StructureChecker]] and passed on to the [[Solver]].
 * 
 * Each validation unit validates a single term that is part of a WFJudgement, i.e.,
 * the other parts of the judgement are assumed to be valid.
 * 
 * @param component the term component that is validated, e.g., namespace?theory?symbol?type
 * @param unknowns the unknowns parts of the expressions that should be inferred during validation
 * @param judgement the typing judegment to validate
 */
case class ValidationUnit(component: CPath, unknowns: Context, judgement: WFJudgement)

/**
 * A Validator is a stateless convenience data structure that provides a function for a validating [[ValidationUnit]]s.
 * It can manage errors and dependencies.  
 */
class Validator(controller: Controller) extends Logger {
      val logPrefix = "validator"
      val report = controller.report
      /**
       * @param v the validation unit to validate
       * @param errorCont a continuation that will be called on every validation error
       * @param depCont a continuation that will be called on every component that the validation depends on
       * @return (b,t) where b is true iff validation succeeded and
       * t is the result of substituting for the solved variables in the validated term 
       */
      def apply(v: ValidationUnit)(errorCont: Invalid => Unit, depCont: CPath => Unit): (Boolean,Term) = {
         log("validation unit " + v.component + ": " + v.judgement.present(controller.presenter.asString))
         val solver = new Solver(controller, v.judgement.stack.theory, v.unknowns)
         solver.logPrefix = v.component.toString
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
         val success = solver.checkSucceeded 
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