package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects._
import frontend._

case class ValidationUnit(component: CPath, unknowns: Context, judgement: WFJudgement)

class Validator(controller: Controller) extends Logger {
      val logPrefix = "validator"
      val report = controller.report
      def apply(v: ValidationUnit)(errorCont: Invalid => Unit) {
         log("validation unit " + v.component + ": " + v.judgement)
         val solver = new Solver(controller, v.judgement.stack.theory, v.unknowns)
         val result = logGroup{solver(v.judgement)}
         val solution = solver.getSolution
         if (result && solution.isDefined) {
            log("validated " + v.component)
            log("solution: " + solution.get.toString)
            val tc = controller.globalLookup.getComponent(v.component)
            tc.analyzed = v.judgement.wfo ^ solution.get
         } else {
            log("errors while validating " + v.component)
            log(solver.toString)
            solver.getConstraints foreach {
               case j: WFJudgement =>
                  errorCont(InvalidObject(j.wfo, j.toString))
               case j => errorCont(InvalidObject(v.judgement.wfo, "unresolved constraint: " + j.toString))
            }
            //errorCont(InvalidObject(v.judgement.wfo, solver.toString))
         }
      }
}