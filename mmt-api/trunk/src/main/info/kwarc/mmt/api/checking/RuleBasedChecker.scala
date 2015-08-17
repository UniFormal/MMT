package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import symbols._
import modules._
import frontend._

/**
 * the primary object level checker of MMT
 * 
 * It checks [[CheckingUnit]]s by invoking [[Solver]]s and updates the checked objects with the solutions.
 * It manages errors and dependencies.  
 */
class RuleBasedChecker extends ObjectChecker {
   override val logPrefix = "object-checker"

   def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment) {
      log("checking unit " + cu.component + ": " + cu.judgement.present(o => controller.presenter.asString(o)))
      val tc = controller.globalLookup.getComponent(cu.component) match {
         case tc: TermContainer => tc
         case _ => throw ImplementationError("not a TermContainer")
      }
      tc.dependsOn.clear
      // ** checking **
      log("using " + rules.getAll.mkString(", "))
      val solver = new Solver(controller, cu.context, cu.unknowns, rules)
      solver.logPrefix = cu.component.toString
      val mayHold = logGroup {
         solver.apply(cu.judgement)
      }
      // if solved, we can substitute all unknowns; if not, we still substitute partially
      val psol = solver.getPartialSolution
      val remUnknowns = solver.getUnsolvedVariables 
      val subs = psol.toPartialSubstitution
      val tI = cu.judgement.wfo ^? subs //fill in inferred values
      val tIS = SimplifyInferred(tI, rules, cu.context ++ remUnknowns) //substitution may have created simplifiable terms
      TermProperty.eraseAll(tIS) // reset term properties (whether a term is, e.g., simplified, depends on where it is used)
      val result = if (remUnknowns.variables.isEmpty) tIS else OMBIND(OMID(parser.ObjectParser.unknown), remUnknowns, tIS)
      //now report results, dependencies, errors
      val solution = solver.getSolution
      val success = solver.checkSucceeded
      // ** logging and error reporting **
      if (success) {
         log("success")
         solver.getDependencies foreach {d =>
            env.reCont(ontology.DependsOn(cu.component, d))
            tc.dependsOn += d
         }
      } else {
         log("------------- failure " + (if (mayHold) " (not proved)" else " (disproved)"))
         logGroup {
            solver.logState(logPrefix)
            val errors = solver.getErrors
            errors foreach {e =>
               env.errorCont(InvalidUnit(cu, e.narrowDownError, cu.present(solver.presentObj)))
            }
            if (errors.isEmpty) {
               solver.getConstraints foreach {dc =>
                  val h = dc.history + "unresolved constraint"
                  env.errorCont(InvalidUnit(cu, h, cu.present(solver.presentObj)))
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
    * A Traverser that reduces all redexes introduced by solving unknowns.
    */
   private object SimplifyInferred extends Traverser[RuleSet] {
      def traverse(t: Term)(implicit con : Context, rules: RuleSet) : Term = {
         t match {
            case OMA(OMS(parser.ObjectParser.oneOf), OMI(i) :: args) =>
               Traverser(this, args(i.toInt))
            case _ if parser.SourceRef.get(t).isEmpty =>
               controller.simplifier(t, con, rules)
            case _ =>
               Traverser(this, t)
         }
      }
   }
}