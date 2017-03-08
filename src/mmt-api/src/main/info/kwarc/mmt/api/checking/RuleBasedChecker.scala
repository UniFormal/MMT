package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import symbols._
import modules._
import frontend._
import parser._

/**
 * the primary object level checker of MMT
 * 
 * It checks [[CheckingUnit]]s by invoking [[Solver]]s and updates the checked objects with the solutions.
 * It manages errors and dependencies.  
 */
class RuleBasedChecker extends ObjectChecker {
   override val logPrefix = "object-checker"

   def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment) = {
      val con = cu.judgement.context
      val tm = cu.judgement.wfo
      val prOrg = ParseResult(cu.unknowns, con, tm)
      
      def fail(msg: String) = {
        env.errorCont(InvalidUnit(cu, NoHistory, msg))
        val tm = ParseResult
        CheckingResult(false, None, prOrg.toTerm)
      }
      if (cu.isKilled) {
        fail("not checked")
      }
      
      log("checking unit " + cu.component.getOrElse("without URI") + ": " + cu.judgement.present(o => controller.presenter.asString(o)))
      log("full form of term: " + cu.judgement.wfo.toString)
      // if a component is given, we perform side effects on it
      val updateComponent = cu.component map {comp =>
         controller.globalLookup.getComponent(comp) match {
            case tc: TermContainer =>
               tc.dependsOn.clear
               (comp,tc)
            case _ => throw ImplementationError("not a TermContainer")
         }
      }
      // ** checking **
      log("using " + rules.getAll.mkString(", "))
      val solver = new Solver(controller, cu, rules)
      val mayHold = logGroup {
         solver.applyMain
      }
      // if solved, we can substitute all unknowns; if not, we still substitute partially
      val psol = solver.getPartialSolution
      val remUnknowns = solver.getUnsolvedVariables 
      val subs = psol.toPartialSubstitution
      val contm = prOrg.copy(unknown = Context.empty).toTerm
      val contmI = contm ^? subs //fill in inferred values
      val contmIS = SimplifyInferred(contmI, rules, cu.context ++ remUnknowns) //substitution may have created simplifiable terms
      TermProperty.eraseAll(contmIS) // reset term properties (whether a term is, e.g., simplified, depends on where it is used)
      val pr = ParseResult.fromTerm(contmIS).copy(unknown = remUnknowns)
      val result = pr.toTerm
      //now report results, dependencies, errors
      val success = solver.checkSucceeded
      if (success) {
        // free variables may remain but their types are solved
        if (pr.free.exists({case IncludeVarDecl(_) => false case _ => true})) {
          env.errorCont(InvalidUnit(cu, NoHistory, "check succeeded, but free variables remained: " + pr.free.map(_.name).mkString(", ")))
        }
      }
      // ** logging and error reporting **
      if (success) {
         log("success")
         updateComponent foreach {case (comp, tc) =>
            solver.getDependencies foreach {d =>
               env.reCont(ontology.DependsOn(comp, d))
               tc.dependsOn += d
            }
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
      updateComponent foreach {case (comp, tc) =>
         // hadValue: true if a previous successful analysis result was known
         // changed: true if hadValue and the new analysis result differs from the previous one
         val (hadValue, changed) = tc.getAnalyzedIfFullyChecked match {
           case Some(r) =>
             // new value differs from old value
             (true, r != result)
           case None =>
             // this happens in particular when checking a document from scratch
             // in that case, we compare to the previously-loaded version (if any) that is still remembered by the library
             controller.previousLocalLookup.getO(comp.parent).flatMap(_.getComponent(comp.component)) match {
               case None => (false, false)
               case Some(tcP: TermContainer) =>
                 val p = tcP.getAnalyzedIfFullyChecked
                 (p.isDefined, !p.contains(result))
               case Some(_) => (true,false) // impossible
             }
         }
         tc.analyzed = result // set it even if unchanged so that dirty flag gets cleared
         if (! success)
            tc.setAnalyzedDirty // revisit failed declarations
         if (changed) {
            log("changed")
            controller.memory.content.notifyUpdated(comp) //TODO: this could be cleaner if taken care of by the onCheck method
         } else {
            if (hadValue)
              log("not changed")
            else
              log("new value")
         }
      }
      CheckingResult(success, Some(psol), result)
   }
   /**
    * A Traverser that reduces all redexes introduced by solving unknowns.
    */
   private object SimplifyInferred extends Traverser[RuleSet] {
      def traverse(t: Term)(implicit con : Context, rules: RuleSet) : Term = {
         t match {
            case OMA(OMS(parser.ObjectParser.oneOf), uom.OMLiteral.OMI(i) :: args) =>
               Traverser(this, args(i.toInt))
            case _ if parser.SourceRef.get(t).isEmpty =>
               controller.simplifier(t, con, rules)
            case _ =>
               Traverser(this, t)
         }
      }
   }
}