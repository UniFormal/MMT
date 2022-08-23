package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._
import objects._
import symbols._
import checking._
import uom._
import utils._
import notations.ImplicitArg

/**
 * a first step towards generalizing the SimplificationRuleGenerator towards arbitrary rules
 *
 * the usefulness is unclear because such rules usually theorem proving anyway
 * the DeclarativeRule abstractions are very nice though
 * it could also be interesting bootstrap the LF rules this way
 */

abstract class RuleGenerator extends ChangeListener {
  override val logPrefix = "rule-gen"
  protected val nameSuffix = "Rule"
  def ruleTags: List[String]
  def judgementTags: List[String]
  private val matcher = new RuleMatcher(controller.globalLookup, judgementTags)

  private def getGeneratedRule(p: Path): Option[GeneratedRule] = {
     p match {
        case p: GlobalName =>
           controller.globalLookup.getO(p / nameSuffix) match {
              case Some(r: RuleConstant) => r.df.map(df => df.asInstanceOf[GeneratedRule])
              case _ => None
           }
        case _ => None
     }
  }

  override def onAdd(e: StructuralElement): Unit = {onCheck(e)}
  override def onDelete(e: StructuralElement): Unit = {
     getGeneratedRule(e.path).foreach {r => controller.delete(r.rulePath)}
  }

  override def onCheck(e: StructuralElement): Unit = {
     val c = e match {
        case c: Constant if c.rl.exists(ruleTags.contains) =>
          if (c.tpC.analyzed.isDefined) {
             // check if an up-to-date rule for this constant exists already: if so break, otherwise delete it
             getGeneratedRule(c.path) foreach {r =>
                if (r.validSince >= c.tpC.lastChangeAnalyzed) {
                   log("rule is up-to-date")
                   return
                } else
                   controller.delete(r.rulePath)
             }
             c
          } else {
             log("not valid, skipped")
             return
          }
        case _ => return
     }
     val tp = c.tpC.analyzed.get
     if (parser.ObjectParser.isOnlyParsed(tp)) {
       log("type only partially validated, skipped")
       return
     }
     tp match {
        case matcher.Rule(ir) => generateRule(ir)
        case _ =>
           throw LocalError("not an inference rule: " + c.path)
     }
  }

  val TypingRuleTag = "Typing"
  val EqualityRuleTag = "Equality"

  /** reflects a declared rule into an implemented rule */
  private def generateRule(r: DeclarativeRule): Unit = {
     val ir = r.conclusion match {
        case AtomicJudgement(TypingRuleTag, typingOp, List(a,b)) =>
            //doesn't quite work because typing rules should be sufficient and *necessary*
            new TypingRule(???) {
              def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) = {
                 val rec = new Recurser(solver)
                 val mt = rec.makeMatcher
                 val matches = mt(rec.solverContext ++ stack.context, r.parameters) {eq => eq(a, tm) && eq(b, tp)}
                 matches match {
                   case MatchSuccess(sub, true) =>
                      Some(r.assumptions.forall(a => rec(a ^ sub)))
                   case _ =>
                    None // not applicable
                 }
              }
           }
     }
  }

  /** calls the solver to check a ComplexJudgment */
  private class Recurser(solver: Solver) {
     val solverContext = solver.constantContext ++ solver.getPartialSolution
     val solverVars = solverContext.map(_.name)

     def apply(cj: ComplexJudgement)(implicit context: Stack, h: History): Boolean = {
        val (newParams, sub) = Context.makeFresh(cj.parameters, solverVars ::: context.context.map(_.name))
        val newContext = context ++ newParams
        val j = cj.thesis match {
           case AtomicJudgement(TypingRuleTag, typingOp, List(a,b)) =>
              Typing(newContext, a ^? sub, b ^? sub, Some(typingOp))
           case AtomicJudgement(EqualityRuleTag, equalOp, args) if args.length >= 2 =>
              Equality(newContext, args.init.last ^? sub, args.last ^? sub, None)
           case _ =>
              throw LocalError("")
        }
        solver.check(j)
     }
     def makeMatcher = {
        new Matcher(solver.controller, solver.rules)
     }
  }
}

trait GeneratedRule {
   def tag: String
   def from : Constant
   def rulePath = from.path / tag
   def validSince : Long
}
