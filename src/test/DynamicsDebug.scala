import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.{Context, Term}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{NamespaceMap, Path, RuleSet}
import info.kwarc.mmt.frameit.archives.LabelVerbalizationRule
import info.kwarc.mmt.lf.{Beta, RewriteRule}
import info.kwarc.mmt.lf.comptrans.{CompositionalTotalMorphism, CompositionalTranslation}

/**
  * @author Navid
  */
object StepUntilDebug extends MagicTest("debug") {
  override def run(): Unit = {
    implicit val ctrl: Controller = controller

    val thy = Path.parseM("http://mathhub.info/FrameIT/frameworld/integrationtests/dynamicsdebug?StepUntilDebug")
    val c = thy ? "my_list"
    val term = ctrl.getConstant(c).df.get

    val simplify: Term => Term = {
      val ctx = Context(thy)
      val simplificationRules = RuleSet.collectRules(ctrl, ctx)

      val relevantRules = simplificationRules.getAll.filter(_.getClass.getName.contains("StepUntil"))
      println(relevantRules)

      val simplicationUnit = SimplificationUnit(ctx, expandVarDefs = true, expandConDefs = true, fullRecursion = true)

      ctrl.simplifier(_, simplicationUnit, simplificationRules)
    }

    println(term)
    println()
    println(simplify(term))

    sys.exit(0)
  }
}

object IfThenElseDebug extends MagicTest("debug") {
  override def run(): Unit = {
    implicit val ctrl: Controller = controller
    ctrl.handleLine("log+ simp-rule-gen")

    val thy = Path.parseM("http://mathhub.info/FrameIT/frameworld/integrationtests/dynamicsdebug?IfElseDebug")
    val term = ctrl.getConstant(thy ? "c").df.get

    val simplify: Term => Term = {
      val ctx = Context(thy)
      val simplificationRules = RuleSet.collectRules(ctrl, ctx)

      val relevantRules = simplificationRules.get(classOf[RewriteRule]).filter(_.head.toString.contains("simplify_ifelse"))
      println(relevantRules)

      val simplicationUnit = SimplificationUnit(ctx, expandVarDefs = true, expandConDefs = true, fullRecursion = true)

      ctrl.simplifier(_, simplicationUnit, simplificationRules)
    }

    println(term)
    println()
    println(simplify(term))

    sys.exit(0)
  }
}
