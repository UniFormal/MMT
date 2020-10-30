package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, OMSReplacer}
import info.kwarc.mmt.api.uom.Simplifiability.NoRecurse
import info.kwarc.mmt.api.uom.{Simplifiability, SimplificationRule, SimplificationUnit, Simplify}
import info.kwarc.mmt.api.{??, GeneralError, GlobalName, LocalName, RuleSet}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations.MetaKeys
import info.kwarc.mmt.frameit.archives.{FrameIT, LabelVerbalizationRule, MitM}
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.Utils

import scala.util.Try

sealed trait ScrollViewRenderer {
  def apply(term: Term): Term
  def hasAssignmentFor(symbol: GlobalName): Boolean
}

class StaticRenderer(private val scroll: ElaboratedScrollReference)(implicit ctrl: Controller) extends ScrollViewRenderer {
  private val simplifier = new FullSimplifier(Context(scroll.problemTheory))
  override def apply(term: Term): Term = simplifier(term)

  override def hasAssignmentFor(symbol: GlobalName): Boolean = false
}

// @todo make it simplifying, too!
class ScrollApplicationRenderer(private val scrollApp: ScrollApplication)(implicit ctrl: Controller) extends ScrollViewRenderer {
  private val viewApplicator = new OMSReplacer {
    override def replace(p: GlobalName): Option[Term] = scrollApp.assignments.get(p)
  }

  // Since the scroll application might be less than total, we need both
  // the problem theory *and* the situation theory in context
  private val simplifier = new FullSimplifier(Context(scrollApp.ref.problemTheory) ++ scrollApp.situationTheory)

  override def apply(term: Term): Term = {
    simplifier(viewApplicator(term, Context(scrollApp.ref.problemTheory)))
  }

  override def hasAssignmentFor(symbol: GlobalName): Boolean = scrollApp.assignments.contains(symbol)
}

private class FullSimplifier(context: Context)(implicit ctrl: Controller) {
  private val simplificationUnit = SimplificationUnit(context, expandDefinitions = false, fullRecursion = true)

  private val ruleSet: RuleSet = {
    val foundRules = RuleSet.collectRules(ctrl, simplificationUnit.context)
    foundRules.add(new LabelVerbalizationRule()(ctrl.globalLookup))

    foundRules
  }

  def apply(t: Term): Term = {
    ctrl.simplifier(t, simplificationUnit, ruleSet)
  }
}
