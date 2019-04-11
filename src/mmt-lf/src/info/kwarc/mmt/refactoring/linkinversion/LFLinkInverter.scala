package info.kwarc.mmt.refactoring.linkinversion

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Link, Theory, View}
import info.kwarc.mmt.api.objects.{Context, Matcher}
import info.kwarc.mmt.api.refactoring.linkinversion.{LinkInverter, RewriteErrorHandler}
import info.kwarc.mmt.api.{MPath, RuleSet}

/**
	* Invert links.
	*
	* @todo ComFreek: Add bluenote describing this.
	*
	* @see [[info.kwarc.mmt.api.refactoring.linkinversion.LinkInversionRulesProvider]]
	* @see [[LFLinkInversionRulesProvider]]
	*/
object LFLinkInverter {
	def invertLink(R: Theory, S: Theory, RToS: Link,
								 newModulePath: MPath,
								 generatedMorphismPath: MPath,
								 rewriteErrorHandler: RewriteErrorHandler)
								(implicit
								 ctrl: Controller): (Theory, View) = {

		val matcher = new Matcher(
			ctrl,

			// Might need RToS.to.toMPath in context as well?
			// TODO(ComFreek)
			RuleSet.collectRules(ctrl, Context(S.path))
		)

		val linkInversionRulesProvider = new LFLinkInversionRulesProvider(ctrl, matcher)

		LinkInverter.invertLink(
			R,
			S,
			RToS,
			linkInversionRulesProvider,
			newModulePath,
			generatedMorphismPath,
			rewriteErrorHandler
		)
	}
}
