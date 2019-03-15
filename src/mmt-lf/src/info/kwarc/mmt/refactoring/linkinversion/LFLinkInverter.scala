package info.kwarc.mmt.refactoring.linkinversion

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Link, Theory}
import info.kwarc.mmt.api.objects.{Context, Matcher}
import info.kwarc.mmt.api.refactoring.linkinversion.{LinkInverter, RewriteErrorHandler}
import info.kwarc.mmt.api.{MPath, RuleSet}

// TODO(ComFreek) Document
object LFLinkInverter {
	def invertLink(R: Theory, S: Theory, RToS: Link,
								 newModulePath: MPath,
								 rewriteErrorHandler: RewriteErrorHandler)
								(implicit
								 ctrl: Controller): Theory = {

		val matcher = new Matcher(
			ctrl,

			// Might need RToS.to.toMPath in context as well?
			// TODO(ComFreek)
			RuleSet.collectRules(ctrl, Context(S.path))
		)

		val linkInversionRulesProvider = new LFLinkInversionRulesProvider(ctrl, matcher)

		LinkInverter.invertLink(R, S, RToS, linkInversionRulesProvider, newModulePath,
			rewriteErrorHandler)
	}
}
