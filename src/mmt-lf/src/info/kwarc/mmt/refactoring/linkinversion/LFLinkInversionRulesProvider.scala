package info.kwarc.mmt.refactoring.linkinversion

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Link
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.linkinversion.LinkInversionRulesProvider
import info.kwarc.mmt.api.refactoring.linkinversion.LinkUtils.getTermMappingForLink
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{ApplySpine, LF}

/**
	* Provide link inversion rules for links in a LF setting, i.e.
	* both domain and codomain modules import LF in some way.
	*
	* @param ctrl    The controller.
	* @param matcher A matcher to use, it should include LF typing rules, see example.
	* @example   ```val matcher = new Matcher(
	*            ctrl,
	*            // where T is a module which has LF imported (transitively)
	*            RuleSet.collectRules(ctrl, Context(T.path))
	*            )```
	* @see [[LFLinkInverter]]
	**/
class LFLinkInversionRulesProvider(private val ctrl: Controller, private val matcher: Matcher)
	extends LinkInversionRulesProvider {

	def getInverseRewritingRules(link: Link): RuleSet = {
		val rules = getTermMappingForLink(link)(ctrl)
			.map { case (domainSymbol, assignment) =>
				LFLinkInversionRulesProvider.getInverseSimplificationRule(matcher)(domainSymbol, assignment)
			}
			.toSet

		new RuleSet {
			/** the underlying set of rules */
			override def getAll: Iterable[Rule] = rules
		}
	}
}

private object LFLinkInversionRulesProvider {
	/**
		* TODO(ComFreek) Document
		*
		* @param matcher
		* @param domainSymbol
		* @param assignment
		* @return
		*/
	private def getInverseSimplificationRule(matcher: Matcher)(domainSymbol: GlobalName, assignment: Term): SimplificationRule = {
		val someHead = DPath(URI("http://example.com")) ? "module" ? "someHead"
		val LFLambdaBinder = OMID(LF._path ? "lambda")
		assignment match {
			case OMS(renamedSymbol) =>
				new AbbrevRule(renamedSymbol, OMID(domainSymbol))
			case OMBINDC(LFLambdaBinder, _, _) =>
				getInverseSimplificationRuleForLambdaBoundVariable(matcher)(
					domainSymbol,
					assignment.asInstanceOf[OMBINDC]
				)
			case _ =>
				// Otherwise, always rewrite the whole RHS to the LHS of the morphism assignment.
				new SimplificationRule(someHead) {
                  def apply(c: Context,t: Term): Simplifiability = if (t == assignment) Simplify(OMID(domainSymbol)) else Recurse
                }
		}
	}

	/**
		* Get an inverse simplification rule for morphism assignments whose definiens
		* is an LF lambda term such as `d := λab. ||a - b||`.
		*
		* The simplification rule is as follows:
		* ```
		* If (c := λ x_1, ..., x_n. s) is the assignment, then
		*
		*
		* t
		* ---------------------- if sσ = t for a substitution σ with dom(σ) ⊆ {x_1, ..., x_n}
		* s σ(x_1) ... σ(x_n)
		* ```
		*
		* @param matcher      A [[Matcher]] instance.
		* @param domainSymbol The LHS of the morphism assignment, e.g. when
		*                     `d := λab. ||a - b||`, then this is `d`.
		* @param assignment   The definiens of the morphism assignment, e.g. when
		*                     `d := λab. ||a - b||`, then this is `λab. ||a - b||`.
		* @return A simplification rule as described above.
		*/
	private def getInverseSimplificationRuleForLambdaBoundVariable(matcher: Matcher)
																																(domainSymbol: GlobalName, assignment: OMBINDC): SimplificationRule = {
		// "Ex:" annotations will take you through a running example throughout the code.
		//
		// Ex: Take the canonical morphism from metric to normed spaces:
		// Ex: domainSymbol = d, assignment = λab. ||a - b||

		val LFLambdaBinder = OMID(LF._path ? "lambda")

		val someHead = DPath(URI("http://example.com")) ? "module" ? "someHead"

		assignment match {
			// Ex: λab. ||a - b||  => lambdaScopes = List(||a - b||)
			case OMBINDC(binder, lambdaBoundContext, lambdaScopes) if binder == LFLambdaBinder =>
				assert(lambdaScopes.size == 1, "Lambda scope's size wasn't exactly 1.")

				// The "raw" template with original variable naming we will later match against.
				// Note that we will rename it before actually matching, see below.
				//
				// Ex: rawTemplateContext = {a: ..., b: ...}
				// Ex: rawTemplate = ||a - b||
				val (rawTemplateContext, rawTemplate) = (lambdaBoundContext, lambdaScopes.head)

				new SimplificationRule(someHead) {
					override def apply(simplifierContext: Context, termToSimplify: Term): Simplifiability = {
						// Ex: simplifierContext = {f: ..., a: ..., b: ..., -: ..., +: ..., apply:
						// ...}
						// Ex: (apply being LF?apply)
						// Ex: termToSimplify = ||(f a) - (f (a + b))||

						// 1. Prepare for matching
						// ========================
						val goalContext = simplifierContext
						val goal = termToSimplify

						// The matcher cannot handle cases where the template's variables clash
						// with free variables of the goal. Hence, rename the template and its
						// context:
						// Ex: templateContext = {c: ..., d: ...}
						// Ex: templateFreshnessSubstitution = [a -> c, b -> d]
						val (templateContext, templateFreshnessSubstitution) = Context.makeFresh(
							rawTemplateContext,
							forbidden = simplifierContext.domain
						)

						// Ex: template = ||c - d|| (rawTemplate was ||a - b||)
						val template = rawTemplate ^ templateFreshnessSubstitution

						// 2. Match
						// ========================
						matcher(
							goalContext,
							goal,
							templateContext,
							template
						) match {
							case MatchSuccess(matchingSubstitution, total) =>
								// Ex: matchingSubstitution = [c -> f a, d -> f (a + b)]
								// Ex: total = true
								// Ex: Hence, (||c - d||) ^ matchingSubstitution = ||(f a) - (f (a + b)||
								// Ex:        = goal
								//

								// TODO Is this check really necessary? [[RewriteRule]] does it, too.
								if (!(total ||
									template.freeVars.toSet.subsetOf(matchingSubstitution.domain.toSet))) {
									Recurse
								}

								// 3. Prepare provided arguments
								// ========================

								// Ex: We would now like to rewrite the initial term we got
								//    (||(f a) - (f (a + b)||) to "d (f a) (f (a + b))"
								//
								// The argument (f a), (f (a + b)) are exactly given by the
								// matchingSubstitution in the order the variables appeared in the
								// lambda context.

								val argumentToProvide: List[Term] = lambdaBoundContext.map(originalVariable => {
									// Ex: originalVariable = a
									// renamedFreshVariable = c
									val renamedFreshVariable: LocalName =
									templateFreshnessSubstitution(originalVariable.name) match {
										case Some(OMV(newName)) => newName
										case _ => throw new AssertionError("Freshness substitution was " +
											"exactly constructed to rename original variables, but now did " +
											"not contain one of them.")
									}

									// matchingSubstitution(c) = Some(f a)
									matchingSubstitution(renamedFreshVariable).getOrElse(
										throw new AssertionError("The codomain of the freshness " +
											"substitution must be a subseteq of the domain of the matching" +
											"substitution.")
									)
								})

								Simplify(ApplySpine(
									OMID(domainSymbol),
									argumentToProvide: _*
								))
							case _ =>
								Recurse
						}
					}
				}

			case _ => throw new IllegalArgumentException("Method called with OMBINDC " +
				"whose binder is not *the* urtheories binder, namely: " + LFLambdaBinder.path)
		}
	}
}