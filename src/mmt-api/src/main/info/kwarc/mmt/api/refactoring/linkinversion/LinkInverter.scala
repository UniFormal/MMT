package info.kwarc.mmt.api.refactoring.linkinversion

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Link, Theory, View}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.{HasMeta, RelationExp}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom.{AbbrevRule, SimplificationUnit}

import scala.collection.mutable

// @formatter:off
/**
	* Provider for inverse rewrite rules for links.
	*
	* Initially, the [[LinkInverter]] starts with the rules returned by
	* [[LinkInversionRulesProvider.getInverseRewritingRules()]] and starts rewriting
	* declarations.
	*
	* Different implementations may consider different "equality rules".
	*
	* - The simplest [[LinkInversionRulesProvider]] would return [[AbbrevRule]] instances
	*   for links which are renamings. Namely, those rules would rewrite the RHS of the
	*   renaming to the LHS.
	* - A more complex implementation could in addition provide rules for link mappings
	*   `d = d'` where `d' = OMBINDC(_, _, _)`. Namely, the rule would rewrite terms
	*   `t'`, which are the same as `d'` modulo a renaming of the binding context, to `d`.
	* - An even more complex implementation would also match terms, e.g. when `t'` is the
	*   body (scope) of the [[OMBINDC]] of `d'` with instantiated variables for the bound
	*   context, it woud rewrite `t'` to `d [arguments for the instantiation]`.
	*   See [[LFLinkInversionRulesProvider]].
	*/
//@formatter:on
trait LinkInversionRulesProvider {
	/**
		* Provide rewrite rules for the inverted link.
		*
		* @see [[LinkInversionRulesProvider]] for more documentation.
		* @param link The link.
		* @return Rewrite rules.
		*/
	def getInverseRewritingRules(link: Link): RuleSet
}

sealed abstract class RewriteError(originalDecl: Declaration,
																	 attemptedDecl: Option[Declaration])

/**
	*
	* @param originalDecl  The original declaration, which was supposed to be rewritten.
	* @param attemptedDecl In case any attempt was made to rewrite the declaration, this
	*                      option contains
	*                      it. For example, when you received a RewriteError from
	*                      [[LinkInverter.invertLink]] for a constant declaration, then
	*                      the type or definiens component might still contain references
	*                      to the theory S which was supposed to be generalized.
	*
	*                      However, in cases where the declarations wasn't known to
	*                      [[LinkInverter.invertLink]] at all (e.g. proper structures -
	*                      not inclusions), this might be [[None]].
	* @param blamableTerms In case the declaration was a constant declaration and could
	*                      not be fully rewritten due to some left over terms referring
	*                      to things in the theory S, which was supposed to be
	*                      generalized, this map contains it grouped by their occurrence
	*                      in the term's component.
	*/
sealed case class RewriteConstantError(originalDecl: Constant,
																			 attemptedDecl: Option[Declaration],
																			 blamableTerms: Map[ComponentKey, Seq[Term]])
	extends RewriteError(originalDecl, attemptedDecl) {
	override def toString: String = {
		val blamableTermsString = blamableTerms.map { case (key, terms) => (key, terms.map
		(_.toStr(shortURIs = true)))
		}.toString()

		"RewriteError(originalDecl = " + originalDecl.name + ", attemptedDecl = " +
			attemptedDecl.map(_.name) + ", blamableTerms = " + blamableTermsString + ")"
	}
}

sealed case class RewriteUnknownError(originalDecl: Declaration)
	extends RewriteError(originalDecl = originalDecl, attemptedDecl = None)

/**
	* The continuation style telling [[LinkInverter.invertLink()]] how to
	* continue after encountering a [[RewriteConstantError]].
	*
	* Apart from it being used a little bit internally for the control flow in
	* [[LinkInverter]], it usually is returned by a [[RewriteErrorHandler]] given
	* as a callback to [[LinkInverter.invertLink()]].
	*/
sealed abstract class ContinuationStyle

/**
	* Skip the failed declaration.
	*
	* @see [[ContinuationStyle]]
	*/
case object SkipDeclaration extends ContinuationStyle

/**
	* Skip the failed declaration.
	*
	* @see [[ContinuationStyle]]
	*/
case object AssumeRewritable extends ContinuationStyle

/**
	* Forcefully rewrite the declaration to a provided one.
	*
	* If you are within a [[RewriteErrorHandler]], `decl` should start with the same new
	* module path as passed to [[LinkInverter.invertLink()]].
	*
	* @see [[ContinuationStyle]]
	*/
final case class RewriteAs(decl: Declaration) extends ContinuationStyle

/**
	* An error handler for [[LinkInverter.invertLink()]] called upon
	* declarations which could not be rewritten due to a [[RewriteError]].
	* The handler decides how to continue using a [[ContinuationStyle]].
	*/
trait RewriteErrorHandler {
	def apply(error: RewriteError): ContinuationStyle
}

// TODO Replace with [[AnonymousDiagram]]
final case class LinkInverterResult(invertedTheory: List[Declaration],
																		generatedMorphism: List[Declaration])

/**
	* @todo ComFreek: Document, especially when terms are not rewritable.
	*/
object LinkInverter {
	def invertLink(R: Theory, S: Theory, RToS: Link,
								 linkInversionRuleProvider: LinkInversionRulesProvider,
								 newModulePath: MPath,
								 newMorphismPath: MPath,
								 rewriteErrorHandler: RewriteErrorHandler)
								(implicit
								 ctrl: Controller): (Theory, View) = {

		val linkInversionResult = invertLinkToAnonymous(
			R, S, RToS, linkInversionRuleProvider, newModulePath, newMorphismPath,
			rewriteErrorHandler
		)

		val invertedTheory = ModuleCreator
			.getBuilder
			// Add generalized declarations
			.addNewSymbols(linkInversionResult.invertedTheory.map(decl => {
			x: MPath => decl
		})).asNewTheory(newModulePath.parent, newModulePath.name, R.meta)

		val generatedMorphism = ModuleCreator
			.getBuilder
			.addNewSymbols(linkInversionResult.generatedMorphism.map(decl => {
				x: MPath => decl
			})).asNewView(
			newMorphismPath.parent,
			newMorphismPath.name,
			from = invertedTheory.path,
			to = S.path
		)

		(invertedTheory, generatedMorphism)
	}

	// TODO Make it return an [[AnonymousBody]]
	// TODO Rename S to T, actually we only need a link (implicitly giving us R and S)
	//  and then T
	def invertLinkToAnonymous(R: Theory, S: Theory, RToS: Link,
														linkInversionRuleProvider: LinkInversionRulesProvider,
														newModulePath: MPath,
														newMorphismPath: MPath,
														rewriteErrorHandler: RewriteErrorHandler)
													 (implicit
														ctrl: Controller)
	: LinkInverterResult
	= {
		assert(R.meta == S.meta)

		assert(RToS.from.toMPath == R.path, "Passed RToS link does not have R as its domain.")

		// TODO
		assert(
			S.path == RToS.to.toMPath ||
				S.getDeclarations.exists({
					case decl: Structure if decl.from.toMPath == RToS.to.toMPath => true
					case _ => false
				}), "Passed theory 'S' (quote-unquote!) must either be S itself or a theory " +
				"including S. We might support theories where an implicit morphism exists from " +
				"S as well. Contact me, Navid Roux, in that case, so I can have a look.")

		// The initial context in which are rewriting
		// It does not encompass already rewritten declarations when
		// we are in the course of doing so.
		val globalContext = Context(RToS.to.toMPath) ::: Context(S.path)

		/*// Simplest case: match only with consideration of rules within the
		// globalContext, most importantly, this will probably encompass
		// LF application rules!
		//
		// More advanced maybe in the future: consider S-theorems
		val matcher = new Matcher(ctrl, RuleSet.collectRules(ctrl, globalContext))*/

		// The inversion rules which we will gradually build up while
		// inverting
		val inversionRules = new MutableRuleSet
		inversionRules.add(
			linkInversionRuleProvider.getInverseRewritingRules(RToS).getAll.toSeq: _*
		)

		val emptySimplificationUnit = SimplificationUnit(
			globalContext,
			// Do not expand OMIDs in declarations. But we still rewrite them
			// using [[AbbrevRule AbbrevRules]].
			expandConDefs = false,
			expandVarDefs =false,
			fullRecursion = true
		)

		def rewrite(term: Term): Term = {
			// TODO Shouldn't we add already rewritten declarations to the context?
			// TODO Maybe it already works as-is because they are in the globalContext with
			// TODO the same name?
			ctrl.simplifier(term, emptySimplificationUnit, inversionRules)
		}

		val newDeclarations = new mutable.ArrayBuffer[Declaration]
		val outLinkDeclarations = new mutable.ArrayBuffer[Declaration]

		val allowedModuleReferences = new mutable.HashSet[MPath]()
		allowedModuleReferences += newModulePath

		// Start inversion
		// Add inclusion of R to generated theory
		newDeclarations += Include(home = OMID(newModulePath), from = R.path, args = Nil)
		outLinkDeclarations += Include(
			home = OMID(newMorphismPath),
			from = R.path,
			args = Nil,
			df = Some(RToS.toTerm)
		)

		// Now try inverting all declarations in T
		// getConstants returns the constants in narrative (i.e. dependency-conforming) order
		S.getDeclarations.foreach(originalDecl => {
			// If RToS contains a renaming originalDecl' -> originalDecl, then use the local
			// name of originalDecl' as the new name. Otherwise, keep the name of originalDecl.
			// This is precisely what [[rewrite]] does under the condition that
			// expandDefinitions is set to false in the simplification unit.
			val newName = rewrite(originalDecl.toTerm) match {
				case OMID(GlobalName(_, newLocalName)) => newLocalName
				case _ => throw new AssertionError("Simplifier rewrote OMID to a GlobalName " +
					"to a non-OMID or to an OMID not referring to a GlobalName anymore.")
			}

			originalDecl match {
				case includeDecl: Structure if includeDecl.isInclude =>
					if (includeDecl.from.toMPath == RToS.to.toMPath) {
						// Ignore include of S in T
						// The very sense of inversion/generalization is that
						// we want to replace the inclusion of S by an inclusion of R

						// The inclusion of R has already been added above,
						// likewise the inclusion of RToS to the generated morphism.
					}
					else {
						// Copy the inclusion to the generated theory and morphism
						// but with adjusted home terms, of course.
						newDeclarations.append(new Structure(
							OMID(newModulePath),
							includeDecl.name,
							includeDecl.tpC.copy,
							includeDecl.dfC.copy,
							isImplicit = includeDecl.isImplicit,
							isTotal = includeDecl.isTotal
						))

						outLinkDeclarations.append(new Structure(
							OMID(newMorphismPath),
							includeDecl.name,
							includeDecl.tpC.copy,
							includeDecl.dfC.copy,
							isImplicit = includeDecl.isImplicit,
							isTotal = includeDecl.isTotal
						))

						// Allow OMIDs referencing symbols in the included theory
						// or any of its transitively included theories later on.
						allowedModuleReferences += includeDecl.from.toMPath
					}
				case originalConstant: Constant =>
					val newTypeContainer = originalConstant.tpC.map(rewrite)
					val newDefContainer = originalConstant.dfC.map(rewrite)

					val attemptedDeclaration = new FinalConstant(
						home = OMID(newModulePath),
						name = newName,
						alias = originalConstant.alias,
						tpC = newTypeContainer,
						dfC = newDefContainer,
						rl = originalConstant.rl,
						notC = originalConstant.notC.copy,
						vs = originalConstant.vs
					)

					def getUnrewritableTerms(term: Term): Seq[Term] = {
						new UnresolvablePathCollector(
							ctrl.library,
							R.path,
							allowedModuleReferences.toSet
						).collectUnresolvableTerms(term)
					}

					val invalidTypeSubterms = attemptedDeclaration.tp.map(getUnrewritableTerms).getOrElse(Nil)
					val invalidDefSubterms = attemptedDeclaration.df.map(getUnrewritableTerms).getOrElse(Nil)

					// Determine how to continue (in error or in success)
					val continuationStyle: ContinuationStyle = {
						if (invalidTypeSubterms.nonEmpty || invalidDefSubterms.nonEmpty) {
							rewriteErrorHandler(RewriteConstantError(originalConstant, Some(attemptedDeclaration), Map(
								TypeComponent -> invalidTypeSubterms,
								DefComponent -> invalidDefSubterms
							)))
						}
						else {
							// We actually succeeded in rewriting
							RewriteAs(attemptedDeclaration)
						}
					}

					continuationStyle match {
						case AssumeRewritable =>
							inversionRules.add(new AbbrevRule(
								originalConstant.toTerm.path.asInstanceOf[GlobalName],
								attemptedDeclaration.toTerm
							))
						case RewriteAs(newDeclaration: Constant) =>
							inversionRules.add(new AbbrevRule(
								originalConstant.toTerm.path.asInstanceOf[GlobalName],
								newDeclaration.toTerm
							))
							newDeclarations.append(newDeclaration)

							outLinkDeclarations.append(Constant(
								home = OMID(newModulePath),
								name = newDeclaration.name,
								alias = Nil,
								tp = newDeclaration.tp,
								df = Some(originalConstant.toTerm),
								rl = None
							))

						case RewriteAs(_) => throw new AssertionError("The RewriteErrorHandler passed to LinkInverter rewrote a constant " +
							"declaration to a declaration which is not a (subclass of) constant " +
							"anymore. It isn't clear how the generated morphism should account for " +
							"that."
						)

						case SkipDeclaration => /* Do nothing */
					}
				case unknownDecl =>
					rewriteErrorHandler(RewriteUnknownError(unknownDecl)) match {
						case AssumeRewritable =>
							inversionRules.add(new AbbrevRule(
								unknownDecl.toTerm.path.asInstanceOf[GlobalName],
								OMID(newModulePath ? newName)
							))
						case RewriteAs(newDecl) =>
							newDeclarations.append(newDecl)

							inversionRules.add(new AbbrevRule(
								unknownDecl.toTerm.path.asInstanceOf[GlobalName],
								newDecl.toTerm
							))
						case SkipDeclaration => /* Do nothing */
					}
			}
		})

		LinkInverterResult(
			invertedTheory = newDeclarations.toList,
			generatedMorphism = outLinkDeclarations.toList
		)
	}
}
