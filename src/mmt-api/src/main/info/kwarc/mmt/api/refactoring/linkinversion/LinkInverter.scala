package info.kwarc.mmt.api.refactoring.linkinversion

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Link, ModuleCreator, Theory}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.{HasMeta, RelStore, RelationExp}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom.{AbbrevRule, SimplificationUnit}

import scala.collection.mutable

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
trait LinkInversionRulesProvider {
	/**
		* Provide rewrite rules for the inverted link.
		*
		* @see [[LinkInversionRulesProvider]] for more documentation.
		*
		* @param link The link.
		* @return Rewrite rules.
		*/
	def getInverseRewritingRules(link: Link): RuleSet
}

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
sealed case class RewriteError(originalDecl: Declaration,
															 attemptedDecl: Option[Declaration],
															 blamableTerms: Map[ComponentKey, Seq[Term]]) {
	override def toString: String = {
		val blamableTermsString = blamableTerms.map { case (key, terms) => (key, terms.map
		(_.toStr(shortURIs = true)))
		}.toString()

		"RewriteError(originalDecl = " + originalDecl.name + ", attemptedDecl = " +
			attemptedDecl.map(_.name) + ", blamableTerms = " + blamableTermsString + ")"
	}
}

/**
	* The continuation style telling [[LinkInverter.invertLink()]] how to
	* continue after encountering a [[RewriteError]].
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

object LinkInverter {
	def invertLink(R: Theory, S: Theory, RToS: Link,
								 linkInversionRuleProvider: LinkInversionRulesProvider,
								 newModulePath: MPath,
								 rewriteErrorHandler: RewriteErrorHandler)
								(implicit
								 ctrl: Controller): Theory = {

		val invertedDeclarations = invertLinkToAnonymous(
			R, S, RToS, linkInversionRuleProvider, newModulePath, rewriteErrorHandler
		)

		val invertedTheory = ModuleCreator
			.getBuilder
			// Include R
			.addNewSymbol(targetPath => Include(OMMOD(targetPath), R.path, Nil))
			// Add generalized declarations
			.addNewSymbols(invertedDeclarations.map(decl => {
			x: MPath => decl
		})).asNewTheory(newModulePath.parent, newModulePath.name, R.meta)

		invertedTheory
	}

	// TODO Make it return an [[AnonymousBody]]
	// TODO Rename S to T, actually we only need a link (implicitly giving us R and S)
	//  and then T
	def invertLinkToAnonymous(R: Theory, S: Theory, RToS: Link,
														linkInversionRuleProvider: LinkInversionRulesProvider,
														newModulePath: MPath,
														rewriteErrorHandler: RewriteErrorHandler)
													 (implicit
														ctrl: Controller)
	: List[Declaration]
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
			linkInversionRuleProvider.getInverseRewritingRules(RToS).getAll.toSeq : _*
		)

		val emptySimplificationUnit = SimplificationUnit(
			globalContext,
			// Do not expand OMIDs in declarations. But we still rewrite them
			// using [[AbbrevRule AbbrevRules]].
			expandDefinitions = false,
			fullRecursion = true
		)

		def rewrite(term: Term): Term = {
			// TODO Shouldn't we add already rewritten declarations to the context?
			// TODO Maybe it already works as-is because they are in the globalContext with
			// TODO the same name?
			ctrl.simplifier(term, emptySimplificationUnit, inversionRules)
		}

		val newDeclarations = new mutable.ArrayBuffer[Declaration]

		val allowedOMIDReferences = new mutable.HashSet[Path]()
		val allowedModuleReferences = new mutable.HashSet[MPath]()

		def getUnrewritableTerms(term: Term): Seq[Term] = {
			isTermInTheoryObjects(
				term,
				R.path,
				allowedOMIDReferences.toSet,
				allowedModuleReferences.toSet
			)(ctrl.depstore)
		}

		// getConstants returns the constants in narrative (i.e. dependency-conforming) order
		S.getDeclarations.foreach({
			case includeDecl: Structure if includeDecl.isInclude =>
				if (includeDecl.from.toMPath == RToS.to.toMPath) {
					// Ignore include of S in T
					// The very sense of inversion/generalization is that
					// we want to replace the inclusion of S by an inclusion of R
				}
				else {
					newDeclarations.append(includeDecl)

					// Allow OMIDs referencing symbols in the included theory
					// or any of its transitively included theories later on.
					allowedModuleReferences ++= ctrl.depstore.querySet(
						includeDecl.from.toMPath,
						(RelationExp.Imports | HasMeta).^*
					).map(_.asInstanceOf[MPath])
				}
			case decl: Constant =>
				// If RToS contains a renaming decl' -> decl, then use the local name of decl'
				// as the new name.
				// Otherwise, keep the name of decl.
				// This is precisely what [[rewrite]] does under the condition that
				// expandDefinitions is set to false in the simplification unit.
				val newName = rewrite(decl.toTerm) match {
					case OMID(GlobalName(_, newLocalName)) => newLocalName
					case _ => throw new AssertionError("Simplifier rewrote OMID to a GlobalName " +
						"to a non-OMID or to an OMID not referring to a GlobalName anymore.")
				}

				val newTypeContainer = decl.tpC.map(rewrite)
				val newDefContainer = decl.dfC.map(rewrite)

				val attemptedDeclaration = new FinalConstant(
					home = OMID(newModulePath),
					name = newName,
					alias = decl.alias,
					tpC = newTypeContainer,
					dfC = newDefContainer,
					rl = decl.rl,
					notC = decl.notC.copy,
					vs = decl.vs
				)

				val invalidTypeSubterms = attemptedDeclaration.tp.map(getUnrewritableTerms).getOrElse(Nil)
				val invalidDefSubterms = attemptedDeclaration.df.map(getUnrewritableTerms).getOrElse(Nil)

				// Determine how to continue (in error or in success)
				val continuationStyle: ContinuationStyle = {
					if (invalidTypeSubterms.nonEmpty || invalidDefSubterms.nonEmpty) {
						rewriteErrorHandler(RewriteError(decl, Some(attemptedDeclaration), Map(
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
							decl.toTerm.path.asInstanceOf[GlobalName],
							attemptedDeclaration.toTerm
						))
						allowedOMIDReferences += attemptedDeclaration.path
					case RewriteAs(newDeclaration) =>
						inversionRules.add(new AbbrevRule(
							decl.toTerm.path.asInstanceOf[GlobalName],
							newDeclaration.toTerm
						))
						allowedOMIDReferences += newDeclaration.path
						newDeclarations.append(newDeclaration)

					case SkipDeclaration =>
				}
			case unknownDecl =>
				rewriteErrorHandler(RewriteError(unknownDecl, None, Map())) match {
					case AssumeRewritable =>
						allowedOMIDReferences += unknownDecl.path
					case RewriteAs(decl) =>
						newDeclarations.append(decl)
						allowedOMIDReferences += unknownDecl.path
					case SkipDeclaration =>
				}
		})

		newDeclarations.toList
	}

	/**
		* Get all references (paths inside OMIDs) of a term which are neither resolvable by
		* `theory` nor `allowedFurtherReferences`.
		*
		* A reference to a path is resolvable iff.
		*
		* a) it references a symbol (i.e. the path was a [[GlobalName]]) inside `theory` or
		* any of its (transitively) *imported* (meta)theories, or
		*
		* b) it references a symbol (i.e. the path was a [[GlobalName]]) whose module path
		* is contained in `allowedFurtherModuleReferences`, or
		*
		* c) it is contained in `allowedFurtherReferences`.
		*
		* "Imported" encompasses inclusions. See [[RelationExp.Imports]] for what it
		* precisely means.
		* Precisely, in a) it is checked whether the the path references a module in
		* `(RelationExp.Imports | HasMeta)^*`.
		*
		* @param term                           The term to check. All subterms will be gone through.
		* @param theory                         The theory which specifies allowed referenced symbols within `term`.
		* @param allowedFurtherPathReferences   Whitelist of further allowed referenced paths in
		*                                       OMIDs.
		* @param allowedFurtherModuleReferences Whitelist of further allowed module paths.
		* @param depstore                       The relational store used for querying inclusion and meta
		*                                       relations, required as describes in the doccomment for
		*                                       `theory`.
		* @return A list of unresolvable references. This being Nil is a requirement for
		*         the term to be welltyped.
		*/
	private def isTermInTheoryObjects(term: Term, theory: MPath,
																		allowedFurtherPathReferences: Set[Path],
																		allowedFurtherModuleReferences: Set[MPath])(implicit
																																								depstore: RelStore)
	: Seq[Term]
	= {

		val transitiveIncludedModules: Set[Path] =
			depstore.querySet(theory, (RelationExp.Imports | HasMeta).^*).toSet

		val totalAllowedModuleReferences =
			transitiveIncludedModules ++ allowedFurtherModuleReferences

		def check(subterm: Term): Seq[Term] = subterm match {
			case OMID(GlobalName(module, _))
				if totalAllowedModuleReferences.contains(module) =>
				Nil
			case OMID(path) if allowedFurtherPathReferences.contains(path) => Nil
			case unresolvedReference: OMID => List(unresolvedReference)

			case OMV(_) => Nil
			case ComplexTerm(symbol, _, _, subterms) =>
				(OMID(symbol) :: subterms).flatMap(check)
			case UnknownOMLIT(_, synType) => check(synType)
			case OMLIT(_, realizedType) => check(realizedType.synType)
		}

		check(term)
	}

}
