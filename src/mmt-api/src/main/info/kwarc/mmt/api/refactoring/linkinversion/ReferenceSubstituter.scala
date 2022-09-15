package info.kwarc.mmt.api.refactoring.linkinversion

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.utils.MMT_TODO
import info.kwarc.mmt.api.{ContentPath, GlobalName}

@deprecated("MMT_TODO: Directly use info.kwarc.mmt.api.symbols.Renamer", since="forever")// since "2019-03-14"
object ReferenceSubstituter {

	/**
		* Substitute references to given [[GlobalName GlobalName paths]] in
		* `victim` by
		* given replacement [[GlobalName GlobalName paths]].
		*
		* The substitutions will only be applied once, e.g. if you have `Map(a -> b, b -> c)`
		* and your term contains `f(a, b)`, then the resulting term is `f(b, c)` - not `f
		* (c, c)`.
		*
		* @param victim        The term whose references - after copying - will be replaced.
		* @param substitutions Substitutions of the form `keyGlobalName ->
		*                      replacementGlobalName`.
		* @return The substituted term. The types [[OMV]], [[OMID]], [[OMBINDC]], [[OMA]]
		*         and [[OMATTR]] are preserved, i.e. if victim was of one of these types,
		*         then the returned term is also of the *same* type.
		*/
	// TODO Use Traverser once it is fixed to also traverse into key, see TODO in MMT source
	def substitute(victim: Term, substitutions: Map[ContentPath, ContentPath])
	: Term = {
		assert(substitutions.forall { case (x, y) => x.getClass == y.getClass }, "You must " +
			"not substitute a ContentPath by a differently typed ContentPath, e.g. a " +
			"GlobalName path by an MPath.")

		return Renamer(name => substitutions.get(name).map(_.asInstanceOf[GlobalName]))
			.apply(victim, Context())

		/*val recurse = substitute(_, substitutions)

		victim match {
			case variableReference: OMV => variableReference

			// Reference to a symbol or module
			case OMID(contentPath) => OMID(substitutions.getOrElse(contentPath, contentPath))

			// Recursively replace references within binders (e.g. for `∀[x: Nat] t` go down
			// to t)
			case OMBINDC(binder, context, scopes) => OMBINDC(recurse(binder), context,
				scopes.map(recurse))

			// Function applications: t s_1 ... s_n --> t' s_1' ... s_n'
			case OMA(functionTerm, arguments) => OMA(substitute(functionTerm, substitutions),
				arguments.map(recurse))

			case OMATTR(arg, key, value) => OMATTR(recurse(arg), recurse(key)
				.asInstanceOf[OMID], recurse(value))

			case OML(name, tp, df, nt, featureOpt) => OML(name, tp.map(recurse), df.map
			(recurse), nt, featureOpt)

			case OMLIT(value, RealizedType(synType, semType)) => OMLIT(value, RealizedType
			(recurse(synType), semType))

			case UnknownOMLIT(valueString, synType) => UnknownOMLIT(valueString,
				recurse(synType))

			case _ => ???
		}*/
	}

	def substituteDeclaration(decl: Declaration, substitutions: Map[ContentPath,
		ContentPath]): Declaration = {

		val renamer = Renamer(name => substitutions.get(name).map(_
			.asInstanceOf[GlobalName]))
		val translator = TraversingTranslator(renamer)

		decl.translate(translator, Context())
	}
}
