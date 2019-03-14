package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils.URI

object LinkUtils {
	def getDefiniensTerm(decl: Declaration): Term = {
		val defComponent = decl.getComponent(DefComponent).getOrElse(throw new AssertionError(
			"The declaration '" + decl.path + "' inside its link had no definiens component."
		))

		defComponent match {
			case termContainer: AbstractTermContainer =>
				termContainer.get.getOrElse(throw new AssertionError("The declaration " +
					"'" + decl
					.path + "' had a definiens, but the associated AbstractTermContainer " +
					"contained no term."))

			case _ => throw new AssertionError("The declaration '" + decl.path + "' had a" +
				" definiens " +
				"component, but it was not an AbstractTermContainer. How should this method " +
				"handle this? When does this occur?")
		}
	}

	def getLinkDomainPaths(link: Link): List[GlobalName] = {
		link.getDeclarations.map(decl => getDomainPathFromLinkDeclarationPath(decl.path))
	}

	/**
		* For a declaration of a link get the GlobalName to the symbol being mapped.
		*
		* For flat theories and flat links (views), this is a GlobalName referring to
		* a symbol within the domain theory.
		* However, if the domain theory includes another theory, S, then the view might as
		* well map symbols from S to something. Hence, the returned GlobalName can also refer
		* to a symbol from a (transitively) included theory.
		*
		* @param decl A declaration of a link.
		* @return The GlobalName referring to the declaration being mapped.
		* @throws AssertionError If the declaration's local name is not of the form
		*                        LocalName(List(ComplexStep(mpath), ...)).
		*                        All link declarations start with a ComplexStep in MMT.
		*/
	def getDomainPathFromLinkDeclarationPath(decl: GlobalName): GlobalName = decl.name match {
		case LocalName(List(ComplexStep(domainMPath), actualLocalName)) => domainMPath ?
			actualLocalName
		case _ => throw new AssertionError("Link contained a declaration which did not " +
			"start with a ComplexStep as was once promised in API documentation of " +
			"[[Link]]")
	}

	/**
		* For a link get the mapping of the declarations.
		*
		* For example, if the link contains the assignment (c = d), then the mapping will
		* contain OMID(path to c) -> d, where d might be an arbitrary term.
		*
		* @param link The link.
		* @return The mapping.
		*/
	def getTermMappingForLink(link: Link)(implicit ctrl: Controller)
	: Map[GlobalName, Term] = {
		link
			.getDeclarations
			.map(linkDecl => {
				(LinkUtils.getDomainPathFromLinkDeclarationPath(linkDecl.path),
					LinkUtils.getDefiniensTerm(linkDecl))
			})
			.toMap
	}

	/**
		* For an injective link get the inverse term mapping. It is just the inverted map from
		* [[LinkUtils.getTermMappingForLink()]].
		*
		* A link is injective :<=> for every two declarations of it the assigned terms are
		* *not* equal (as in terms of the MMT Scala API).
		*
		* @return The inverse mapping.
		* @throws AssertionError It asserts that the link is injective.
		*/
	def getInverseTermMappingForInjectiveLink(link: Link)(implicit ctrl: Controller)
	: Map[Term, Term] = {
		val forwardMapping = getTermMappingForLink(link)

		assert(forwardMapping.values.toSet.size == forwardMapping.size, "Link is not " +
			"injective!")

		forwardMapping.map(_.swap)

		??? // now changed forwardMapping, adjust!
	}


}
