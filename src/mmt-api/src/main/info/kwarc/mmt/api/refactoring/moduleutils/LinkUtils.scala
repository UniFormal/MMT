package info.kwarc.mmt.api.refactoring.moduleutils

import info.kwarc.mmt.api.modules.Link
import info.kwarc.mmt.api.{ComplexStep, GlobalName, LocalName}

object LinkUtils {
	def getLinkDomainPaths(link: Link): List[GlobalName] = {
		link.getDeclarations.map(decl => getDomainPathFromLinkDeclarationPath(decl.path))
	}

	def getDomainPathFromLinkDeclarationPath(decl: GlobalName): GlobalName = decl.name match {
		case LocalName(List(ComplexStep(domainMPath), actualLocalName)) => domainMPath ?
			actualLocalName
		case _ => throw new AssertionError("Link contained a declaration which did not " +
			"start with a ComplexStep as was once promised in API documentation of " +
			"[[Link]]")
	}
}
