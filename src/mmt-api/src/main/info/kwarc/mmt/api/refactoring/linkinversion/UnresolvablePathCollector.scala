package info.kwarc.mmt.api.refactoring.linkinversion

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.objects._

import scala.collection.mutable

/**
	* A [[Traverser]] collecting all references (paths inside OMIDs) of a term
	* whose referenced module is neither in scope of `referenceModule` nor whitelisted by
	* `allowedFurtherModuleReferences`.
	*
	* A module is in scope of `referenceModule` iff. there is an implicit morphism
	* (known to `library` at least) to `referenceModule`.
	* E.g. this is the case if the module is (transitively) included or even
	* `referenceModule` itself.
	*
	* @param library                        The library which is queried for implicit morphisms.
	* @param referenceModule                The reference module as described above.
	* @param allowedFurtherModuleReferences Whitelist of further allowed module paths.
	*/
private class UnresolvablePathCollector(library: Library, referenceModule: MPath,
																				allowedFurtherModuleReferences: Set[MPath])
	extends
		Traverser[mutable.ArrayBuffer[OMID]] {
	override type State = mutable.ArrayBuffer[OMID]

	override def traverse(term: Term)(implicit con: Context, state: State): Term = {
		term match {
			case omid: OMID =>
				if (!allowedFurtherModuleReferences.contains(omid.path.module) &&
					!library.hasImplicit(OMMOD(omid.path.module), OMMOD(referenceModule))) {
					state += omid
				}
			case _ => Traverser(this, term)
		}

		term
	}

	/**
		* Get unresolvable OMID terms as described in the docs of
		* [[UnresolvablePathCollector]].
		*
		* @param term The term to check. All subterms will be gone through.
		*
		* @return A sequence of unresolvable references. This being Nil is a
		*         requirement for the term to be welltyped (at least with the implicit
		*         morphisms known to `library` as passed to the constructor
		*         [[UnresolvablePathCollector()]])
		*/
	def collectUnresolvableTerms(term: Term): Seq[OMID] = {
		val unresolvableTerms = new mutable.ArrayBuffer[OMID]
		this (term, unresolvableTerms)

		unresolvableTerms
	}
}