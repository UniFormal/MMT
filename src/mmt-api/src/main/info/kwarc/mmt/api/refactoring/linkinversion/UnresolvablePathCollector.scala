package info.kwarc.mmt.api.refactoring.linkinversion

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.objects._

import scala.collection.mutable

/**
	* A [[Traverser]] collecting all references (paths inside OMIDs) of a term
	* whose referenced module is neither in scope of `referenceModule` nor in
	* scope by any whitelisted modules in `allowedFurtherModuleReferences`.
	*
	* A module M is in scope of M' iff. there is an implicit morphism
	* (known to `library` at least!) from M to M'.
	* E.g. this is the case if M is (transitively) included in M' or
	* also the case is M = M'.
	*
	* Thus this traverser can be used to check whether a term is a
	* ({referenceModule} \cup allowedFurtherModuleReferences)-expression.
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

	override def traverse(term: Term)(implicit con: Context, unresolvableOMIDs: State): Term
	= {
		term match {
			case omid: OMID =>
				if (library.hasImplicit(OMMOD(omid.path.module), OMMOD(referenceModule))) {
					// Fine
				}
				else if (allowedFurtherModuleReferences.exists(furtherModule =>
					library.hasImplicit(OMMOD(omid.path.module), OMMOD(furtherModule))
				)) {
					// Fine
				}
				else {
					// Unresolvable
					unresolvableOMIDs += omid
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
		* @return A sequence of unresolvable references. This being Nil is a
		*         requirement for the term to be welltyped (at least with the implicit
		*         morphisms known to `library` as passed to the constructor
		*         [[UnresolvablePathCollector()]])
		*/
	def collectUnresolvableTerms(term: Term): Seq[OMID] = {
		val unresolvableTerms = new mutable.ArrayBuffer[OMID]
		this (term, unresolvableTerms)

		unresolvableTerms.toSeq
	}
}