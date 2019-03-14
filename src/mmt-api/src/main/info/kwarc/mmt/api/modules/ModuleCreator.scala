package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.{OMMOD, Term}
import info.kwarc.mmt.api.symbols.Declaration

object ModuleCreator {

	def getBuilder: Builder = new Builder()

	final class Builder private (collectedDeclarations: List[MPath =>
		List[Declaration]] = List(),
										 collectedSubstitutions: Map[ContentPath,
											 LocalName] = Map()) {

		def this() = this(List(), Map())

		def addNewSymbol(declaration: MPath => Declaration): Builder = {
			val newCollectedDeclaration: MPath => List[Declaration] = declaration.andThen(d => List(d))

			new Builder(collectedDeclarations ++ List(newCollectedDeclaration),
				collectedSubstitutions)
		}

		def addNewSymbols(declarations: List[MPath => Declaration]): Builder = {
			declarations.foldLeft(this)((currentBuilder, newDecl) => {
				currentBuilder.addNewSymbol(newDecl)
			})
		}

		def addFromExistingModule(module: Module): Builder = {
			addFromExistingSymbols(module.getDeclarations)
		}

		def addFromExistingSymbols(declarations: List[Declaration]): Builder = {
			if (declarations.isEmpty) {
				return this
			}

			val sourceModule = declarations.head.parent
			assert(declarations.forall(decl => decl.parent == sourceModule), "All " +
				"declarations must come from the same source module, i.e. have the same MPath " +
				"in their GlobalName path.")

			val substitutions: Map[GlobalName, LocalName] = declarations.map(decl => {
				(decl.path, decl.name)
			}).toMap

			// TODO Assert that substitutions don't overlap?
			new Builder(collectedDeclarations ++ List((_: MPath) => declarations),
				collectedSubstitutions ++
					substitutions)
		}

		def asNewTheory(documentPath: DPath, theoryName: LocalName,
										metaTheory: Option[MPath])
		: Theory = {
			val newTheory = Theory.empty(documentPath, theoryName, metaTheory)
			appendTo(newTheory)
			newTheory
		}


		def asNewView(documentPath: DPath, viewName: LocalName, from: MPath, to: MPath,
									isImplicit: Boolean = false)
		: View = {
			asNewView(documentPath, viewName, OMMOD(from), OMMOD(to), isImplicit)
		}

		def asNewView(documentPath: DPath, viewName: LocalName, from: Term, to: Term,
									isImplicit: Boolean): View
		= {
			val newView = View(documentPath, viewName, from, to, isImplicit = isImplicit)
			appendTo(newView)
			newView
		}

		def appendTo(module: Module): Unit = {
			// Realize substitutions by inserting the new module path
			val realizedSubstitutions = collectedSubstitutions.map { case (needleContentPath,
			replacementLocalName) => (needleContentPath, module.path ? replacementLocalName)
			}

			collectedDeclarations
				.flatMap(decl => decl(module.path))
				.map(ReferenceSubstituter.substituteDeclaration(_, realizedSubstitutions))
				.foreach { decl => module.add(decl, AtEnd) }
		}
	}

}
