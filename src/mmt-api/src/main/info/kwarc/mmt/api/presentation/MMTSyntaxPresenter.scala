package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents.{DRef, Document, InterpretationInstruction, MRef}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.opaque.{OpaqueElement, OpaqueTextPresenter}
import info.kwarc.mmt.api.parser.Reader
import info.kwarc.mmt.api.symbols._

/**
	* Presenter writing out parsable MMT surface syntax.
	*
	* This class supersedes the now deleted class MMTStructurePresenter.
	* Previously was just a minimally modifie copy of MMTStructurePresenter
	* now been deleted.
	*
	* TODO Document that the element has to be added to the controller before.
	*/

class MMTSyntaxPresenter(objectPresenter: ObjectPresenter = new NotationBasedPresenter) extends Presenter(objectPresenter) {

	/** determines if the modules should be flattened */
	val presentGenerated: Boolean = false

	def key = "present-text-notations" + (if (presentGenerated) "-flat" else "")

	override def outExt = "mmt"

	private val OBJECT_DELIMITER = "❘"
	private val DECLARATION_DELIMITER = "❙"
	private val MODULE_DELIMITER = "❚"

	/**
		* Present an element such as a [[Theory]], a [[View]] or a [[Declaration]].
		*
		* @param element    The element to present.
		* @param standalone if true, include appropriate header and footer
		* @param rh         output stream
		*/
	override def apply(element: StructuralElement, standalone: Boolean = false)(implicit
																																							rh: RenderingHandler) {
		controller.simplifier(element) //TODO simplifying here is bad for elements that are not part of the diagram yet
		present(element, rh)
	}

	/**
		* Get a wrapping rendering handler indenting every line it is passed to.
		*
		* @param rh          The original rendering handler to delegate to.
		* @param indentation The number of indentation levels. E.g. 1 corresponds to two
		*                    spaces, 2 to four spaces and so on.
		* @return A rendering handler which will indent the first string you pass it to
		*         with the given indentation and replace every \n by the same indentation.
		*/
	private def indented(rh: RenderingHandler, indentation: Int = 1): RenderingHandler = {
		// TODO Prefer tabs?
		val indentationString = "  " * indentation

		var firstCall = true

		str: String => {
			if (firstCall && str.nonEmpty) {
				rh.write(indentationString)
				firstCall = false
			}
			rh.write(str.replace("\n", "\n" + indentationString))
		}
	}

	private def beginDecl(element: StructuralElement, rh: RenderingHandler): Unit = {
		// empty so far
	}

	private def present(element: StructuralElement, rh: RenderingHandler): Unit = {
		beginDecl(element, rh)
		element match {
			//TODO delimiters, metadata
			case d: Document =>
				rh("document " + d.path.toPath + "\n")
				d.getDeclarations foreach { decl => present(decl, indented(rh)) }
			case r: DRef =>
				rh("document " + r.target.toPath)
			case r: MRef =>
				controller.getO(r.target) match {
					case None => rh("module " + r.target.toPath)
					case Some(m) => present(m, rh)
				}
			case oe: OpaqueElement =>
				controller.extman.get(classOf[OpaqueTextPresenter], oe.format)
			case ii: InterpretationInstruction =>
				rh(ii.toString)
			case c: Constant => doConstant(c, rh)
			case t: Theory => doTheory(t, rh)
			case v: View => doView(v, rh)
			case dd: DerivedDeclaration =>
				rh << dd.feature + " "
				controller.extman.get(classOf[StructuralFeature], dd.feature) match {
					case None => rh << dd.name + " (implementation is not known)"
					case Some(sf) =>
						val header = sf.makeHeader(dd)
						apply(header, Some(dd.path $ TypeComponent))(rh)
				}
				rh << "\n"
				dd.module.getDeclarations.foreach { d => present(d, indented(rh)) }
			case dm: DerivedModule =>
				doTheory(dm, indented(rh))
			case nm: NestedModule =>
				present(nm.module, rh)
			case s: Structure => doStructure(s, rh)
			case r: RuleConstant =>
				if (r.df.isEmpty) rh("unknown ")
				rh("rule ")
				r.tp foreach { t => apply(t, Some(r.path $ TypeComponent))(rh) }
		}
		endDecl(element, rh)
		rh("\n")
	}

	private def endDecl(element: StructuralElement, rh: RenderingHandler): Unit = element match {
		case _: Document =>
		case _: DRef => rh(Reader.GS.toChar.toString) // check?
		case _: MRef => rh(Reader.GS.toChar.toString) // check?
		case _: ModuleOrLink => rh(MODULE_DELIMITER)
		case _: NestedModule => rh(DECLARATION_DELIMITER)

		// some declarations are handled before by ModuleOrLink already
		case _: Declaration => rh(DECLARATION_DELIMITER)
	}

	private def doTheory(theory: AbstractTheory, rh: RenderingHandler): Unit = {
		//TODO this ignores all narrative structure inside a theory
		rh(theory.feature + " " + theory.name)
		theory.meta.foreach(metaTheoryPath => rh(" : " + metaTheoryPath.toString))
		// TODO print type component
		// TODO What is a def component of a theory?
		doDefComponent(theory, rh)
		rh(" =")
		rh(" \n")
		theory.getDeclarations.foreach { d =>
			if (!d.isGenerated || presentGenerated) present(d, indented(rh))
		}
	}

	private def doConstant(c: Constant, rh: RenderingHandler): Unit = {
		rh(c.name.last.toString)

		val hadAlias = c.alias.nonEmpty
		c.alias foreach { a =>
			rh(" @ ")
			rh(a.toPath)
		}

		val hadTypeComponent = c.tp.isDefined
		c.tp foreach { typeTerm =>
			if (hadAlias) {
				rh(OBJECT_DELIMITER)
			}

			rh("\n")
			indented(rh)("  : ")
			apply(typeTerm, Some(c.path $ TypeComponent))(rh)
		}

		val hadDefiniensComponent = c.df.isDefined
		c.df foreach { definiensTerm =>
			if (hadTypeComponent) {
				rh(OBJECT_DELIMITER)
			}

			rh("\n")
			indented(rh)("  = ")
			apply(definiensTerm, Some(c.path $ DefComponent))(rh)
		}

		c.notC.parsing foreach { textNotation =>
			if (hadDefiniensComponent) {
				rh(OBJECT_DELIMITER)
			}

			rh("\n")
			indented(rh)("  # ")
			rh(textNotation.toText)
		}
	}

	private def doView(view: View, rh: RenderingHandler): Unit = {
		rh("view " + view.name + " : ")
		apply(view.from, Some(view.path $ DomComponent))(rh)
		rh(" -> ")
		apply(view.to, Some(view.path $ CodComponent))(rh)
		doDefComponent(view, rh)
		view.getPrimitiveDeclarations.foreach {
			case c: Constant =>
				indented(rh)("" + c.name.last)
				c.df foreach { t =>
					rh("  = ")
					apply(t, Some(c.path $ DefComponent))(rh)
					rh(DECLARATION_DELIMITER + "\n")
				}
			case d => present(d, indented(rh))
		}
	}

	private def doStructure(s: Structure, rh: RenderingHandler): Unit = {
		val decs = s.getPrimitiveDeclarations
		if (decs.isEmpty) {
			rh("include ")
			apply(s.from, Some(s.path $ TypeComponent))(rh)
		} else {
			rh("structure " + s.name + " : " + s.from.toMPath.^^.last + "?" + s.from.toMPath.last)
			//this.present(s.from, Some(s.path $ TypeComponent))
			rh(Reader.US.toChar.toString)
			doDefComponent(s, rh)
			decs.foreach { d => present(d, indented(rh)) }
		}
	}

	/** `= df` if df is preset, returns true if there was a df */
	private def doDefComponent(m: ModuleOrLink, rh: RenderingHandler): Boolean
	= {
		m.df match {
			case Some(df) =>
				rh(" = ")
				apply(df, Some(m.path $ DefComponent))(rh)
				true
			case None =>
				false
		}
	}
}

/** flattened */
class FlatMMTSyntaxPresenter(oP: ObjectPresenter) extends MMTSyntaxPresenter {
	override val presentGenerated = true
}
