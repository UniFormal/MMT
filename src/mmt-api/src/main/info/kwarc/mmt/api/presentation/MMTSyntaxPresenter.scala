package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects.{OMID, OMMOD, OMPMOD, Term}
import info.kwarc.mmt.api.opaque.{OpaqueElement, OpaqueText, OpaqueTextPresenter}
import info.kwarc.mmt.api.parser.Reader
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.URI

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

	def key: String = "present-text-notations" + (if (presentGenerated) "-flat" else "")

	override def outExt = "mmt"

	private val OBJECT_DELIMITER = "❘"
	private val DECLARATION_DELIMITER = "❙"
	private val MODULE_DELIMITER = "❚"
	private val EOL = "\n"

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
		present(element, rh)(new PersistentNamespaceMap)
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

		var isAtStartOfLine = true

		str: String => {
			if (isAtStartOfLine && str.nonEmpty) {
				rh.write(indentationString)
				isAtStartOfLine = false
			}

			// Replace inner EOLs by EOL+indentationString
			val indentedStr = {
				if (str.endsWith(EOL)) {
					// TODO Probably bad for Unicode EOL (which we don't have anyway, though)
					str.dropRight(EOL.length).replace(EOL, EOL + indentationString) + EOL
				}
				else {
					str.replace(EOL, EOL + indentationString)
				}
			}

			rh.write(indentedStr)
		}
	}

	private def beginDecl(element: StructuralElement, rh: RenderingHandler): Unit = {
		// empty so far
	}

	class PersistentNamespaceMap {
		private var nsm = controller.getNamespaceMap
		def base(s: String) = nsm = nsm.base(s)
		def add(s : String,uri : URI) = nsm = nsm.add(s,uri)
		def compact(s : String) = nsm.compact(s)
	}

	private def present(element: StructuralElement, rh: RenderingHandler)(implicit nsm : PersistentNamespaceMap): Unit = {
		beginDecl(element, rh)
		element match {
			//TODO delimiters, metadata
			case d: Document =>
				// rh("document " + d.path.toPath + "\n")
				d.getDeclarations foreach { decl => present(decl, rh) }
			case r: DRef =>
				rh("document " + r.target.toPath)
			case r: MRef =>
				controller.getO(r.target) match {
					case None => rh("module " + r.target.toPath)
					case Some(m) => present(m, rh)
				}
			case oe: OpaqueElement =>
				rh("\n/T ")
				val pres = controller.extman.get(classOf[OpaqueTextPresenter], oe.format)
				pres.get.toString(objectPresenter,oe)(rh)
			case ii: InterpretationInstruction =>
				rh(ii.toString)
				ii match {
					case Namespace(_,ns) => nsm.base(ns.toString)
					case NamespaceImport(_,pr,ns) => nsm.add(pr,ns.uri)
					case _ =>
				}
			case c: Constant => doConstant(c, rh)
			case t: Theory =>
				rh("\n")
				doTheory(t, rh)
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
				r.tp.foreach(doURI(_,rh))
				// r.tp foreach { t => apply(t, Some(r.path $ TypeComponent))(rh) }
		}
		endDecl(element, rh)
	}

	private def endDecl(element: StructuralElement, rh: RenderingHandler): Unit = element match {
		case _: Document => /* do nothing */
		case _: DRef => rh(MODULE_DELIMITER)
		case _: MRef => rh(MODULE_DELIMITER)
		case s: Structure if s.isInclude => rh(DECLARATION_DELIMITER + "\n")

		// TODO Fix for [[Structure.isInclude]] not accounting for inclusions
		//  with definiens component, see tod*o note in [[Include.unapply]]
		case s: Structure if s.getPrimitiveDeclarations.isEmpty => rh(DECLARATION_DELIMITER + "\n")
		case _: ModuleOrLink => rh(MODULE_DELIMITER)
		case _: NestedModule => rh(DECLARATION_DELIMITER)

		// some declarations are handled before by ModuleOrLink already
		case _: Declaration => rh(DECLARATION_DELIMITER + "\n")
		case ns : InterpretationInstruction =>
			rh(MODULE_DELIMITER + "\n")
		case t : OpaqueText =>
			val del = if (t.parent.toString.endsWith("omdoc")) MODULE_DELIMITER else DECLARATION_DELIMITER
			// rh("/t ")
			// t.text.toString(objectPresenter)(rh,OpaqueText.defaultEscapes)
			rh(del+"\n")
	}

	private def doTheory(theory: AbstractTheory, rh: RenderingHandler)(implicit nsm : PersistentNamespaceMap): Unit = {
		//TODO this ignores all narrative structure inside a theory
		rh(theory.feature + " " + theory.name)

		theory.meta.foreach(metaTheoryPath => {
			rh(" : ")
			doURI(OMMOD(metaTheoryPath),rh)
		})
		// TODO print type component
		// TODO What is a def component of a theory?
		doDefComponent(theory, rh)
		rh(" =")
		rh(" \n")
		theory.getDeclarations.foreach { d =>
			if (!d.isGenerated || presentGenerated) {
				// rh("\n")
				present(d, indented(rh))
			}
		}
	}

	/**
		* Present a URI, possibly relative with the given namespace map.
		*/
	private def doURI(tm: Term, rh: RenderingHandler)(implicit nsm : PersistentNamespaceMap): Unit = tm match {
		case OMPMOD(p,args) =>
			rh(nsm.compact(p.toString))
			if (args.nonEmpty) {
				rh(" (")
				args.foreach(o => { apply(o,None)(rh);rh(" ") })
				rh(")")
			}
		case _ =>
			objectLevel.apply(tm,None)(rh)
	}

	private def doConstant(c: Constant, rh: RenderingHandler)(implicit nsm : PersistentNamespaceMap): Unit = {
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
			if (hadTypeComponent || hadAlias) {
				rh(OBJECT_DELIMITER)
			}

			rh("\n")
			indented(rh)("  = ")
			apply(definiensTerm, Some(c.path $ DefComponent))(rh)
		}

		c.notC.parsing foreach { textNotation =>
			if (hadDefiniensComponent || hadTypeComponent || hadAlias) {
				rh(OBJECT_DELIMITER)
			}

			rh("\n")
			indented(rh)("  # ")
			rh(textNotation.toText)
		}
	}

	private def doView(view: View, rh: RenderingHandler)(implicit nsm : PersistentNamespaceMap): Unit = {
		rh("view " + view.name + " : ")
		doURI(view.from, rh)
		rh(" -> ")
		doURI(view.to,rh)
		rh(" =\n")

		// TODO doDefComponent does nothing for views?
		doDefComponent(view, rh)
		view.getPrimitiveDeclarations.foreach {
			case c: Constant =>
				indented(rh)(c.name.last.toString)
				c.df foreach { t =>
					rh(" = ")
					apply(t, Some(c.path $ DefComponent))(rh)
					rh(DECLARATION_DELIMITER + "\n")
				}
			case d => present(d, indented(rh))
		}
	}

	private def doStructure(s: Structure, rh: RenderingHandler)(implicit nsm : PersistentNamespaceMap): Unit = {
		val decs = s.getPrimitiveDeclarations
		if (decs.isEmpty) {
			rh("include ")

			// In views "include" declarations carry a definiens component.
			// As an example, consider the view v from a theory T1 to a theory T2.
			//
			// (1) Let both theories include a common base theory S, then the view can
			//     encompass "include ?S" which will be turned into a structure whose
			//     type component is "?T1" and whose definiens component is `OMIDENT(OMID
			//     (T2.path))`
			// (2) Let T1 include S1 and T2 include S2 and phi: S1 -> S2 a morphism.
			//     Then the view v can encompass "include ?phi", which will be turned
			//     into a structure whose type component is "?S1" and whose definiens
			//     component is "?phi".
			// TODO(Florian|Dennis) Have a look at the description above and confirm/decline.
			val incl = if (s.df.isDefined) s.df.get else s.from
			doURI(incl,rh)
		} else {
			rh("structure " + s.name + " : " + s.from.toMPath.^^.last + "?" + s.from.toMPath.last)
			//this.present(s.from, Some(s.path $ TypeComponent))
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
