package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects.{OMID, OMMOD, OMPMOD, Term}
import info.kwarc.mmt.api.opaque.{OpaqueElement, OpaqueText, OpaqueTextPresenter}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.URI

/**
  * Presenter writing out parsable MMT surface syntax.
  *
  * Usage
  * {{{
  * import info.kwarc.mmt.api.presentation
  * import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
  *
  * val presenter = state.ctrl.extman.getOrAddExtension(classOf[MMTSyntaxPresenter], "present-text-notations").getOrElse(
  *   throw new Exception // do something
  * )
  *
  * val stringRenderer = new presentation.StringBuilder
  * val yourTheory : Theory = ???
  * presenter(yourTheory)
  *
  * println(stringRenderer.get)
  * }}}
  *
  */

class MMTSyntaxPresenter(objectPresenter: ObjectPresenter = new NotationBasedPresenter) extends Presenter(objectPresenter) {

  /**
    * Determines if generated declarations (e.g. from derived modules or diagram operators) shall
    * be output as well.
    **/
  protected val presentGenerated = false

  /**
    * The format of [[MMTSyntaxPresenter]] as an extension
    */
  def key: String = "present-text-notations" + (if (presentGenerated) "-flat" else "")

  override def outExt = "mmt"

  private val OBJECT_DELIMITER = "❘"
  private val DECLARATION_DELIMITER = "❙"
  private val MODULE_DELIMITER = "❚"
  private val EOL = "\n"

  /**
    * Present an element such as a [[Theory]], a [[View]] or a [[Declaration]].
    *
    * @param element    The element to present. It must already be added to the [[info.kwarc.mmt.api.frontend.Controller]] instance this extension is linked to.
    * @param standalone if true, include appropriate header and footer
    * @param rh         output stream
    */
  override def apply(element: StructuralElement, standalone: Boolean = false)(implicit rh: RenderingHandler) {
    controller.simplifier(element) //TODO simplifying here is bad for elements that are not part of the diagram yet
    present(element, rh)(new PersistentNamespaceMap)
  }

  /**
    * Present an element such as a [[Theory]], a [[View]] or a [[Declaration]] to a string.
    *
    * Behavior equals [[apply()]] with [[presentation.StringBuilder]] as the [[RenderingHandler]].
    */
  def presentToString(element: StructuralElement, standalone: Boolean = false): String = {
    val stringRenderer = new presentation.StringBuilder
    apply(element, standalone)(stringRenderer)

    stringRenderer.get
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

    def add(s: String, uri: URI) = nsm = nsm.add(s, uri)

    def compact(s: String) = nsm.compact(s)
  }

  private def present(element: StructuralElement, rh: RenderingHandler)(implicit nsm: PersistentNamespaceMap): Unit = {
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
        pres.get.toString(objectPresenter, oe)(rh)
      case ii: InterpretationInstruction =>
        rh(ii.toString)
        ii match {
          case Namespace(_, ns) => nsm.base(ns.toString)
          case NamespaceImport(_, pr, ns) => nsm.add(pr, ns.uri)
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
        r.tp.foreach(doURI(_, rh))
      // r.tp foreach { t => apply(t, Some(r.path $ TypeComponent))(rh) }
    }
    endDecl(element, rh)
  }

  private def endDecl(element: StructuralElement, rh: RenderingHandler): Unit = element match {
    case _: Document => /* do nothing */
    case _: DRef => // rh(MODULE_DELIMITER)
    case _: MRef => // rh(MODULE_DELIMITER)
    case s: Structure if s.isInclude => rh(DECLARATION_DELIMITER + "\n")

    // TODO Fix for [[Structure.isInclude]] not accounting for inclusions
    //  with definiens component, see tod*o note in [[Include.unapply]]
    case s: Structure if s.getPrimitiveDeclarations.isEmpty => rh(DECLARATION_DELIMITER + "\n")
    case _: ModuleOrLink => rh(MODULE_DELIMITER)
    case _: NestedModule => rh(DECLARATION_DELIMITER)

    // some declarations are handled before by ModuleOrLink already
    case _: Declaration => rh(DECLARATION_DELIMITER + "\n")
    case ns: InterpretationInstruction =>
      rh(MODULE_DELIMITER + "\n")
    case t: OpaqueText =>
      val del = if (t.parent.toString.endsWith("omdoc")) MODULE_DELIMITER else DECLARATION_DELIMITER
      // rh("/t ")
      // t.text.toString(objectPresenter)(rh,OpaqueText.defaultEscapes)
      rh(del + "\n")
  }

  private def doTheory(theory: AbstractTheory, rh: RenderingHandler)(implicit nsm: PersistentNamespaceMap): Unit = {
    //TODO this ignores all narrative structure inside a theory
    rh(theory.feature + " " + theory.name)

    theory.meta.foreach(metaTheoryPath => {
      rh(" : ")
      doURI(OMMOD(metaTheoryPath), rh)
    })
    // TODO print type component

    // A def component of a theory might be an anonymous theory expression, for example
    // TODO In case we've got a def component, this presenter presents invalid syntax.
    doDefComponent(theory, rh)
    rh(" =")
    rh(" \n")
    theory.getDeclarations.foreach { d =>
      if (!d.isGenerated || presentGenerated) {
        present(d, indented(rh))
      }
    }
  }

  private def doView(view: View, rh: RenderingHandler)(implicit nsm: PersistentNamespaceMap): Unit = {
    rh("view " + view.name + " : ")
    doURI(view.from, rh)
    rh(" -> ")
    doURI(view.to, rh)

    // A def component of a view might be an anonymous morphism expression, for example
    // TODO In case we've got a def component, this presenter presents invalid syntax.
    doDefComponent(view, rh)
    rh(" =")
    rh(" \n")

    val declarations = if (presentGenerated) view.getDeclarations else view.getPrimitiveDeclarations
    declarations.foreach {
      case c: Constant => doConstant(c, rh, presentType = false)
      // In all other cases, present as usual, this might present invalid syntax, though
      case d => present(d, indented(rh))
    }
  }

  /**
    * Present a URI, possibly relative with the given namespace map.
    */
  private def doURI(tm: Term, rh: RenderingHandler)(implicit nsm: PersistentNamespaceMap): Unit = tm match {
    case OMPMOD(p, args) =>
      rh(nsm.compact(p.toString) + " ")
      if (args.nonEmpty) {
        rh("(")
        args.foreach(o => {
          apply(o, None)(rh);
          rh(" ")
        })
        rh(")")
      }
    case _ =>
      objectLevel.apply(tm, None)(rh)
  }

  /**
    * Present a constant
    *
    * @param presentType If true, the type component is presented if one exists. If false, the type component is not presented even if it exists.
    */
  private def doConstant(c: Constant, rh: RenderingHandler, presentType: Boolean = true)(implicit nsm: PersistentNamespaceMap): Unit = {
    /*
       A constant is built of multiple "elements" delimited by [[OBJECT_DELIMITER]].
       For example, here is a [[Constant]] with many elements:

       {{{
       judgement
         : {V:vocabulary}{Γ:Ctx V}Expr Γ⟶(Expr Γ⟶prop)
        ❘ = [V:vocabulary][Γ:Ctx V][e:Expr Γ][E:Expr Γ]foo bar
        ❘ role simplify
        ❘ @ jud
        ❘ @ judgment
        ❘ # 1 |- 2 ∶ 3 prec 100
        ❘ meta metakey1 ?DummyTheory2
        ❘ meta metakey2 (f  x)
      ❙
      }}}

      Note that "elements" is a term I made-up for the sake of this comment.
      It differs from the word "component" (established in MMT circles) insofar that for instance
      above we have multiple "meta" elements, but MMT treats the set of all metadatums of a constant
      as a single [[MetaDataComponent]] of that constant.

      In the following we aggregate lists of such elements, which we later join by appropriate
      newlines and [[OBJECT_DELIMITER object delimiters]].
      Since instead of naive string concatenation, we use the concept of [[RenderingHandler rendering handlers]]
      overall, these lists of elements are actually lists of callback functions each accepting a rendering handler.
     */

    // usual type and definiens, each lists of at most one element
    val typeElements = if (presentType) {
      c.tp.toList.map(typeTerm => (rh: RenderingHandler) => {
        rh(": ")
        apply(typeTerm, Some(c.path $ TypeComponent))(rh)
      })
    } else {
      Nil
    }

    val definiensElements = c.df.toList.map(definiensTerm => (rh: RenderingHandler) => {
      rh("= ")
      apply(definiensTerm, Some(c.path $ DefComponent))(rh)
    })

    // miscellaneous elements
    val aliasElements = c.alias.map(a => (rh: RenderingHandler) => {
      rh(s"@ ${a.toPath}")
    })

    val roleElements = c.rl.toList.map(roleStr => (rh: RenderingHandler) => {
      rh(s"role ${roleStr}")
    })

    val notationElements = c.notC.parsing.toList.map(textNotation => (rh: RenderingHandler) => {
      rh(s"# ${textNotation.toText}")
    })

    // TODO: (1) generalize this metadata output to theories, views (i.e. modules), and documents
    //       (2) Do not print special metadata like source references
    val metadataElements = c.metadata.getAll.map(datum => (rh: RenderingHandler) => {
      rh("meta ")
      doURI(OMID(datum.key), rh)
      rh(" ")
      objectPresenter(datum.value, Some(c.path $ MetaDataComponent))(rh)
    })

    // aggregate all elements in visually pleasing order
    val elements: List[RenderingHandler => Unit] =
      typeElements ::: definiensElements ::: roleElements ::: aliasElements ::: notationElements ::: metadataElements

    // present all elements
    rh(c.name.last.toString)
    val indentedRh = indented(rh)
    elements.zipWithIndex.foreach { case (renderFunction, index) =>
      if (index == 0) {
        indentedRh("\n" + "   ")
      } else {
        indentedRh("\n" + OBJECT_DELIMITER + " ")
      }
      renderFunction(indentedRh)
    }
    rh("\n")
  }

  private def doStructure(s: Structure, rh: RenderingHandler)(implicit nsm: PersistentNamespaceMap): Unit = s match {
    // special case of structures: trivial include
    case Include(IncludeData(home, from, args, df, total)) =>
      rh("include ")
      doURI(OMMOD(from), rh)
      // TODO args ignored
      df.foreach(definiensTerm => {
        rh(" " + OBJECT_DELIMITER + " ")
        objectPresenter(definiensTerm, Some(s.path $ DefComponent))(rh)
      })

    // actual structure, not just trivial include
    case _ =>
      rh("structure " + s.name + " : ")
      doURI(s.from, rh)
      doDefComponent(s, rh)

      val indentedRh = indented(rh)
      s.getDeclarations.foreach {
        case c: Constant => doConstant(c, indentedRh, presentType = false)
        case x => present(x, indentedRh)
      }
  }

  /** `= df` if df is present, returns true if there was one */
  private def doDefComponent(m: ModuleOrLink, rh: RenderingHandler): Boolean = {
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
class FlatMMTSyntaxPresenter(oP: ObjectPresenter = new NotationBasedPresenter) extends MMTSyntaxPresenter {
  override val presentGenerated = true
}
