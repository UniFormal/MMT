package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects.{OMID, OMMOD, OMPMOD, Term}
import info.kwarc.mmt.api.opaque.{OpaqueElement, OpaqueText, OpaqueTextPresenter}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils.URI

/**
  * Presents (hopefully) parsable MMT surface syntax from [[StructuralElement]]s like
  * [[Theory theories]], [[View views]], [[Constant constants]], etc.
  *
  * @example ''controller.presenter.asString(element)'' to present a
  *          [[StructuralElement]] ''element'' to a String.
  *          If you repeatedly present elements to strings, it might be more efficient
  *          to use the [[apply()]] method with a [[StringBuilder]] (from the
  *          mmt.api.presentation API!)
  *
  * @see The server ''info.kwarc.mmt.api.web.SyntaxPresenterServer'' exposes this syntax presenter to the web.
  *
  * @author Among others, ComFreek knows the code somewhat well, esp. on the generated indentations.
  */

class MMTSyntaxPresenter(objectPresenter: ObjectPresenter = new NotationBasedPresenter) extends Presenter(objectPresenter) {

  /**
    * Determines if generated declarations (e.g. from derived modules or diagram operators) shall
    * be output as well.
    **/
  protected val presentGenerated = false

  /** Determines if the special delimiters of MMT surface syntax are printed (only needed if machine parsing of output is planned) */
  protected val presentDelimiters = false

  /**
    * The format of [[MMTSyntaxPresenter]] as an extension
    */
  def key: String = "present-text-notations" + (if (presentGenerated) "-flat" else "")

  override def outExt = "mmt"

  private val OBJECT_DELIMITER = "❘"
  private def getObjDelim = if (presentDelimiters) OBJECT_DELIMITER + " " else ""
  private val DECLARATION_DELIMITER = "❙"
  private def getDeclDelim = if (presentDelimiters) DECLARATION_DELIMITER + "\n" else ""
  private val MODULE_DELIMITER = "❚"
  private def getModDelim = if (presentDelimiters) MODULE_DELIMITER + "\n" else ""
  private val EOL = "\n"

  /**
    * Present an element such as a [[Theory]], a [[View]] or a [[Declaration]].
    *
    * @param element    The element to present. It must already be added to the [[info.kwarc.mmt.api.frontend.Controller]] instance this extension is linked to.
    * @param standalone if true, include appropriate header and footer
    * @param rh         output stream
    */
  override def apply(element: StructuralElement, standalone: Boolean = false)(implicit rh: RenderingHandler): Unit = {
    controller.simplifier(element) //TODO simplifying here is bad for elements that are not part of the diagram yet
    present(element, rh)(new PersistentNamespaceMap)
  }

  /**
    * Creates an indented version of a rendering handler.
    *
    * Every line rendered with the indented handler is indented. This is even true if
    * you pass to it multiline strings at once. (They will be inspected.)
    *
    * In your code, you can freely mix the original handler ''rh'' and the indented one.
    * The indented has *no* internal buffer or cache.
    *
    * @param rh          The original rendering handler to delegate to.
    * @param indentation The number of indentation levels (i.e. tabs)
    *
    * @author ComFreek
    */
  private def indented(rh: RenderingHandler, indentation: Int = 1): RenderingHandler = {
    val indentationString = "\t" * indentation

    var isAtStartOfLine = true

    str: String => {
      if (isAtStartOfLine && str.nonEmpty) {
        rh.write(indentationString)
        isAtStartOfLine = false
      }

      // Replace *inner* EOLs by EOL+indentationString
      val indentedStr = {
        if (str.endsWith(EOL)) {
          isAtStartOfLine = true

          // TODO Probably bad for when EOL == Unicode EOL (but we have EOL == "\n" anyway, which has normal
          //      ASCII length characteristics)
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

    def base(s: String): Unit = nsm = nsm.base(s)

    def add(s: String, uri: URI): Unit = nsm = nsm.add(s, uri)

    def compact(s: String): String = nsm.compact(s)
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
      case s: SRef =>
        controller.getO(s.target) match {
          case None => rh("symbol " + s.target.toPath)
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
      case v: View =>
        rh("\n")
        doView(v, rh)
      case dd: DerivedDeclaration =>
        rh << dd.feature + " "
        controller.extman.get(classOf[StructuralFeature], dd.feature) match {
          case None => rh << dd.name.toString + " (implementation is not known)"
          case Some(sf) =>
            val header = sf.makeHeader(dd)
            apply(header, Some(dd.path $ TypeComponent))(rh)
        }
        if (dd.getDeclarations.nonEmpty) {rh << "\n"}
        val notationElements = List(dd.notC.getParseDefault, dd.notC.getPresentDefault).zipWithIndex.map { not =>
          (rh: RenderingHandler) => not match {
            case (Some(not), 0) =>
              rh("\n")
              rh(s"# ${not.toText}")
            case (Some(not), 1) =>
              rh("\n" + getObjDelim)
              rh(s"## ${not.toText}")
            case (None, _) => // nothing to do
            case _ => ??? // not yet implemented
          }
        }

        notationElements.foreach { _(indented(rh)) }
        if (dd.getDeclarations.nonEmpty) {
          rh(getObjDelim + "=\n")
          dd.module.getDeclarations.foreach { d => present(d, indented(rh)) }
        }

      case dm: DerivedModule =>
        doTheory(dm, indented(rh))
      case nm: NestedModule =>
        present(nm.module, rh)
      case s: Structure => doStructure(s, rh)
      case r: RuleConstant =>
        if (r.df.isEmpty) rh("unknown ")
        rh("rule ")
        r.tp.foreach(doURI(_, rh, needsHand = true))
      // r.tp foreach { t => apply(t, Some(r.path $ TypeComponent))(rh) }
    }
    endDecl(element, rh)
  }

  private def endDecl(element: StructuralElement, rh: RenderingHandler): Unit = element match {
    case _: Document => /* do nothing */
    case _: DRef => // rh(MODULE_DELIMITER)
    case _: MRef => // rh(MODULE_DELIMITER)
    case s : SRef =>
    case s: Structure if s.isInclude => rh(getDeclDelim)

    // TODO Fix for [[Structure.isInclude]] not accounting for inclusions
    //  with definiens component, see todo note in [[Include.unapply]]
    case s: Structure if s.getPrimitiveDeclarations.isEmpty => rh(getDeclDelim)
    case _: ModuleOrLink => rh(getModDelim)
    case _: NestedModule => /* nothing, the module delimiter of the presented module already accounts for this */

    // some declarations are handled before by ModuleOrLink already
    case _: Declaration => rh(getDeclDelim)
    case _: InterpretationInstruction =>
      rh(getModDelim)
    case t: OpaqueText =>
      val del = if (t.parent.toString.endsWith("omdoc")) getModDelim else getDeclDelim
      // rh("/t ")
      // t.text.toString(objectPresenter)(rh,OpaqueText.defaultEscapes)
      rh(del)
  }

  private def doTheory(theory: AbstractTheory, rh: RenderingHandler)(implicit nsm: PersistentNamespaceMap): Unit = {
    //TODO this ignores all narrative structure inside a theory
    rh(theory.feature + " " + theory.name)

    theory.meta.foreach(metaTheoryPath => {
      rh(" : ")
      doURI(OMMOD(metaTheoryPath), rh, needsHand = false)
    })
    // TODO print type component

    // A def component of a theory might be an anonymous theory expression, for example
    // TODO In case we've got a def component, this presenter presents invalid syntax.
    doDefComponent(theory, rh)
    rh(" =")
    rh(" \n")
    val indentedRh = indented(rh)
    theory.getDeclarations.foreach { d =>
      // if (!d.isGenerated || presentGenerated) {
      // TODO(NR@anyone): I deactivated this because now all diagops output
      //     is marked as generated, thus hidden
        present(d, indentedRh)
      // }
    }
    rh("\n")
  }

  private def doView(view: View, rh: RenderingHandler)(implicit nsm: PersistentNamespaceMap): Unit = {
    rh("view " + view.name + " : ")
    doURI(view.from, rh, needsHand = false)
    rh(" -> ")
    doURI(view.to, rh, needsHand = false)

    // A def component of a view might be an anonymous morphism expression, for example
    // TODO In case we've got a def component, this presenter presents invalid syntax.
    doDefComponent(view, rh)
    rh(" =")
    rh(" \n")

    val indentedRh = indented(rh)
    // TODO(NR@anyone): I deactived this, too
    val declarations = /*if (presentGenerated)*/ view.getDeclarations /*else view.getPrimitiveDeclarations*/
    declarations.foreach {
      case c: Constant =>
        // We want to avoid presenting types here, hence manually call doConstant and endDecl
        // instead of present.
        // doStructure does something similar.
        doConstant(c, indentedRh, presentType = false)
        endDecl(c, indentedRh)

      // In all other cases, present as usual, this might present invalid syntax, though
      case d => present(d, indentedRh)
    }
    rh("\n")
  }

  /**
    * Presents a URI, possibly relative with the given namespace map.
    * @param needsHand If the hand symbol (☞) should be prepended. As a rule of thumb, if you are rendering in a context
    *                  where a Term is genreally expected, the hand is needed.
    *
    *                  See also [[https://stackoverflow.com/questions/64789876/how-to-give-an-absolute-uri-in-mmt-getting-unbound-token-http-and-ill-forme]].
    */
  private def doURI(tm: Term, rh: RenderingHandler, needsHand: Boolean)(implicit nsm: PersistentNamespaceMap): Unit = tm match {
    case OMPMOD(p, args) =>
      if (needsHand) {
        rh("☞")
      }
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
      rh(s"role $roleStr")
    })

    // zipWithIndex before collecting (filtering) since we want to enforce that
    // parsing notations are presented with one hash ('#'), and presentation notations with two hashes ('##')
    val notationElements = List(c.notC.getParseDefault, c.notC.getPresentDefault).zipWithIndex.collect {
      case (Some(not), i) => (not, i)
    }.map { x => (rh: RenderingHandler) => (x: @unchecked) match { // match is exhausting, scalac doesn't get it, though
          case (not, 0) => rh(s"# ${not.toText}")
          case (not, 1) => rh(s"## ${not.toText}")
        }
    }

    // TODO: (1) generalize this metadata output to theories, views (i.e. modules), and documents
    //       (2) Do not print special metadata like source references
    val metadataElements = c.metadata.getAll.map(datum => (rh: RenderingHandler) => {
      rh("meta ")
      doURI(OMID(datum.key), rh, needsHand = false)
      rh(" ")
      objectPresenter(datum.value, Some(c.path $ MetaDataComponent))(rh)
    })

    // aggregate all elements in visually pleasing order
    val elements: List[RenderingHandler => Unit] =
      typeElements ::: definiensElements ::: roleElements ::: aliasElements ::: notationElements ::: metadataElements

    // present all elements

    // only present a simplified (possibly ambiguous) variant of the name to remain readable for humans.
    rh(c.name.dropComplex.toString)

    val indentedRh = indented(rh)
    elements.zipWithIndex.foreach { case (renderFunction, index) =>
      if (index == 0) {
        indentedRh("\n")
      } else {
        indentedRh("\n" + getObjDelim)
      }
      renderFunction(indentedRh)
    }
    rh("\n")
  }

  private def doStructure(s: Structure, rh: RenderingHandler)(implicit nsm: PersistentNamespaceMap): Unit = s match {
    // special case of structures: plain includes and realizations
    case Include(IncludeData(home, from, args, df, total)) =>
      if (total) rh("realize ") else rh("include ")
      doURI(OMMOD(from), rh, needsHand = true)
      // TODO args ignored
      df.foreach(definiensTerm => {
        rh(" " + getObjDelim)
        objectPresenter(definiensTerm, Some(s.path $ DefComponent))(rh)
      })

    // actual structure, not just trivial include
    case _ =>
      rh("structure " + s.name + " : ")
      doURI(s.from, rh, needsHand = false)
      doDefComponent(s, rh)

      rh("= \n")

      val indentedRh = indented(rh)
      s.getDeclarations.foreach {
        case c: Constant =>
          // We want to avoid presenting types here, hence manually call doConstant and endDecl
          // instead of present.
          // doView does something similar.
          doConstant(c, indentedRh, presentType = false)
          endDecl(c, indentedRh)
        case x => present(x, indentedRh)
      }
      rh("\n")
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
class FlatMMTSyntaxPresenter(objectPresenter: ObjectPresenter = new NotationBasedPresenter) extends MMTSyntaxPresenter(objectPresenter) {
  override val presentGenerated = true
}