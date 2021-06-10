package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{AbstractTheory, Link, Module, Theory, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMCOMP, OMIDENT, OMMOD, Term}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api._

// TODO: document setOrigin usage

/**
  * Linearly transforms theories to theories, and views to views.
  * A functor on diagrams, which acts include-preservingly and declaration-by-declaration in theories
  * and views.
  *
  * Implementors must implement
  *
  *  - `applyConstant()` (inherited as [[LinearOperator.applyConstant()]])
  *
  * and may override, among other methods, in particular
  *
  *  - `beginTheory()`
  *  - `beginView()`
  *  - `beginStructure()`
  */
trait LinearFunctor extends LinearModuleOperator with Functor {
  /**
    * Creates a new output theory that serves to contain the to-be-mapped declarations; called by
    * [[beginModule()]].
    *
    * You may override this method to do additional action.
    *
    * @return The output theory. If `Some(outTheory)` is returned, you must have called
    *         [[DiagramInterpreter.add()]] on `outTheory`.
    *
    * @example Some transformers need to add includes at the beginning of every theory, and
    *          correspondingly an include assignment at the beginning of every view.
    *
    *          The transformers can override [[beginTheory()]] and [[beginView()]] as follows:
    *          {{{
    *            val additionalTheory: MPath
    *
    *            override protected def beginTheory(thy: Theory, state: LinearState)(implicit interp: DiagramInterpreter): Option[Theory] = {
    *              super.beginTheory(thy, state).map(outTheory => {
    *                val include = PlainInclude(additionalTheory, outTheory.path)
    *                interp.add(include)
    *                interp.endAdd(include)  // don't forget endAdd!
    *                outTheory
    *              })
    *            }
    *
    *            override protected def beginView(view: View, state: LinearState)(implicit interp: DiagramInterpreter): Option[View] = {
    *              super.beginView(view, state).map(outView => {
    *                val include = Include.assignment(
    *                  outView.toTerm,
    *                  OMMOD(additionalTheory),
    *                  Some(OMIDENT(OMMOD(additionalTheory)))
    *                )
    *                interp.add(include)
    *                interp.endAdd(include)  // don't forget endAdd!
    *
    *                outView
    *              })
    *          }}}
    */
  protected def beginTheory(thy: Theory)(implicit interp: DiagramInterpreter): Option[Theory] = {
    val outPath = applyModulePath(thy.path)
    val newMeta = thy.meta.map {
      case mt if dom.hasImplicitFrom(mt)(interp.ctrl.library) =>
        applyDomain(OMMOD(mt)).toMPath
      case mt =>
        if (applyModule(interp.ctrl.getModule(mt)).isEmpty) {
          interp.errorCont(InvalidElement(thy, s"Theory had meta theory `$mt` for which there " +
            s"was no implicit morphism into `$dom`. Recursing into meta theory as usual " +
            s"failed, too; reasons are probably logged above. Keeping meta theory as-is."))
          mt
        } else {
          applyModulePath(mt)
        }
    }

    val outTheory = Theory.empty(outPath.doc, outPath.name, newMeta)
    outTheory.setOrigin(GeneratedFrom(thy.path, this))
    interp.add(outTheory)

    Some(outTheory)
  }

  /**
    * Creates a new output view that serves to contain the to-be-mapped assignments; called by
    * [[beginModule()]].
    *
    * @return The output view. If `Some(outView)` is returned, you must have called
    *         [[DiagramInterpreter.add()]] on `outView`.
    *
    * You may override this method to do additional action.
    *
    * @see [[beginTheory()]] for an example
    */
  protected def beginView(view: View)(implicit interp: DiagramInterpreter): Option[View] = {
    if (applyModule(interp.ctrl.getModule(view.from.toMPath)).isEmpty) {
      return None
    }
    if (applyModule(interp.ctrl.getModule(view.to.toMPath)).isEmpty) {
      return None
    }

    inheritState(view.path, view.to.toMPath)

    val outPath = applyModulePath(view.path)
    val outView = View(
      outPath.doc, outPath.name,
      OMMOD(applyModulePath(view.from.toMPath)), OMMOD(applyModulePath(view.to.toMPath)),
      view.isImplicit
    )
    outView.setOrigin(GeneratedFrom(view.path, this))
    interp.add(outView)

    Some(outView)
  }

  /**
    * Creates a new output structure that serves to contain the to-be-mapped assignments; called by
    * [[beginModule()]].
    *
    * @return The output structure. If `Some(outStructure)` is returned, you must have called
    *         [[DiagramInterpreter.add()]] on `outStructure`.
    *
    * You may override this method to do additional action.
    *
    * @see [[beginTheory()]], [[beginView()]]
    */
  protected def beginStructure(s: Structure)(implicit interp: DiagramInterpreter): Option[Structure] = s.tp.flatMap {
    case OMMOD(structureDomain) =>
      val newStructureDomain = applyModule(interp.ctrl.getModule(structureDomain)).getOrElse(
        return None
      )

      inheritState(s.modulePath, s.home.toMPath)

      // TODO: s.dfC is thrown away/ignored
      val outStructure = new Structure(
        home = OMMOD(applyModulePath(s.path.module)),
        name = s.name,
        tpC = TermContainer.asAnalyzed(newStructureDomain.toTerm),
        dfC = TermContainer.empty(),
        s.isImplicit, s.isTotal
      )
      outStructure.setOrigin(GeneratedFrom(s.path, this))
      interp.add(outStructure)
      Some(outStructure)
    case _ => None
  }

  /**
    * Creates a new output module that serves to contain the to-be-mapped assignments; called by
    * [[beginContainer()]].
    *
    * @see [[beginTheory()]], [[beginView()]], [[beginStructure()]].
    */
  private def beginModule(inModule: Module)(implicit interp: DiagramInterpreter): Option[Module] = {
    /* TODO if (!diagramState.seenModules.contains(inModule.path)) {
      interp.errorCont(InvalidElement(
        inModule,
        "unbound module not in input diagram"
      ))
      return None
    }*/

    (inModule match {
      case thy: Theory => beginTheory(thy)
      case view: View => beginView(view)
    }).map(outModule => {
      transformedContainers += inModule -> outModule
      interp.addToplevelResult(outModule)
      outModule
    })
  }

  /**
    * See superclass documentation, or [[beginContainer()]].
    */
  final override def beginContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean = {
    val outContainer = inContainer match {
      case m: Module => beginModule(m)
      case s: Structure => beginStructure(s)
    }

    outContainer match {
      case Some(outContainer) =>
        transformedContainers += inContainer -> outContainer
        true

      case _ => false
    }
  }

  /**
    *
    * {{{
    *   include ?opDom [= E]  |-> include ?opCod [= E']
    *   include ?S [= E]      |-> include ?S [= E']         if there is an implicit morphism ?S -> ?opDom (case probably wrong)
    *   include ?S [= E]      |-> include ?op(S) [= E']     if ?S is in input diagram
    * }}}
    *
    * and E via
    * {{{
    *   OMIDENT(?opDom)       |-> OMIDENT(?opCod)
    *   OMIDENT(?S)           |-> OMIDENT(?S)              if there is an implicit morphisim ?S -> ?opDom (case probably wrong)
    *   OMIDENT(?S)           |-> OMIDENT(?op(S))          if ?T is in input diagram
    *   ?v                    |-> ?op(v)                   if ?v is in input diagram
    * }}}
    */
  override def applyIncludeData(include: IncludeData, structure: Structure, container: Container)(implicit interp: DiagramInterpreter): Unit = {
    val ctrl = interp.ctrl
    implicit val library: Lookup = ctrl.library

    if (include.args.nonEmpty) ???
/*

TODO: problem: unbound includes cannot be noticed anymore since we have no information of what the current input diagram is
*/
    def tr(t: Term): Term = t match {
      // base cases
      case t if dom.hasImplicitFrom(t) => applyDomain(t)
      case OMMOD(from) =>
        applyModule(ctrl.getModule(from)).map(m => {
          inheritState(container.modulePath, from)
          m.toTerm
        }).getOrElse(OMMOD(from)) // when applyModule is inapplicable, default to leaving include data as-is

      // complex cases
      case OMCOMP(mors) => OMCOMP(mors.map(tr))
      case OMIDENT(t) => OMIDENT(tr(t))
    }

    val newFrom = tr(OMMOD(include.from))
    val newDf = include.df.map(tr)

    val s = Structure(
      home = OMMOD(applyModulePath(container.modulePath)),
      name = LocalName(newFrom.toMPath), // TODO NR@FR: does this `name` make sense?
      from = newFrom,
      df = newDf,
      isImplicit = if (container.isInstanceOf[Theory]) true else false,
      isTotal = include.total
    )
    s.setOrigin(GeneratedFrom(structure.path, this))

    // TODO hack to prevent: "add error: a declaration for the name [...] already exists [...]"
    //      when refactoring the whole framework, we should fix this anyway in the course of doing so
    if (ctrl.getO(s.path).isEmpty) {
      interp.add(s)
      interp.endAdd(s)
    }
  }

  // some helper DSL
  def const(p: GlobalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter): Constant = {
    new FinalConstant(
      home = OMMOD(p.module),
      name = p.name, alias = Nil,
      tpC = TermContainer.asAnalyzed(tp), dfC = TermContainer.asAnalyzed(df),
      rl = None, notC = NotationContainer.empty(), vs = Visibility.public
    )
  }

  // TODO (NR@FR): this construct is needed because otherwise in a linear functor
  //      doing something like ``Constant(applyModulePath(c.path.module), c.name, ...``
  //      produces wrong constant names for c being a view assignment (namely,
  //      the domain theory in the ComplexStep also needs to be passed through applyModulePath)
  protected lazy val equiNamer: SystematicRenamer = getRenamerFor("")

  /**
    * Utilities and DSL to systematically rename constants in [[LinearOperator]]s.
    *
    * These utilities are meant to be invoked within [[LinearOperator.applyDeclaration()]]
    * or methods called therein; in particular [[LinearOperator.applyConstant()]],
    * [[SimpleLinearModuleTransformer.applyConstantSimple()]], and
    * [[SimpleLinearConnectorTransformer.applyConstantSimple()]].
    *
    * Only renames constants seen so far while processing (incl. the declaration being processed
    * right now).
    * Concretely, the methods herein depend on declarations being added to
    * `state.processedDeclarations` *before* they are passed to [[LinearOperator.applyDeclaration()]].
    * See also the pre-condition of [[LinearOperator.applyDeclaration()]].
    *
    * @todo add example
    */
  protected def getRenamerFor(tag: String): SystematicRenamer = new SystematicRenamer {
    override def apply(name: LocalName): LocalName = name.suffixLastSimple(tag)

    /**
      * Bends a path pointing to something in `state.inContainer` to a path
      * pointing to the systematically renamed variant in `state.outContainer`.
      *
      * @example Suppose `path = doc ? thy ? c` where c is a [[SimpleStep]],
      *          then `apply(path) = applyModulePath(doc ? thy) ? apply(c)`
      * @example Suppose we are in a view and encounter an assignment with
      *          `path = doc ? view ? ([doc ? thy] / c)` where `[doc ? thy]`.
      *          Here, `[doc ? thy]` is a [[ComplexStep]] encoding the domain
      *          the constant to be assigned.
      *          Then,
      *          `apply(path) = applyModulePath(doc ? view) ? ([applyModulePath(doc ? thy)] / c)`.
      * @todo Possibly, this method is wrong for nested theories and views. Not tested so far.
      */
    override def apply(path: GlobalName): GlobalName = {
      if (seenDeclarations(path.module).contains(path)) {
        applyAlways(path)
      } else {
        path
      }
    }

    def applyAlways(path: GlobalName): GlobalName = {
      val newModule = applyModulePath(path.module)
      val newName = path.name match {
        case LocalName(ComplexStep(domain) :: name) =>
          LocalName(applyModulePath(domain)) / apply(name)
        case name => apply(name)
      }

      newModule ? newName
    }

    /*override def apply(term: Term): Term = {
      val self: SystematicRenamer = this // to disambiguate this in anonymous subclassing expression below
      new OMSReplacer {
        override def replace(p: GlobalName): Option[Term] = Some(OMS(self(p)))
      }.apply(term, Context.empty)
    }*/
  }

  /**
    * usually used like ''connector.applyModulePath(expressionContext(c))'' within [[applyConstant()]].
    * @param c
    * @param interp
    * @return
    */
  protected def expressionContext(c: Constant)(implicit interp: DiagramInterpreter): Term = {
    interp.ctrl.getModule(c.path.module) match {
      case t: AbstractTheory => t.toTerm
      case l: Link => l.to
    }
  }
}

object LinearFunctor {
  /**
    * No-op identity [[LinearOperator transformer]] on some diagram.
    *
    * Its purpose is to serve for the `in` or `out` field of [[LinearConnector]]s.
    */
  def identity(domain: Diagram): LinearFunctor = new LinearFunctor {
    override def applyModuleName(name: LocalName): LocalName = name

    override val dom: Diagram = domain
    override val cod: Diagram = domain
    override def applyDomainModule(m: MPath): MPath = m

    override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = List(c)
  }

  def identity(domainTheory: MPath): LinearFunctor = identity(Diagram(List(domainTheory), None))
}
