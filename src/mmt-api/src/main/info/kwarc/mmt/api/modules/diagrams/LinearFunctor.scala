package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{AbstractTheory, Link, Module, Theory, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{Context, OMCOMP, OMID, OMIDENT, OMMOD, OMS, OMSReplacer, Term}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller

/**
  * A functor that linearly maps theories to theories and views to views.
  *
  * Concretely, it maps diagrams over [[ModuleOperator.dom]] to diagrams over [[ModuleOperator.cod]]
  * (both inherited fields) in an include- and structure-preserving way, declaration-by-declaration.
  *
  * Implementors must implement
  *
  *  - `applyConstant()` (inherited as [[LinearOperator.applyConstant]])
  *
  * and may override, among other methods, for reasons of preprocessing in particular
  *
  *  - `beginTheory()`
  *  - `beginView()`
  *  - `beginStructure()`
  */
trait LinearFunctor extends LinearModuleOperator with Functor with LinearFunctorDSL {
  /**
    * Creates a new output theory that serves to contain the to-be-mapped declarations; called by
    * [[beginModule]].
    *
    * You may override this method to implement additional action.
    *
    * @return The output theory if the functor is applicable on `thy`. If it was and `Some(outTheory)` is returned, then
    *         this method must have called [[DiagramInterpreter.add]] on `outTheory` before returning.
    * @example Some functors choose to add includes at the beginning of every output theory, and
    *          correspondingly an include assignment at the beginning of every output view.
    *
    *          Those functors can override [[beginTheory]] and [[beginView]] as follows:
    *          {{{
    *            val additionalTheory: MPath
    *
    *            override protected def beginTheory(thy: Theory)(implicit interp: DiagramInterpreter): Option[Theory] = {
    *              super.beginTheory(thy, state).map(outTheory => {
    *                val include = PlainInclude(additionalTheory, outTheory.path)
    *                interp.add(include)
    *                interp.endAdd(include)  // don't forget endAdd!
    *                outTheory
    *              })
    *            }
    *
    *            override protected def beginView(view: View)(implicit interp: DiagramInterpreter): Option[View] = {
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
    *           }}}
    */
  protected def beginTheory(thy: Theory)(implicit interp: DiagramInterpreter): Option[Theory] = {
    val outPath = applyModulePath(thy.path)
    val newMeta = thy.meta.map {
      case mt if dom.hasImplicitFrom(mt)(interp.ctrl.library) => // meta theory is subsumed by functor's domain
        applyDomain(OMMOD(mt)).toMPath
      case mt => // otherwise, recurse into meta theory
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
    outTheory.setOrigin(GeneratedFrom(thy.path, this, None))
    interp.add(outTheory)

    Some(outTheory)
  }

  /**
    * Creates a new output view that serves to contain the to-be-mapped assignments; called by
    * [[beginModule]].
    *
    * @return The output view if the functor was applicable on `view`. If it was and `Some(outView)` is returned, then
    *         this method must have called [[DiagramInterpreter.add `interp.add(outView)`]] before returning.
    *
    * You may override this method to implement additional action, see documentation at [[beginTheory]].
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
    outView.setOrigin(GeneratedFrom(view.path, this, None))
    interp.add(outView)

    Some(outView)
  }

  /**
    * Creates a new output structure that serves to contain the to-be-mapped assignments; called by
    * [[beginModule]].
    *
    * @return The output structure. If `Some(outStructure)` is returned, you must have called
    *         [[DiagramInterpreter.add]] on `outStructure`.
    *
    * You may override this method to implement additional action, see documentation at [[beginTheory]].
    * @see [[beginTheory]], [[beginView]]
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
      outStructure.setOrigin(GeneratedFrom(s.path, this, None))
      interp.add(outStructure)
      Some(outStructure)
    case _ => None
  }

  /**
    * Creates a new output module that serves to contain the to-be-mapped assignments; called by
    * [[beginContainer]].
    *
    * @see [[beginTheory]], [[beginView]], [[beginStructure]].
    */
  private def beginModule(inModule: Module)(implicit interp: DiagramInterpreter): Option[Module] = {
    (inModule match {
      case thy: Theory => beginTheory(thy)
      case view: View => beginView(view)
    }).map(outModule => {
      interp.addToplevelResult(outModule)
      outModule
    })
  }

  /**
    * @inheritdoc [[LinearModuleOperator.beginContainer]]
    */
  final override def beginContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean = {
    val outContainer = inContainer match {
      case m: Module => beginModule(m)
      case s: Structure => beginStructure(s)
    }

    outContainer match {
      case Some(outContainer) =>
        mappedContainers += inContainer -> outContainer
        true

      case _ => false
    }
  }

  override def applyIncludeData(include: IncludeData, structure: Structure, container: Container)(implicit interp: DiagramInterpreter): Unit = {
    val ctrl = interp.ctrl
    implicit val library: Lookup = ctrl.library

    if (include.args.nonEmpty)
      throw new NotImplementedError("Parametric includes not supported by linear diagram operators yet")

    def handleFrom(from: MPath): Term = {
      if (dom.hasImplicitFrom(from)) applyDomain(OMMOD(from))
      else {
        applyModule(ctrl.getModule(from)).map(m => {
          inheritState(container.modulePath, from)
          m.toTerm
        }).getOrElse(OMMOD(from)) // when applyModule is inapplicable, default to leaving include data as-is
      }
    }

    def handleDf(t: Term): Term = t match {
      case OMCOMP(mors) => OMCOMP(mors.map(handleDf))
      case OMIDENT(t) => OMIDENT(handleDf(t))
      case _ => ???
    }

    val newFrom = handleFrom(include.from)
    val newDf = include.df.map(handleDf)

    val s = Structure(
      home = OMMOD(applyModulePath(container.modulePath)),
      name = LocalName(newFrom.toMPath), // TODO NR@FR: does this `name` make sense? esp. for views?
      from = newFrom,
      df = newDf,
      isImplicit = if (container.isInstanceOf[Theory]) true else false, // theory includes are always implicit
      isTotal = include.total
    )
    s.setOrigin(GeneratedFrom(structure.path, this, None))

    // TODO hack to prevent: "add error: a declaration for the name [...] already exists [...]"
    //      when refactoring the whole framework, we should fix this anyway in the course of doing so
    if (ctrl.getO(s.path).isEmpty) {
      interp.add(s)
      interp.endAdd(s)
    }
  }
}

object LinearFunctor {
  /**
    * The no-op identity linear functor for a given domain/codomain diagram.
    *
    * This method comes in handy when overriding the `in` or `out` field of [[LinearConnector]] implementations.
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

// todo(NR,FR) review this together
trait LinearFunctorDSL {
  this: LinearModuleOperator =>

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
    * Systematically renames according to the state of the outer [[LinearFunctor]] class.
    * Implementors only need to give [[SystematicRenamer.apply(name: LocalName)]]
    */
  trait LinearRenamer extends SystematicRenamer {

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
      if (seenDeclarations.get(path.module).exists(_.contains(path))) {
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

    override def apply(term: Term): Term = {
      val self: SystematicRenamer = this // to disambiguate this in anonymous subclassing expression below
      new OMSReplacer {
        override def replace(p: GlobalName): Option[Term] = Some(OMS(self(p)))
      }.apply(term, Context.empty)
    }
  }

  /**
    * Utilities and DSL to systematically rename constants in [[LinearOperator]]s.
    *
    * These utilities are meant to be invoked within [[LinearOperator.applyDeclaration]]
    * or methods called therein; in particular [[LinearOperator.applyConstant]],
    * [[SimpleLinearModuleTransformer.applyConstantSimple]], and
    * [[SimpleLinearConnectorTransformer.applyConstantSimple]].
    *
    * Only renames constants seen so far while processing (incl. the declaration being processed
    * right now).
    * Concretely, the methods herein depend on declarations being added to
    * `state.processedDeclarations` *before* they are passed to [[LinearOperator.applyDeclaration]].
    * See also the pre-condition of [[LinearOperator.applyDeclaration]].
    *
    * @todo add example
    */
  protected def getRenamerFor(tag: String): SystematicRenamer = new LinearRenamer {
    override def apply(name: LocalName): LocalName = name.suffixLastSimple(tag)
  }

  protected def getRenamerFor(tag: LocalName): SystematicRenamer = new LinearRenamer {
    override def apply(name: LocalName): LocalName = name / tag
  }

  /**
    * usually used like ''connector.applyModulePath(expressionContext(c))'' within [[applyConstant]].
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
