package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{Context, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{ComplexStep, GeneratedFrom, GlobalName, ImplementationError, LocalName, MPath, SimpleStep}

/**
  * Linearly connects diagrams output by two [[LinearModuleOperator]] `in` and `out` with views.
  *
  * In categorical tonus, `in` and `out` are functors on MMT diagrams, and implementors of this trait
  * *may* be natural transformations, but do not need to be.
  *
  * For every theory `X`, the view `v: in(X) -> out(X)` is created include-preservingly and
  * declaration-by-declaration.
  * Views are not mapped at all.
  *
  * Implementors must implement/override
  *
  *  - `applyConstant()` (inherited as [[LinearOperator.applyConstant()]])
  *  - `translationView()`
  *
  * and may override, among other methods, in particular
  *
  *  - `beginTheory()`
  *  - `beginStructure()`
  *
  * @example In universal algebra, we can create the [[LinearModuleOperator]] `Sub(-)`
  *          that maps an SFOL-theory `X` to its SFOL-theory of substructures `Sub(X)`.
  *          But we can do more: for every mapped `X` we desire a view `sub_model: X -> Sub(X)`
  *          that realizes models of `Sub(X)` (i.e. submodels of X-models) as models of `X` (i.e. models
  *          of `X`) via predicate subtypes.
  *          Note that creation of the connecting view is still linear in `X`.
  *          You can use this trait to realzie exactly the creation of the `sub_model` connecting views.
  *
  *          Invariants so far:
  *
  *  - in and out have same domain/codomain
  *  - applyDeclaration outputs declarations valid in a view (esp. for output FinalConstants that means they
  *    have a definiens)
  */
trait LinearConnector extends LinearModuleOperator {
  val in: Functor
  val out: Functor

  lazy override val dom: Diagram = {
    if (in.dom != out.dom) {
      throw ImplementationError("Domains of in and out functors of a linear connector must match. But we got " +
        s"in.dom == `${in.dom}` and out.dom == `${out.dom}`.")
    }
    in.dom
  }
  lazy override val cod: Diagram = {
    /* wrong, even for mere pushout:
    if (in.cod != out.cod) {
      throw ImplementationError("Codomains of in and out functors of a linear connector must match. But we got " +
        s"in.cod == `${in.cod}` and out.cod == `${out.cod}`.")
    }*/
    in.cod
  }

  def applyDomainTheory(thy: MPath): Term
  /**
    * pre-conditions:
    *
    *  - only applicable on theory expressions
    *  - [[dom.hasImplicitFrom(t)]] must be true
    * @param t
    * @return
    */
  final def applyDomain(t: Term): Term = t match {
    case OMMOD(p) /* ideally: only if p points to a theory */ => applyDomainTheory(p)
  }

  /**
    * Creates a new output view that serves to contain the to-be-created assignments; called by
    * [[beginContainer()]].
    *
    * You may override this method to do additional action.
    *
    * @return The output view. If `Some(outView)` is returned, you must have called
    *         [[DiagramInterpreter.add()]] on `outView`.
    *
    * @example Some transformers need to add includes. They should
    *          override the method as follows:
    *          {{{
    *            override protected def beginTheory(...): Option[View] = {
    *              super.beginTheory(...).map(view => {
    *                val include: Structure = /* ... */
    *                interp.add(include)
    *                interp.endAdd(include) // don't forget!
    *
    *                view
    *              })
    *            }
    *           }}}
    */
  protected def beginTheory(thy: Theory)(implicit interp: DiagramInterpreter): Option[View] = {
    val outPath = applyModulePath(thy.path)

    val outView = View(
      outPath.doc, outPath.name,
      from = OMMOD(in.applyModulePath(thy.path)),
      to = OMMOD(out.applyModulePath(thy.path)),
      isImplicit = false
    )
    outView.setOrigin(GeneratedFrom(thy.path, this))
    interp.add(outView)

    Some(outView)
  }

  final override def beginContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean = {
    inContainer match {
      // connectors are only applicable on theories and their contents
      case _: View => false

      case inTheory: Theory =>
        beginTheory(inTheory) match {
          case Some(outView) =>
            interp.addToplevelResult(outView)
            transformedContainers += inTheory -> outView
            true

          case _ => false
        }

      // We accept structures, but don't create a special out container for them.
      // (Arguably the asymmetry stems from MMT that makes assignments to constants from structures
      //  be represented flatly in views.)
      case _: Structure =>
        // TODO still needed?
        // to fulfill invariants of other code snippets, we have to put something
        // arbitrary into processedElements
        /*state.diagramState.processedElements.put(inContainer.path, inContainer)*/
        true
    }
  }

  /**
    *
    * {{{
    *   include ?opDom   |-> include ?opCod = OMIDENT(?opDom)
    *   include ?S       |-> <nothing>                            if there is an implicit morphism ?S -> ?opDom
    *   include ?S       |-> include in(?S) = conn(?S)            if ?S is in input diagram
    *   include ?S = ?v  |-> include in(?S) = out(?v) . conn(?S)  if ?S, ?v are both in input diagram
    * }}}
    *
    * (In the last line, one path from the square of the commutativity of the natural transformation con -)
    *  is chosen. The other path could have been chosen as well.)
    *
    * We can handle the last two cases in a unified way as follows:
    * read ''include ?T'' as ''include ?T = OMIDENT(?T)'' and have cases
    *
    * {{{
    *   include ?S = ?v           |-> include in(?S) = out(?v) . conn(?S)
    *   include ?S = OMIDENT(?S)  |-> include in(?S) = OMIDENT(out(?S)) . conn(?S)
    * }}}
    *
    * Example:
    * Let S, T be theories, v: S -> T a view and suppose T contains an ''include ?S = ?v''. Then,
    * {{{
    *   S       in(S) -----conn(S)----> out(S)
    *   | v      | in(v)                  | out(v)
    *   v        v                        v
    *   T       in(T) -----conn(T)----> out(T)
    * }}}
    *
    * Here, in(T) contains an ''include in(S) = in(v)'' and out(T) contains analogously ''include out(S) = out(v)''.
    * Now, conn(T) must contain ''include in(S) = out(v) . conn(S)'' (or, alternatively,
    * ''include in(S) = conn(T) . in(v)'', but the latter but be somewhat self-referential in conn(T), so unsure
    * whether it works.)
    */
  final override def applyIncludeData(include: IncludeData, structure: Structure, container: Container)(implicit interp: DiagramInterpreter): Unit = {
    val ctrl = interp.ctrl // shorthand
    implicit val library: Lookup = ctrl.library

    // only need to connect undefined declarations
    if (include.df.nonEmpty) {
      return
    }

    if (include.args.nonEmpty) {
      // unsure what to do
      ???
    }

    val (newFrom, newDf) = include.from match {
      case from if dom.hasImplicitFrom(from) =>
        // only create the actually necessary includes
        // TODO hacky work around here, see discussion at https://mattermost.kwarc.info/kwarc/pl/opp88dhc57g4zmhyfzr7gyqixr
        if (library.hasImplicit(applyDomain(OMMOD(from)), OMMOD(in.applyModulePath(container.path.toMPath))))
          return

        (in.applyDomain(OMMOD(from)).toMPath, applyDomain(OMMOD(from)))

      case from =>
        val newDf = applyModule(ctrl.getModule(from)).map(m => {
          inheritState(container.path, m.path)
          m.path
        }).getOrElse(from) // when applyModule is inapplicable, default to leaving include data as-is

        (in.applyModulePath(from), OMMOD(newDf))
    }

    val outputInclude = Include.assignment(
      home = OMMOD(applyModulePath(container.path.toMPath)),
      from = newFrom,
      df = Some(newDf)
    )
    outputInclude.setOrigin(GeneratedFrom(structure.path, this))
    interp.add(outputInclude)
    interp.endAdd(outputInclude)

    /* TODO: in case we ever desire to map defined includes, too, here's how I did in the past
    val newDf: Term = include.df.getOrElse(OMIDENT(OMMOD(include.from))) match {
      case OMMOD(v) if diagramState.seenModules.contains(v) =>
        // even though we, as a connector, don't act on views, for consistency, we call applyModule nonetheless
        applyModule(ctrl.getModule(v))
        // todo: in which order does OMCOMP take its arguments? (Document this, too!)
        OMCOMP(OMMOD(out.applyModulePath(v)), OMMOD(applyModulePath(include.from)))

      case OMIDENT(OMMOD(thy)) if diagramState.seenModules.contains(thy) =>
        OMMOD(applyModulePath(include.from))

      case OMIDENT(OMMOD(p)) if in.operatorDomain.hasImplicitFrom(p) =>
        applyMetaModule(OMIDENT(OMMOD(p)))

      case _ => ???
    }*/
  }

  // some helper DSL
  def assgn(p: GlobalName, assignment: Term): Constant = {
    new FinalConstant(
      home = OMMOD(applyModulePath(p.module)),
      name = ComplexStep(p.module) / p.name, alias = Nil,
      tpC = TermContainer.empty(), dfC = TermContainer.asAnalyzed(assignment),
      rl = None, notC = NotationContainer.empty(), vs = Visibility.public
    )
  }
}

trait InwardsLinearConnector extends LinearConnector {
  val out: Functor
  lazy override val in: Functor = LinearFunctor.identity(out.dom) // lazy to let first `out` initialize
}

trait OutwardsLinearConnector extends LinearConnector {
  val in: Functor
  lazy override val out: Functor = LinearFunctor.identity(in.dom) // lazy to let first `in` initialize
}