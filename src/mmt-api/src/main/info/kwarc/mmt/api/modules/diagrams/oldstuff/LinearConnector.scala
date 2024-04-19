package info.kwarc.mmt.api.modules.diagrams.oldstuff

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMMOD, Term}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.diagrams.{Diagram, DiagramInterpreter}

/**
  * A natural transformation betweeen two [[LinearFunctor]]s `in` and `out` that linearly maps theories to views ("connections") and views not at all.
  *
  * Concretely, it maps theories `X` in diagrams in an include- and structure-preserving way to views
  * `v: in(X) -> out(X)`, where declaration-by-declaration every declaration of `X` is mapped to an assignment
  * in `v` by means of `applyConstant()`.
  *
  * Implementors must implement/override
  *
  *  - `applyConstant()` (inherited as [[LinearOperator.applyConstant]])
  *  - `translationView()`
  *
  * and may override, among other methods, for reasons of preprocessing in particular
  *
  *  - `beginTheory()`
  *  - `beginStructure()`
  *
  * A crucial requirement onto implementors is that both functors have the same domain: `in.dom == out.dom`.
  */
trait LinearConnector extends LinearModuleOperator with LinearConnectorDSL {
  val in: Functor
  val out: Functor

  lazy override val dom: Diagram = { // lazy such that implementors can first initialize `in` and `out`
    if (in.dom != out.dom) {
      throw ImplementationError("Domains of in and out functors of a linear connector must match. But we got " +
        s"in.dom == `${in.dom}` and out.dom == `${out.dom}`.")
    }
    in.dom
  }
  lazy override val cod: Diagram = { // lazy such that implementors can first initialize `in` and `out`
    // todo what value of cod should we choose?
    /* wrong, even for mere pushout:
    if (in.cod != out.cod) {
      throw ImplementationError("Codomains of in and out functors of a linear connector must match. But we got " +
        s"in.cod == `${in.cod}` and out.cod == `${out.cod}`.")
    }*/
    in.cod
  }

  /**
    * A natural transformation between [[in.cod]] and [[out.cod]], indexed by theory expressions over [[dom]].
    *
    * @param t A theory expression over [[dom]], i.e., [[dom.hasImplicitFrom(t)]] is true.
    * @return A morphism expression [[in.applyDomain(t)]] -> [[out.applyDomain(t)]].
    * @see [[applyDomainModule]] for the base case of [[OMMOD]]s referencing [[MPath theory paths]].
    */
  final override def applyDomain(t: Term): Term = t match {
    case OMMOD(m) /* ideally: only if m points to a theory */ => OMMOD(applyDomainModule(m))
    case _ => ???
  }

  final override def applyModuleExpression(m: Term): Term = m match {
    case OMMOD(m) => OMMOD(applyModulePath(m))
    case _ => ???
  }

  // restate `applyDomainModule()` without implementing to refine superclass' documentation
  /**
    * A natural transformation between [[in.cod]] and [[out.cod]], indexed by [[MPath theory paths]] over [[dom]].
    *
    * The induced transformation indexed by *theory expressions* is given by [[applyDomain]].
    *
    * @param m A theory path over [[dom]], i.e., [[dom.hasImplicitFrom(m)]] is true.
    * @return A path to a view [[in.applyDomainModule(m)]] -> [[out.applyDomainModule(m)]].
    */
  override def applyDomainModule(m: MPath): MPath

  /**
    * Creates a new output view that serves to contain the to-be-created assignments; called by
    * [[beginContainer]].
    *
    * You may override this method to implement additional action.
    *
    * @return The output view if the connector is applicable on `thy`. If it was and `Some(outView)` is returned, then
    *         this method must have called [[DiagramInterpreter.add]] on `outView` before returning.
    * @example Some connectors choose to add includes at the beginning of every output view.
    *          Those connectors can override [[beginTheory]] as follows:
    *          {{{
    *            override protected def beginTheory(thy: Theory)(implicit interp: DiagramInterpreter): Option[View] = {
    *              super.beginTheory(thy).map(view => {
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
    outView.setOrigin(GeneratedFrom(thy.path, this, None))
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
            mappedContainers += inTheory -> outView
            true

          case _ => false
        }

      // We accept structures, but don't create a special out container for them.
      // (Arguably the asymmetry stems from MMT that makes assignments to constants from structures
      //  be represented flatly in views.)
      case _: Structure => true
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
    * (In the last line, one path from the square of the commutativity of the natural transformation c  -)
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
    val ctrl = interp.ctrl
    implicit val library: Lookup = ctrl.library

    if (include.df.nonEmpty) // nothing to do
      return

    if (include.args.nonEmpty)
      throw new NotImplementedError("Parametric includes not supported by linear diagram operators yet")

    val (newFrom, newDf) = include.from match {
      case from if dom.hasImplicitFrom(from) =>
        // only create the actually necessary includes
        // TODO hacky work around here, see discussion at https://mattermost.kwarc.info/kwarc/pl/opp88dhc57g4zmhyfzr7gyqixr
        if (library.hasImplicit(applyDomain(OMMOD(from)), OMMOD(in.applyModulePath(container.path.toMPath))))
          return

        (in.applyDomainModule(from), applyDomain(OMMOD(from)))

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
    outputInclude.setOrigin(GeneratedFrom(structure.path, this, None))
    interp.add(outputInclude)
    interp.endAdd(outputInclude)
  }
}

/**
  * A [[LinearConnector]] from the identity functor to a given functor [[InwardsLinearConnector.out]].
  */
trait InwardsLinearConnector extends LinearConnector {
  val out: Functor
  lazy override val in: Functor = LinearFunctor.identity(out.dom) // lazy to allow implementors to first initialize `out`
}

/**
  * A [[LinearConnector]] from a given functor [[OutwardsLinearConnector.in]] to the identity functor.
  */
trait OutwardsLinearConnector extends LinearConnector {
  val in: Functor
  lazy override val out: Functor = LinearFunctor.identity(in.dom) // lazy to allow implementors to first initialize `in`
}

// todo(NR,FR) review this together
trait LinearConnectorDSL {
  this: LinearModuleOperator =>

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
