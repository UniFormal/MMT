package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.{OMCOMP, OMIDENT, OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Include, IncludeData, Structure}
import info.kwarc.mmt.api.{InvalidElement, MPath}

/**
  * Linearly connects diagrams output by two [[LinearModuleTransformer]] `in` and `out` with views.
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
  *  - `applyConstant()` (inherited as [[LinearTransformer.applyConstant()]])
  *  - `translationView()`
  *
  * and may override, among other methods, in particular
  *
  *  - `beginTheory()`
  *  - `beginStructure()`
  *
  * @example In universal algebra, we can create the [[LinearModuleTransformer]] `Sub(-)`
  *          that maps an SFOL-theory `X` to its SFOL-theory of substructures `Sub(X)`.
  *          But we can do more: for every mapped `X` we desire a view `sub_model: X -> Sub(X)`
  *          that realizes models of `Sub(X)` (i.e. submodels of X-models) as models of `X` (i.e. models
  *          of `X`) via predicate subtypes.
  *          Note that creation of the connecting view is still linear in `X`.
  *          You can use this trait to realzie exactly the creation of the `sub_model` connecting views.
  *
  * Invariants so far:
  *
  *  - in and out have same domain/codomain
  *  - applyDeclaration outputs declarations valid in a view (esp. for output FinalConstants that means they
  *    have a definiens)
  */
trait LinearConnectorTransformer extends LinearModuleTransformer with RelativeBaseTransformer {
  val in: RelativeBaseTransformer with ModulePathTransformer
  val out: RelativeBaseTransformer with ModulePathTransformer

  // declare next two fields lazy, otherwise default initialization order entails in being null
  // see https://docs.scala-lang.org/tutorials/FAQ/initialization-order.html
  final override lazy val operatorDomain: Diagram = in.operatorDomain // == out.operatorDomain ideally
  final override lazy val operatorCodomain: Diagram = out.operatorCodomain // == in.operatorCodomain ideally

  // doing this just in the Scala object would throw hard-to-debug "Exception at Initialization" errors
  private var hasRunSanityCheck = false

  private def sanityCheckOnce()(implicit interp: DiagramInterpreter): Unit = {
    if (hasRunSanityCheck) {
      return
    }
    hasRunSanityCheck = true
    sanityCheck()
  }

  /**
    * Runs a sanity check for whether [[in]] and [[out]] are actually "connectible" operators.
    *
    * The sanity check is only run once in the entire lifetime of ''this''.
    *
    * Subclasses may override and extend this method. Call ''super.sanityCheck()'' in those cases.
    */
  protected def sanityCheck()(implicit interp: DiagramInterpreter): Unit = {
    if (in.operatorDomain != out.operatorDomain) {
      // todo:
      // throw ImplementationError(s"Can only connect between two LinearModuleTransformers with same domain, got ${in.operatorDomain} and ${out.operatorDomain} for in and out, respectively.")
    }
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
  protected def beginTheory(thy: Theory, state: LinearState)(implicit interp: DiagramInterpreter): Option[View] = {
    val outPath = applyModulePath(thy.path)

    val outView = View(
      outPath.doc, outPath.name,
      from = OMMOD(in.applyModulePath(thy.path)),
      to = OMMOD(out.applyModulePath(thy.path)),
      isImplicit = false
    )
    interp.add(outView)

    Some(outView)
  }

  final override def beginContainer(inContainer: Container, state: LinearState)(implicit interp: DiagramInterpreter): Boolean = {
    sanityCheckOnce()
    inContainer match {
      // only applicable on theories and their contents
      case _: View => false

      case inTheory: Theory =>
        beginTheory(inTheory, state) match {
          case Some(outView) =>
            state.diagramState.processedElements.put(inTheory.path, outView)
            interp.addToplevelResult(outView)
            state.outContainer = outView
            true

          case _ => false
        }

      // We accept structures, but don't create a special out container for them.
      // (Arguably the asymmetry stems from MMT that makes assignments to constants from structures
      //  be represented flat in views.)
      case _: Structure =>
        // to fulfill invariants of other code snippets, we have to put something
        // arbitrary into processedElements
        state.diagramState.processedElements.put(inContainer.path, inContainer)
        // TODO: fulfill method contract and set `state.outContainer = ???`
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
  final override def applyIncludeData(include: IncludeData, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    val ctrl = interp.ctrl // shorthand
    implicit val library: Lookup = ctrl.library
    implicit val diagramState: DiagramState = state.diagramState

    if (include.args.nonEmpty) {
      // unsure what to do
      ???
    }

    val newFrom: MPath = include.from match {
      case p if operatorDomain.hasImplicitFrom(p) =>
        applyMetaModule(OMMOD(p)).toMPath

      case from if diagramState.seenModules.contains(from) =>
        applyModule(ctrl.getModule(from))
        state.inherit(diagramState.getLinearState(from))

        in.applyModulePath(from)

      case _ =>
        interp.errorCont(InvalidElement(container, "Cannot handle include (or structure) of " +
          s"`${include.from}`: unbound in input diagram"))
        return
    }

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
    }

    val outputInclude = Include.assignment(
      home = OMMOD(applyModulePath(container.path.toMPath)),
      from = newFrom,
      df = Some(newDf)
    )
    interp.add(outputInclude)
    interp.endAdd(outputInclude)
  }
}
