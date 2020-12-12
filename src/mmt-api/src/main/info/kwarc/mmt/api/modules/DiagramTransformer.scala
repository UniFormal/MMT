package info.kwarc.mmt.api.modules

/**
  * Traits for "anonymous" functorial operators, i.e., operators that are not bound
  * to an MMT symbol.
  * Anonymous operators are useful, e.g., for [[ParametricLinearOperator]]s that create them
  * on-the-fly at runtime based on the parameters they receive.
  * The main trait for linear operators is [[LinearModuleTransformer]].
  *
  * @see FunctorialOperator.scala for named diagram operators.
  */

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMA, OMCOMP, OMIDENT, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, Include, IncludeData, RuleConstant, Structure, TermContainer}
import info.kwarc.mmt.api.{ComplexStep, ContainerElement, GeneralError, ImplementationError, InvalidElement, LocalName, MPath}

trait ModulePathTransformer {
  protected def applyModuleName(name: LocalName): LocalName

  final def applyModulePath(mpath: MPath): MPath = {
    if (mpath == mpath.mainModule) {
      mpath.doc ? applyModuleName(mpath.name)
    } else {
      val newMPath = applyModulePath(mpath.mainModule)
      newMPath.doc ? LocalName(newMPath.name.steps ::: mpath.name.drop(1))
    }
  }
}

/**
  * Transforms modules to modules in a diagram.
  *
  * Base class of whole trait hierarchy of [[LinearModuleTransformer]]s and [[LinearConnectorTransformer]]s.
  *
  * In contrast to the somewhat parallel class hierarchy of [[FunctorialOperator]]s, [[LinearOperator]]s, and
  * [[LinearConnector]]s, this trait hierarchy only concerns itself with the functional behavior of
  * operators/connectors and *not* with parsing input diagram expressions (which are [[Term]]s) and constructing
  * output diagram expressions.
  * Especially, all traits in this hierarchy are *not* associated with any MMT symbol.
  * In contrast, [[FunctorialOperator]] implements [[SyntaxDrivenRule]].
  */
trait FunctorialTransformer extends FunctorialOperatorState with ModulePathTransformer {
  /**
    * Applies a module.
    *
    * Invariants:
    *
    *  - m.isInstanceOf[Theory] => applyModule(m).isInstanceOf[Theory] (actually not any longer?)
    *  - m.isInstanceOf[View] => applyModule(m).isInstanceOf[View] (actually not any longer?)
    *  - post condition: returned module is added
    *    - (a) to state.processedModules
    *    - (b) to [[DiagramInterpreter.addResult() interp.addResult()]]
    *
    * take care not not needlessly compute, check state.processedModules first:
    * state.processedModules.get(m.path).foreach(return _)
    */
  def applyModule(m: Module)(implicit interp: DiagramInterpreter, state: DiagramState): Option[Module]

  final def applyDiagram(modulePaths: List[MPath])(implicit interp: DiagramInterpreter): Option[List[MPath]] = {
    val modules: Map[MPath, Module] = modulePaths.map(p => (p, interp.ctrl.getModule(p))).toMap
    val state = initDiagramState(modules, interp)

    val newModulePaths = modulePaths.flatMap(modulePath => {
      applyModule(interp.get(modulePath))(interp, state).map(newModule => {

        state.processedElements.get(modulePath) match {
          case Some(`newModule`) => // ok
          case Some(m) if m != newModule =>
            throw new Exception("...") // todo: reasonable error message

          case None =>
            throw new Exception("...") // todo: reasonable error message
        }
        if (!interp.hasToplevelResult(newModule.path)) {
          throw GeneralError("diagram operators' applyModule should insert resulting module to DiagramInterpreter")
        }

        newModule.path
      })
    })
    // todo: instead get new module paths from interp?
    Some(newModulePaths)
  }
}

/**
  * Linearly transforms modules to modules in a diagram.
  *
  * It is left to implementations how the type of input module relates to the type of output module:
  * for instance, the implementation [[LinearOperator]]s guarantees that applyModule() transforms
  * theories to theories and views to views.
  * On the other hand, the implementation (TODO: mention connectors) transforms theories to (diagram-connecting)
  * views and transforms views not at all.
  */
trait LinearTransformer extends FunctorialTransformer with LinearOperatorState {
  type Container = ModuleOrLink

  protected def applyContainerBegin(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Boolean

  /**
    *  - pre-condition: only called if applyContainerBegin returned true before
    *  - post-condition:
    *    - if inContainer was a top-level module in the diagram, then state.processedElements must
    *      contain an entry (inContainer.path, outContainer)
    *    - for inContainer fulfilling [[DiagramInterpreter.hasToplevelResult()]]
    *     outContainer must be a [[Module]] and be added via [[DiagramInterpreter.addToplevelResult()]].
    */
  protected def applyContainerEnd(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Unit

  /**
    * Post-condition: all added declaration added to ''interp''
    * (i.e. called [[DiagramInterpreter.add()]] *and* [[DiagramInterpreter.endAdd()]]).
    */
  protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit interp: DiagramInterpreter, state: DiagramState): Unit

  /**
    * Post-condition: all added declaration added to ''interp''
    * (i.e. called [[DiagramInterpreter.add()]] *and* [[DiagramInterpreter.endAdd()]]).
    */
  protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: DiagramState): Unit

  final def applyModule(inModule: Module)(implicit interp: DiagramInterpreter, state: DiagramState): Option[Module] = {
    if (applyContainer(inModule)) {
      state.processedElements.get(inModule.path) match {
        case None =>
          interp.errorCont(ImplementationError(s"Diagram operator $getClass did signal it did processing on ${inModule.path}, but actually did not register anything there."))
          None

        case Some(m: Module) => Some(m)
        case Some(x) => throw ImplementationError(s"Diagram operator $getClass transformed toplevel module $inModule to something else, namely $x")
      }
    } else {
      None
    }
  }

  /**
    * @return true if element was processed (or alraedy had been processed), false otherwise.
    */
  final protected def applyContainer(inContainer: Container)(implicit interp: DiagramInterpreter, state: DiagramState): Boolean = {
    if (state.processedElements.contains(inContainer.path)) {
      return true
    }

    val inLinearState = state.initAndRegisterNewLinearState(inContainer)

    if (!applyContainerBegin(inContainer, inLinearState)) {
      return false
    }

    inContainer.getDeclarations.foreach(decl => {
      inLinearState.registerDeclaration(decl)
      decl match {
        case Include(includeData) => applyIncludeData(inContainer, inLinearState, includeData)
        case s: Structure => applyStructure(inContainer, inLinearState, s)
        case decl: Declaration => applyDeclaration(inContainer, inLinearState, decl)
      }
    })

    applyContainerEnd(inContainer, inLinearState)
    true
  }

  /**
    * Post-condition: all added declaration added to ''interp''
    * (i.e. called [[DiagramInterpreter.add()]] *and* [[DiagramInterpreter.endAdd()]]).
    */
  protected def applyStructure(container: Container, containerState: LinearState, s: Structure)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {
    if (applyContainer(s)) {
      // todo: should actually register all (also unmapped) declarations from
      //   structure's domain theory, no?

      val inducedDeclarationPaths = s.getDeclarations.map(_.path).flatMap(p => {
        if (p.name.tail.nonEmpty) {
          Some(s.path / p.name.tail)
        } else {
          None
        }
      })
      val inducedDeclarations = inducedDeclarationPaths.map(interp.ctrl.getAs(classOf[Declaration], _))
      inducedDeclarations.foreach(containerState.registerDeclaration)
    }
  }

  // DSL
  object NotApplicable {
    def apply[T](c: Declaration, msg: String = "")(implicit interp: DiagramInterpreter, state: LinearState): List[T] = {
      state.registerSkippedDeclaration(c)
      interp.errorCont(InvalidElement(
        c,
        s"${LinearTransformer.this.getClass.getSimpleName} not applicable" +
          (if (msg.nonEmpty) ": " + msg else "")
      ))

      Nil
    }

    object Module {
      def apply(m: Module, msg: String)(implicit interp: DiagramInterpreter): Unit = {
        interp.errorCont(InvalidElement(
          m,
          s"${LinearTransformer.this.getClass.getSimpleName} not applicable" +
            (if (msg.nonEmpty) ": " + msg else "")
        ))
      }
    }
  }
}

trait RelativeBaseTransformer {
  val operatorDomain: MPath
  val operatorCodomain: MPath
}

/**
  * Linearly transforms theories to theories and views to views.
  *
  * A small abstraction over [[LinearOperator]]s that on top perform some parsing of the input diagram
  * expression and construction of the output diagram expression.
  */
trait LinearModuleTransformer extends LinearTransformer with RelativeBaseTransformer {
  final override protected def applyContainerBegin(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Boolean = {
    val outContainer = inContainer match {
      case inModule: Module =>
        if (!diagState.seenModules.contains(inModule.path)) {
          NotApplicable.Module(inModule, "unbound module not in input diagram")
          return false
        }

        val outPath = applyModulePath(inContainer.path.toMPath)
        val outModule = inModule match {
          case thy: Theory =>
            val newMeta = thy.meta.map {
              case mt if interp.ctrl.globalLookup.hasImplicit(mt, operatorDomain) =>
                operatorCodomain
              case mt =>
                if (applyModule(interp.ctrl.getModule(mt)).isEmpty) {
                  interp.errorCont(InvalidElement(thy, s"Theory had meta theory `$mt` for which there " +
                    s"was no implicit morphism into `$operatorDomain`. Recursing into meta theory as usual " +
                    s"failed, too; reasons are probably logged above."))
                  return false
                }
                applyModulePath(mt)
            }
            Theory.empty(outPath.doc, outPath.name, newMeta)

          case view: View =>
            if (applyModule(interp.ctrl.getModule(view.from.toMPath)).isEmpty) {
              return false
            }
            if (applyModule(interp.ctrl.getModule(view.to.toMPath)).isEmpty) {
              return false
            }

            containerState.inherit(diagState.getLinearState(view.to.toMPath))

            View(
              outPath.doc, outPath.name,
              OMMOD(applyModulePath(view.from.toMPath)), OMMOD(applyModulePath(view.to.toMPath)),
              view.isImplicit
            )
        }

        diagState.seenModules += inModule.path

        if (diagState.inputToplevelModules.contains(inModule.path)) {
          interp.addToplevelResult(outModule)
        }

        outModule

      case s: Structure => s.tp match {
        case Some(OMMOD(structureDomain)) =>
          if (!applyContainer(interp.ctrl.getModule(structureDomain))) {
            return false
          }

          // inherit linear state from module where structure is declared
          containerState.inherit(diagState.getLinearState(s.home.toMPath))

          // TODO: s.dfC is thrown away
          new Structure(
            home = OMMOD(applyModulePath(s.path.module)),
            name = s.name,
            TermContainer.asAnalyzed(OMMOD(applyModulePath(structureDomain))), TermContainer.empty(),
            s.isImplicit, s.isTotal
          )
        case _ => ???
      }
    }

    interp.add(outContainer)
    diagState.processedElements.put(inContainer.path, outContainer)

    true
  }

  final override protected def applyContainerEnd(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Unit = {
    val outContainer = diagState.processedElements(inContainer.path).asInstanceOf[ContainerElement[_]]
    interp.endAdd(outContainer)
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
  final override protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {
    val ctrl = interp.ctrl

    if (include.args.nonEmpty) ???

    val newFrom: MPath = include.from match {
      case `operatorDomain` => operatorCodomain.toMPath

      // classic case for include preserving behavior of linear operators
      case from if state.seenModules.contains(from) =>
        applyModule(ctrl.getModule(from))

        if (container.isInstanceOf[Theory]) {
          state.getLinearState(container.path).inherit(state.getLinearState(from))
        }

        applyModulePath(from)

      case from if ctrl.globalLookup.hasImplicit(OMMOD(from), OMMOD(operatorDomain)) =>
        // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
        //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
        //      but in general it might be something else
        //
        // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
        from

      case _ =>
        interp.errorCont(InvalidElement(container, "Cannot handle include (or structure) of " +
          s"`${include.from}`: unbound in input diagram"))
        return
    }

    val newDf: Option[Term] = include.df.map {
      case OMIDENT(`operatorDomain`) => OMIDENT(OMMOD(operatorCodomain))
      case OMIDENT(OMMOD(thy)) if state.seenModules.contains(thy) => OMIDENT(OMMOD(applyModulePath(thy)))
      case OMIDENT(thy) if ctrl.globalLookup.hasImplicit(thy, OMMOD(operatorDomain)) =>
        // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
        //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
        //      but in general it might be something else
        //
        // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
        OMIDENT(thy)

      case OMMOD(dfPath) if state.seenModules.contains(dfPath) =>
        // ???: error: morphism provided as definiens to include wasn't contained in diagram
        applyModule(ctrl.getModule(dfPath))

        if (container.isInstanceOf[View]) {
          state.getLinearState(container.path).inherit(state.getLinearState(dfPath))
        }

        OMMOD(applyModulePath(dfPath))
      case _ =>  ???
    }

    val s = Structure(
      home = OMMOD(applyModulePath(container.modulePath)),
      name = LocalName(newFrom),
      from = OMMOD(newFrom),
      df = newDf,
      isImplicit = if (container.isInstanceOf[Theory]) true else false,
      isTotal = include.total
    )
    interp.add(s)
    interp.endAdd(s)
  }
}

class IdentityLinearTransformer(private val domain: MPath) extends LinearModuleTransformer with DefaultLinearStateOperator {
  override val operatorDomain: MPath = domain
  override val operatorCodomain: MPath = domain

  override protected def applyModuleName(name: LocalName): LocalName = name

  final override protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {}
}

/**
  *
  * invariants so far:
  *
  * - in and out have same domain/codomain
  * - applyDeclaration outputs declarations valid in a view (esp. for output FinalConstants that means they
  *   have a definiens)
  */
trait LinearConnectorTransformer extends LinearTransformer with RelativeBaseTransformer {
  val in: LinearModuleTransformer
  val out: LinearModuleTransformer

  // declare next two fields lazy, otherwise default initialization order entails in being null
  // see https://docs.scala-lang.org/tutorials/FAQ/initialization-order.html
  final override lazy val operatorDomain: MPath = in.operatorDomain
  final override lazy val operatorCodomain: MPath = in.operatorCodomain

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

  final override protected def applyContainerBegin(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: LinearDiagramState): Boolean = {
    sanityCheckOnce()
    inContainer match {
      // only applicable on theories and their contents
      case _: View => false

      // we accept structures, but don't create a special out container for them
      case _: Structure => true

      case thy: Theory =>
        val outPath = applyModulePath(thy.path)

        val outContainer = View(
          outPath.doc, outPath.name,
          from = OMMOD(in.applyModulePath(thy.path)),
          to = OMMOD(out.applyModulePath(thy.path)),
          isImplicit = false
        )

        interp.addToplevelResult(outContainer)
        diagState.processedElements.put(inContainer.path, outContainer)
        true
    }
  }

  final override protected def applyContainerEnd(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: LinearDiagramState): Unit = {
    // be careful with accessing in processedElements
    // in applyContainerBegin, e.g. for structures we didn't add anything to processedElements
    diagState.processedElements.get(inContainer.path)
      .map(_.asInstanceOf[ContainerElement[_]])
      .foreach(interp.endAdd)
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
    * (In the last line, one path from the square of the commutativity of the natural transformation conn(-)
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
  final override protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: LinearDiagramState): Unit = {
    val ctrl = interp.ctrl // shorthand

    if (include.args.nonEmpty) {
      // unsure what to do
      ???
    }

    val newFrom: MPath = include.from match {
      case p if p == in.operatorDomain =>
        in.operatorCodomain //, OMIDENT(OMMOD(in.operatorCodomain)))

      case from if state.seenModules.contains(from) =>
        applyModule(ctrl.getModule(from))
        state.getLinearState(container.path).inherit(state.getLinearState(from))

        in.applyModulePath(from) //, OMMOD(applyModulePath(include.from)))

      case _ =>
        interp.errorCont(InvalidElement(container, "Cannot handle include (or structure) of " +
          s"`${include.from}`: unbound in input diagram"))
        return
    }

    val newDf: Term = include.df.getOrElse(OMIDENT(OMMOD(include.from))) match {
      case OMMOD(v) if state.seenModules.contains(v) =>
        // even though we, as a connector, don't act on views, for consistency, we call applyModule nonetheless
        applyModule(ctrl.getModule(v))
        // todo: in which order does OMCOMP take its arguments? (Document this, too!)
        OMCOMP(OMMOD(out.applyModulePath(v)), OMMOD(applyModulePath(include.from)))

      case OMIDENT(OMMOD(thy)) if state.seenModules.contains(thy) =>
        OMMOD(applyModulePath(include.from))

      case OMIDENT(OMMOD(p)) if p == in.operatorDomain =>
        OMMOD(in.operatorCodomain)

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


/**
  * A [[LinearTransformer]] that works constant-by-constant: structural features are elaborated before calling
  * the to-be-implemented method ''applyConstant''.
  */
trait ElaboratingLinearTransformer extends LinearTransformer {
  protected def applyConstant(container: Container, c: Constant)(implicit interp: DiagramInterpreter, state: LinearState): Unit

  final override protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {
    decl match {
      case c: Constant => applyConstant(container, c)(interp, containerState)
      case _: RuleConstant =>
        NotApplicable(decl, "RuleConstants cannot be processed")(interp, containerState)
      case _ =>
        // do elaboration, then call applyConstant
        // interp.errorCont(InvalidElement(decl, s"Linear operator ${getClass} cannot process this element " +
        //s"of u"))
        ???
    }
  }
}

/**
  * Linearly transforms theories to theories and views to views,
  * while hiding much of the complexity of their contents.
  *
  * Implementors only need to give a ''applyConstantSimple'' method.
  */
trait SimpleLinearModuleTransformer extends LinearModuleTransformer
  with ElaboratingLinearTransformer with DefaultLinearStateOperator {
  type SimpleConstant = (LocalName, Term, Option[Term])

  /**
    *
    * @param c The constant
    * @param name A simplified version of ''c.name''. E.g. if c is an assignment in a view,
    *             ''c.name'' is something like ''LocalName(domainTheory) / actualConstantName'',
    *             which is uncomfortable to deal with in most cases.
    *             Hence in such cases, ''name'' will be simply be ''actualConstantName''.
    * @return In case the operator is not applicable on c, use the helper [[NotApplicable]] function:
    *         return ''NotApplicable(c, "optional error msg")''. Internally, this emits an error
    *         via interp.errorHandler and returns Nil.
    * @example Most operator implementations start like this:
    * {{{
    *   tp match {
    *     // more cases
    *     case _ =>
    *       NotApplicable(c)
    *   }
    * }}}
    *
    * The general convention is that operators produce as much output as possible.
    * For instance, consider an operator that copies every constant ''c'' to ''c^p'' and furthermore creates some ''c^x
    * in some fashion. Now the derivation of ''c^x'' might not be possible for some constants c. But even in these cases,
    * the opreator should output c (and signal inapplicability as explained above).
    *
    * @example Operators producing copies usually follow this pattern:
    * {{{
    *   val par : Renamer[LinearState] = getRenamerFor("p") // as a field on the operator object
    *   val parCopy = (par(name), par(tp), df.map(par(_)))
    *
    *   parCopy :: (tp match {
    *     // more cases
    *     case _ =>
    *       NotApplicable(c)
    *   })
    * }}}
    */
  protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[SimpleConstant]

  final override protected def applyConstant(container: Container, c: Constant)(implicit interp: DiagramInterpreter, state: LinearState): Unit = {
    // Since [[applyConstantSimple()]] takes a simplified name and outputs a simplified name again
    // we need to functions for simplifying and complexifying again:
    def simplifyName(name: LocalName) = container match {
      case _: Theory => name

      // view or structure
      case link: Link => name match {
        case LocalName(ComplexStep(mpath) :: domainSymbolName) if mpath == link.from.toMPath =>
          LocalName(domainSymbolName)
        case _ => name // fallback
      }
    }

    def complexifyName(name: LocalName) = container match {
      case _: Theory => name
      case link: Link => LocalName(link.from.toMPath) / name
    }

    val rawTp = c.tp.getOrElse({
      interp.errorCont(InvalidElement(c, s"Operator $getClass not applicable on constants without type component"))
      return
    })
    val rawDf = c.df

    val tp = interp.ctrl.globalLookup.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = rawDf.map(interp.ctrl.globalLookup.ExpandDefinitions(_, state.skippedDeclarationPaths))

    applyConstantSimple(container, c, simplifyName(c.name), tp, df).foreach {
      case (name, newTp, newDf) =>
        if (container.isInstanceOf[View] && newDf.isEmpty) {
          throw GeneralError(s"applyConstant of SimpleLinearOperator subclass ${this.getClass} returned empty definiens for view declaration ${c.path}")
        }

        interp.add(new FinalConstant(
          home = OMMOD(applyModulePath(container.modulePath)),
          name = complexifyName(name), alias = Nil,
          tpC = TermContainer.asAnalyzed(newTp), dfC = TermContainer.asAnalyzed(newDf),
          rl = None, notC = NotationContainer.empty(), vs = c.vs
        ))
    }
  }
}

trait SimpleLinearConnectorTransformer extends LinearConnectorTransformer with ElaboratingLinearTransformer with DefaultLinearStateOperator {

  /**
    * Maps a constant to a list of assignments in the connecting morphism.
    *
    * @return A list of assignments (simpleName, assignmentTerm), which is used by [[applyConstant]]
    *         to build a [[FinalConstant]] with the right name, empty type container, and a definiens container
    *         containing assignmentTerm.
    */
  protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)]

  final override protected def applyConstant(container: Container, c: Constant)(implicit interp: DiagramInterpreter, state: LinearState): Unit = {
    val rawTp = c.tp.getOrElse({
      interp.errorCont(GeneralError(s"Operator $getClass not applicable on constants without type component"))
      return
    })
    val rawDf = c.df

    val tp = interp.ctrl.globalLookup.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = rawDf.map(interp.ctrl.globalLookup.ExpandDefinitions(_, state.skippedDeclarationPaths))

    applyConstantSimple(container, c, c.name, tp, df).foreach {
      case (name, df) =>
        interp.add(new FinalConstant(
          home = OMMOD(applyModulePath(container.modulePath)),
          name = name, alias = Nil,
          tpC = TermContainer.empty(), dfC = TermContainer.asAnalyzed(df),
          rl = None, notC = NotationContainer.empty(), vs = c.vs
        ))
    }
  }
}
