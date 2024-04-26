package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{Module, ModuleOrLink, Theory, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMCOMP, OMIDENT, OMMOD, Term}
import info.kwarc.mmt.api.symbols._

trait LinearOperators {
  val ops: LinearOperatorSpecs

  private val self = this

  sealed case class LinearOperatorSpecs(specs: List[LinearOperatorSpec]) {
    private val keyedSpecs: Map[String, LinearOperatorSpec] =
      specs.filter(_.key.nonEmpty).map(op => op.key.get -> op).toMap

    def apply(key: String): LinearOperatorSpec =
      keyedSpecs.getOrElse(key, throw ImplementationError(s"Implementor of $self tried retrieving an operator in " +
        s"operator spec with unknown key."))
  }

  abstract class LinearOperatorSpec(val key: Option[String], val renamer: MPath => MPath) {
    def apply(m: MPath): MPath = renamer(m)
    def apply(n: LocalName)(implicit interp: DiagramInterpreter): LocalName
  }
  sealed trait UnresolvedLinearOperatorSpec {
    val key: Option[String]
    val renamer: MPath => MPath
  }

  def resolve(u: List[UnresolvedLinearOperatorSpec]): LinearOperatorSpecs = {
    LinearOperatorSpecs(u.map {
      case op@LinearFunctorSpec(_, _, _, _, _) => op
      case UnresolvedLinearConnectorSpec(key, dom, cod, metaConnector, renamer) =>
        def resolve_(x: Either[String, LinearFunctorSpec]): LinearFunctorSpec = x match {
          case Left(searchKey) => u.find(_.key.contains(searchKey)).get.asInstanceOf[LinearFunctorSpec]
          case Right(givenOp) => givenOp
        }

        LinearConnectorSpec(key, resolve_(dom), resolve_(cod), metaConnector, renamer)
    })
  }

  case class LinearFunctorSpec(override val key: Option[String],
                               dom: Diagram,
                               cod: Diagram,
                               metaFunctor: DiagramFunctor,
                               override val renamer: MPath => MPath)
    extends LinearOperatorSpec(key, renamer) with UnresolvedLinearOperatorSpec {

    private lazy val equinamer = getEquinamer(this)
    override def apply(n: LocalName)(implicit interp: DiagramInterpreter): LocalName = equinamer(n)
    def applyDomain(m: MPath): MPath = metaFunctor(m).toMPath
  }

  private def transformModulePath(f: SimpleStep => SimpleStep): MPath => MPath = m => {
    val newName = LocalName(m.name.steps.map {
      case x@SimpleStep(_) => f(x)
      case ComplexStep(path) => ComplexStep(transformModulePath(f)(path)) // todo: is this correct for nested modules?
    })
    m.doc ? newName
  }

  def prefixBy(str: String): MPath => MPath = transformModulePath {
    case SimpleStep(name) => SimpleStep(str + name)
  }

  def suffixBy(str: String): MPath => MPath = transformModulePath {
    case SimpleStep(name) => SimpleStep(name + str)
  }

  object IdentityFunctorSpec {
    def apply(dom: Diagram): LinearFunctorSpec = LinearFunctorSpec(None, dom, dom, DiagramFunctor.identity(dom), x => x)
  }
  case class LinearConnectorSpec(override val key: Option[String],
                                 dom: LinearFunctorSpec,
                                 cod: LinearFunctorSpec,
                                 metaConnector: DiagramConnection,
                                 override val renamer: MPath => MPath) extends LinearOperatorSpec(key, renamer) {
    override def apply(n: LocalName)(implicit interp: DiagramInterpreter): LocalName = ???

    // this needs to return a Term (instead of a mere MPath) to allow for constructs of the form OMIDENT(thy)
    def applyDomain(m: MPath): Term = metaConnector.applyTheory(m)
  }

  case class UnresolvedLinearConnectorSpec(override val key: Option[String],
                                           dom: Either[String, LinearFunctorSpec],
                                           cod: Either[String, LinearFunctorSpec],
                                           metaConnector: DiagramConnection,
                                           override val renamer: MPath => MPath) extends UnresolvedLinearOperatorSpec

  /** DSL BEGIN **/

  implicit def functorTupleToSpec(x: (String, (Diagram, Diagram), DiagramFunctor, MPath => MPath)): LinearFunctorSpec =
    LinearFunctorSpec(Some(x._1), x._2._1, x._2._2, x._3, x._4)

  implicit def connectorTupleToSpec1(x: (String, (LinearFunctorSpec, LinearFunctorSpec), DiagramConnection, MPath => MPath)): UnresolvedLinearConnectorSpec =
    UnresolvedLinearConnectorSpec(Some(x._1), dom = Right(x._2._1), cod = Right(x._2._2), x._3, x._4)

  implicit def connectorTupleToSpec2(x: (String, (LinearFunctorSpec, String), DiagramConnection, MPath => MPath)): UnresolvedLinearConnectorSpec =
    UnresolvedLinearConnectorSpec(Some(x._1), dom = Right(x._2._1), cod = Left(x._2._2), x._3, x._4)

  implicit def connectorTupleToSpec3(x: (String, (String, LinearFunctorSpec), DiagramConnection, MPath => MPath)): UnresolvedLinearConnectorSpec =
    UnresolvedLinearConnectorSpec(Some(x._1), dom = Left(x._2._1), cod = Right(x._2._2), x._3, x._4)

  implicit def connectorTupleToSpec4(x: (String, (String, String), DiagramConnection, MPath => MPath)): UnresolvedLinearConnectorSpec =
    UnresolvedLinearConnectorSpec(Some(x._1), dom = Left(x._2._1), cod = Left(x._2._2), x._3, x._4)

  def Id(m: MPath): LinearFunctorSpec = IdentityFunctorSpec(Diagram.singleton(m))

  trait ConstantRenamer {
    def apply(n: LocalName)(implicit interp: DiagramInterpreter): LocalName
  }


  def getRenamer(opKey: String)(f: String => String): ConstantRenamer = getRenamer(ops(opKey).asInstanceOf[LinearFunctorSpec])(f)
  def getRenamer(op: LinearFunctorSpec)(f: String => String): ConstantRenamer = new ConstantRenamer {
    override def apply(n: LocalName)(implicit interp: DiagramInterpreter): LocalName = LocalName(n.steps map {
      case SimpleStep(x) => SimpleStep(f(x))
      case ComplexStep(m) if op.dom.hasImplicitFrom(m)(interp.ctrl.library) =>
        ComplexStep(op.applyDomain(m))
      case ComplexStep(m) =>
        ComplexStep(op(m))
    })
  }
  def getEquinamer(op: LinearFunctorSpec): ConstantRenamer = getRenamer(op)(x => x)
  def getEquinamer(opKey: String): ConstantRenamer = getRenamer(opKey)(x => x)

  /** DSL END **/


  /**
    * invariant: idempotent, i.e., when called more than once on the same view, always returns the same modules (i.e., the objects in the Java sense).
    *
    * @return (modules, alreadyProcessed) where modules are the either newly created (but yet empty) or existing mapped modules and alreadyProcessed indicates whether _any_ of the operators has already processed the input module
    * @see [[beginView]]
    */
  protected def beginTheory(thy: Theory)(implicit interp: DiagramInterpreter): (List[ModuleOrLink], Boolean) = {
    var alreadyProcessed = false
    val modules = ops.specs map {
      case op@LinearFunctorSpec(_, dom, _, _, _) =>
        val newThyPath = op(thy.path)

        if (interp.ctrl.getAsO(classOf[Theory], newThyPath).nonEmpty) {
          alreadyProcessed = true
          interp.ctrl.getTheory(newThyPath)
        } else {
          val newMeta = thy.meta.map {
            case mt if op.dom.hasImplicitFrom(mt)(interp.ctrl.library) => // meta theory is subsumed by functor's domain
              op.applyDomain(mt)

            case mt => // otherwise, recurse into meta theory
              if (applyModule(interp.ctrl.getModule(mt)).isEmpty) {
                interp.errorCont(InvalidElement(thy, s"Theory had meta theory `$mt` for which there " +
                  s"was no implicit morphism into `$dom`. Recursing into meta theory as usual " +
                  s"failed, too; reasons are probably logged above. Keeping meta theory as-is."))
                mt
              } else {
                op(mt)
              }
          }

          val newThy = Theory.empty(newThyPath.doc, newThyPath.name, newMeta)
          newThy.setOrigin(GeneratedFrom(thy.path, this, None))
          interp.add(newThy)

          newThy
        }

      case op@LinearConnectorSpec(_, dom, cod, _, _) =>
        val newMorPath = op(thy.path)

        if (interp.ctrl.getAsO(classOf[Module], newMorPath).nonEmpty) {
          alreadyProcessed = true
          interp.ctrl.getModule(newMorPath)
        } else {
          val newMor = View(
            newMorPath.doc, newMorPath.name,
            from = OMMOD(dom(thy.path)),
            to = OMMOD(cod(thy.path)),
            isImplicit = false
          )
          newMor.setOrigin(GeneratedFrom(thy.path, this, None))
          interp.add(newMor)

          newMor
        }
    }
    (modules, alreadyProcessed)
  }

  /**
    * invariant: idempotent, i.e., when called more than once on the same view, always returns the same modules (i.e., the objects in the Java sense).
    *
    * @return (modules, alreadyProcessed) where modules are the either newly created (but yet empty) or existing mapped modules and alreadyProcessed indicates whether _any_ of the operators has already processed the input module
    * @see [[beginTheory]]
    */
  protected def beginView(view: View)(implicit interp: DiagramInterpreter): (List[View], Boolean) = {
    var alreadyProcessed = false
    val modules = ops.specs flatMap {
      case op@LinearFunctorSpec(_, _, _, _, _) =>
        if (applyModule(interp.ctrl.getModule(view.from.toMPath)).isEmpty) {
          None
        } else if (applyModule(interp.ctrl.getModule(view.to.toMPath)).isEmpty) {
          None
        } else {
          val newMorPath = op(view.path)

          if (interp.ctrl.getAsO(classOf[View], newMorPath).nonEmpty) {
            alreadyProcessed = true
            Some(interp.ctrl.getAs(classOf[View], newMorPath))
          } else {
            val newMor = View(
              newMorPath.doc, newMorPath.name,
              OMMOD(op(view.from.toMPath)), OMMOD(op(view.to.toMPath)),
              view.isImplicit
            )

            newMor.setOrigin(GeneratedFrom(view.path, this, None))
            interp.add(newMor)
            Some(newMor)
          }
        }

      case LinearConnectorSpec(_, _, _, _, _) =>
        // todo(diagop-mor-equality): theoretically, we could output morphism equality
        //                            but the MMT system does not yet support native statements of morphism equality
        None
    }

    (modules, alreadyProcessed)
  }

  /**
    * Applies the operators to a module.
    * This method is idempotent, calling it more than once will result in returning of the existing result.
    */
  final def applyModule(m: Module)(implicit interp: DiagramInterpreter): List[ModuleOrLink] = {
    // force elaboration on input module (among other things, this makes sure the implicit graph
    // related to inModule gets constructed)
    interp.ctrl.simplifier(m)

    val (addedModules, alreadyProcessed) = m match {
      case thy: Theory =>
        beginTheory(thy)
      case v: View =>
        beginView(v)
    }

    if (!alreadyProcessed) {
      m.getDeclarations.foreach(d => applyDeclaration(d, m))
      // finish up modules (esp. handle includes that might have been added by the last line)
      addedModules.foreach(interp.endAdd)
    }
    addedModules
  }

  def applyDeclaration(d: Declaration, container: ModuleOrLink)(implicit interp: DiagramInterpreter): Unit = d match {
    case c: Constant => applyConstant(c, container)

    // TODO(NR@anyone): can we unify cases for plain includes and structures?
    case s @ Include(includeData) => applyIncludeData(includeData, s.asInstanceOf[Structure], container)
    case s: Structure => applyStructure(s, container) // must come after Include case
    case _ =>
      interp.errorCont(InvalidElement(d, s"Linear operator `$getClass` not applicable on this kind " +
          s"of declaration."))
  }

  def applyIncludeData(include: IncludeData, structure: Structure, container: ModuleOrLink)(implicit interp: DiagramInterpreter): Unit = {
    val ctrl = interp.ctrl
    implicit val library: Lookup = ctrl.library

    // general behavior:
    //   ignore includes that go above the domain operator's meta diagram
    //   e.g., if we have a linear functor op: {LF} -> {S}, i.e., on the singleton diagram containing the LF theory
    //   to the singleton diagram containing some theory S, then when op processes theories like
    //   T = {include LF, include Lambda, ...}, it will simply ignore `include Lambda` and do nothing on that
    //   particular declaration.

    ops.specs foreach {
      case op@LinearFunctorSpec(_, dom, _, _, _) =>
        if (include.args.nonEmpty)
          throw new NotImplementedError("Parametric includes not supported by linear diagram operators yet")

        def handleFrom(from: MPath): Option[Term] = {
          if (dom.hasImplicitFrom(from)) {
            op.metaFunctor.functor.get(from) // in case of None: will lead to current include being ignored below
          }
          else { // recurse into include's domain module
            applyModule(ctrl.getModule(from))
            // todo(diagop-state): inheritState(container.modulePath, from)
            Some(OMMOD(op(from)))
          }
        }

        def handleDf(t: Term): Term = t match {
          case OMCOMP(mors) => OMCOMP(mors.map(handleDf))
          case OMIDENT(t) => OMIDENT(handleDf(t))
          case _ => ???
        }

        handleFrom(include.from).foreach(newFrom => {
          val newDf = include.df.map(handleDf)

          val s = Structure(
            home = OMMOD(op(container.modulePath)),
            name = LocalName(newFrom.toMPath),
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
        })

      case op@LinearConnectorSpec(_, dom, _, _, _) =>
        if (include.df.nonEmpty) {
          // todo(diagop-mor-equality): theoretically, we could output morphism equality
          //                            but the MMT system does not yet support native statements of morphism equality
        } else if (include.args.nonEmpty) {
          throw new NotImplementedError("Parametric includes not supported by linear diagram operators yet")
        } else {
          val from = include.from
          val newData: Option[(MPath, Term)] = if (dom.dom.hasImplicitFrom(from)) { // assumed invariant dom.dom == cod.dom
            dom.metaFunctor.functor.get(from).map(newFrom => {
              (newFrom.toMPath, op.applyDomain(from))
            }) // in case of None: will lead to current include being ignored below
          } else {
            // recurse into include domain
            applyModule(ctrl.getModule(from))
            // todo(diagop-state): inheritState(container.path, m.path)
            Some((dom(from), OMMOD(op(from))))
          }

          newData match {
            case Some((newFrom, newDf)) =>
              val newInclude = Include.assignment(
                home = OMMOD(op(container.path.toMPath)),
                from = newFrom,
                df = Some(newDf)
              )
              newInclude.setOrigin(GeneratedFrom(structure.path, this, None))
              interp.add(newInclude)
              interp.endAdd(newInclude)
              interp.endAdd(container)

            case None => // nothing to do
          }
        }
    }
  }

  def applyStructure(s: Structure, container: ModuleOrLink): Unit = {
    // todo: implement
  }

  def applyConstant(c: Constant, container: ModuleOrLink)(implicit interp: DiagramInterpreter): Unit
}

class NewPushout(m: Term, dom: MPath, cod: MPath) extends LinearOperators {
  override val ops: LinearOperatorSpecs = resolve(List(
    ("P", Diagram.singleton(dom) -> Diagram.singleton(cod), DiagramFunctor.singleton(dom, cod), suffixBy("_Pushout")),
    ("in", Id(dom) -> "P", DiagramConnection.Singleton(dom, cod, m), suffixBy("_Projection"))
  ))

  def applyConstant(c: Constant, container: ModuleOrLink)(implicit interp: DiagramInterpreter): Unit = {
    def tr(t: Term): Term = interp.ctrl.library.ApplyMorphs(t, OMMOD(ops("in")(c.parent)))

    val newC = new FinalConstant(
      home = OMMOD(ops("P")(c.parent)),
      name = ops("P")(c.name),
      alias = c.alias.map(ops("P")(_)),
      tpC = c.tpC.map(tr),
      dfC = c.dfC.map(tr),
      rl = c.rl,
      notC = NotationContainer.empty(),
      vs = c.vs
    )
    newC.metadata.add(c.metadata.getAll : _*) // TODO: shall we translate meta data via tr(), too?
    interp.add(newC)
    // Declaration.translate method

    val cAssignment = Constant(
      home = OMMOD(ops("in")(c.parent)),
      name = LocalName(ComplexStep(c.parent) :: c.name),
      alias = Nil,
      tp = c.tpC.map(tr).get,
      df = Some(newC.toTerm),
      rl = None
    )
    interp.add(cAssignment)
  }
}
