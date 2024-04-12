package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.{ComplexStep, GeneratedFrom, ImplementationError, InvalidElement, LocalName, MPath}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{Module, ModuleOrLink, Theory, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMCOMP, OMIDENT, OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, Include, IncludeData, Structure}


trait Blah {
  val ops: LinearOperatorSpecs

  private val self = this

  sealed case class LinearOperatorSpecs(specs: List[LinearOperatorSpec]) {
    private val keyedSpecs: Map[String, LinearOperatorSpec] =
      specs.filter(_.key.nonEmpty).map(op => op.key.get -> op).toMap

    def apply(key: String): LinearOperatorSpec =
      keyedSpecs.getOrElse(key, throw ImplementationError(s"Implementor of $self tried retrieving an operator in " +
        s"operator spec with unknown key."))
  }

  abstract class LinearOperatorSpec(val key: Option[String]) {
    def apply(m: MPath): MPath
  }
  sealed trait UnresolvedLinearOperatorSpec {
    val key: Option[String]
  }

  def resolve(u: List[UnresolvedLinearOperatorSpec]): LinearOperatorSpecs = {
    LinearOperatorSpecs(u.map {
      case op@LinearFunctorSpec(_, _, _, _) => op
      case UnresolvedLinearConnectorSpec(key, dom, cod, metaConnector) =>
        def resolve_(x: Either[String, LinearFunctorSpec]): LinearFunctorSpec = x match {
          case Left(searchKey) => u.find(_.key.contains(searchKey)).get.asInstanceOf[LinearFunctorSpec]
          case Right(givenOp) => givenOp
        }

        LinearConnectorSpec(key, resolve_(dom), resolve_(cod), metaConnector)
    })
  }

  case class LinearFunctorSpec(override val key: Option[String], dom: Diagram, cod: Diagram, metaFunctor: DiagramFunctor) extends LinearOperatorSpec(key) with UnresolvedLinearOperatorSpec {
    def applyDomain(m: MPath): MPath = metaFunctor(m).toMPath

    override def apply(m: MPath): MPath = m.doc ? m.name.suffixLastSimple(key.get)
  }
  object IdentityFunctorSpec {
    def apply(dom: Diagram): LinearFunctorSpec = new LinearFunctorSpec(None, dom, dom, DiagramFunctor.identity(dom)) {
      override def apply(m: MPath): MPath = m
    }
  }
  case class LinearConnectorSpec(override val key: Option[String], dom: LinearFunctorSpec, cod: LinearFunctorSpec, metaConnector: DiagramConnection) extends LinearOperatorSpec(key) {
    // this needs to return a Term (instead of a mere MPath) to allow for constructs of the form OMIDENT(thy)
    def applyDomain(m: MPath): Term = metaConnector.applyTheory(m)

    override def apply(m: MPath): MPath = m.doc ? m.name.suffixLastSimple(key.get)
  }

  case class UnresolvedLinearConnectorSpec(override val key: Option[String], dom: Either[String, LinearFunctorSpec], cod: Either[String, LinearFunctorSpec], metaConnector: DiagramConnection) extends UnresolvedLinearOperatorSpec

  implicit def any2ArrowAssoc(x: (String, (Diagram, Diagram), DiagramFunctor)): LinearFunctorSpec =
    LinearFunctorSpec(Some(x._1), x._2._1, x._2._2, x._3)

  implicit def anyasd(x: (String, (LinearFunctorSpec, String), DiagramConnection)): UnresolvedLinearConnectorSpec =
    UnresolvedLinearConnectorSpec(Some(x._1), dom = Right(x._2._1), cod = Left(x._2._2), x._3)

  def Id(m: MPath): LinearFunctorSpec = IdentityFunctorSpec(Diagram.singleton(m))



  protected def beginTheory(thy: Theory)(implicit interp: DiagramInterpreter): List[ModuleOrLink] = {
    ops.specs map {
      case op@LinearFunctorSpec(_, dom, cod, _) =>
        val newThyPath = op(thy.path)
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

      case op@LinearConnectorSpec(_, dom, cod, _) =>
        val newMorPath = op(thy.path)

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

  protected def beginView(view: View)(implicit interp: DiagramInterpreter): List[View] = {
    ops.specs flatMap {
      case op@LinearFunctorSpec(_, dom, cod, _) =>
        if (applyModule(interp.ctrl.getModule(view.from.toMPath)).isEmpty) {
          None
        } else if (applyModule(interp.ctrl.getModule(view.to.toMPath)).isEmpty) {
          None
        } else {
          val newMorPath = op(view.path)
          val newMor = View(
            newMorPath.doc, newMorPath.name,
            OMMOD(op(view.from.toMPath)), OMMOD(op(view.to.toMPath)),
            view.isImplicit
          )

          newMor.setOrigin(GeneratedFrom(view.path, this, None))
          interp.add(newMor)
          Some(newMor)
        }

      case LinearConnectorSpec(_, _, _, _) =>
        // todo(diagop-mor-equality): theoretically, we could output morphism equality
        //                            but the MMT system does not yet support native statements of morphism equality
        None
    }
  }

  final def applyModule(m: Module)(implicit interp: DiagramInterpreter): List[ModuleOrLink] = {
    // force elaboration on input module (among other things, this makes sure the implicit graph
    // related to inModule gets constructed)
    interp.ctrl.simplifier(m)

    val addedModules = m match {
      case thy: Theory =>
        beginTheory(thy)
      case v: View =>
        beginView(v)
    }
    m.getDeclarations.foreach(d => applyDeclaration(d, m))

    addedModules.foreach(interp.ctrl.endAdd)
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

    ops.specs foreach {
      case op@LinearFunctorSpec(_, dom, _, _) =>
        if (include.args.nonEmpty)
          throw new NotImplementedError("Parametric includes not supported by linear diagram operators yet")

        def handleFrom(from: MPath): Term = {
          if (dom.hasImplicitFrom(from)) OMMOD(op.applyDomain(from))
          else {
            // recurse into include's domain module
            applyModule(ctrl.getModule(from))
            // todo(diagop-state): inheritState(container.modulePath, from)
            OMMOD(op(from))
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

      case op@LinearConnectorSpec(_, dom, _, _) =>
        if (include.df.nonEmpty) {
          // todo(diagop-mor-equality): theoretically, we could output morphism equality
          //                            but the MMT system does not yet support native statements of morphism equality
        } else if (include.args.nonEmpty) {
          throw new NotImplementedError("Parametric includes not supported by linear diagram operators yet")
        } else {
          val newData: Option[(MPath, Term)] = include.from match {
            case from if dom.dom.hasImplicitFrom(from) => // assumed invariant dom.dom == cod.dom
              // only create the actually necessary includes
              // TODO hacky work around here, see discussion at https://mattermost.kwarc.info/kwarc/pl/opp88dhc57g4zmhyfzr7gyqixr
              // if (library.hasImplicit(op.dom.applyDomain(from), dom(container.path.toMPath))) {
              //  None
              //} else {
                Some((dom.applyDomain(from), op.applyDomain(from)))
              // }

            case from =>
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
    ???
  }

  def applyConstant(c: Constant, container: ModuleOrLink)(implicit interp: DiagramInterpreter): Unit
}

class NewPushout(m: Term, dom: MPath, cod: MPath) extends Blah {
  override val ops: LinearOperatorSpecs = resolve(List(
    ("P", Diagram.singleton(dom) -> Diagram.singleton(cod), DiagramFunctor.singleton(dom, cod)),
    ("in", Id(dom) -> "P", DiagramConnection.Singleton(dom, cod, m))
  ))

  def applyConstant(c: Constant, container: ModuleOrLink)(implicit interp: DiagramInterpreter): Unit = {
    def tr(t: Term): Term = interp.ctrl.library.ApplyMorphs(t, OMMOD(ops("in")(c.parent)))

    val newC = new FinalConstant(
      home = OMMOD(ops("P")(c.parent)),
      name = c.name,
      alias = c.alias,
      tpC = c.tpC.map(tr),
      dfC = c.dfC.map(tr),
      rl = c.rl,
      notC = NotationContainer.empty(),
      vs = c.vs
    )
    newC.metadata.add(c.metadata.getAll : _*)

    val cAssignment = Constant(
      home = OMMOD(ops("in")(c.parent)),
      name = LocalName(ComplexStep(c.parent) :: c.name),
      alias = Nil,
      tp = c.tpC.map(tr).get,
      df = Some(newC.toTerm),
      rl = None
    )

    interp.add(newC)
    interp.add(cAssignment)
  }
}
