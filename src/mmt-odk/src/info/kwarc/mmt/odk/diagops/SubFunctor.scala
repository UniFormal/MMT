package info.kwarc.mmt.odk.diagops

import info.kwarc.mmt.api.checking.Solver
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.modules.diagrams.oldstuff.{Functor, InwardsLinearConnector, LinearFunctor, LinearOperator, SystematicRenamer}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, Declaration, TermContainer}
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.lf._

import scala.annotation.tailrec

object SubOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?sub_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearOperator] =
    Some((SubParentConnector :: SubFunctor).withFocus(1))
}

object SubFunctor extends LinearFunctor {
  override val dom: Diagram = Diagram.singleton(SFOL.sfoleqnd)
  override val cod: Diagram = Diagram.singleton(SFOL.sfoleqnd)
  override def applyDomainModule(m: MPath): MPath = m

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_Sub")

  val parentRenamer: SystematicRenamer = getRenamerFor("_p")
  val subRenamer: SystematicRenamer = getRenamerFor("_s")

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = {
    implicit val ctrl: Controller = interp.ctrl

    implicit def p(t: Term): Term =
      ctrl.library.ApplyMorphs(t, OMMOD(SubParentConnector.applyModulePath(expressionContext(c).toMPath)))

    implicit val visibleSymbols: Set[GlobalName] = seenDeclarations(c.path.module).toSet

    val parentCopy = Constant(
      home = OMMOD(parentRenamer(c.path).module),
      name = parentRenamer(c.name),
      alias = c.alias,
      tpC = TermContainer.asParsed(c.tp.map(p)),
      dfC = TermContainer.asParsed(c.df.map(p)),
      rl = c.rl,
      notC = c.notC.copy()
    )

    parentCopy :: c.tp.map {
      case SFOL.PredicateSymbolType(_) => Nil
      case tp =>
        List(const(
          subRenamer(c.path),
          /*Beta.reduce(*/ApplySpine(translate(Context(c.path.module), tp), parentRenamer(c))/*)(interp.ctrl)*/,
          c.df.map(translate(Context(c.path.module), _))
        ))
    }.toList.flatten
  }

  @tailrec
  private def retType(t: Term): Term = t match {
    case Pi(_, _, body) => retType(body)
    case t => t
  }
  def translate(ctx: Context, t: Term)(implicit ctrl: Controller, p: Term => Term, visibleSymbols: Set[GlobalName]): Term = t match {
    case SFOL.tp.term =>
      Lambda(LocalName("Up"), SFOL.tp.term, Arrow(SFOL.tm(OMV("Up")), SFOL.prop.term))
    case SFOL.prop.term => Lambda(OMV.anonymous, SFOL.prop.term, SFOL.prop.term)
    case SFOL.tm.term =>
      val ctx = Context(
        VarDecl(LocalName("Tp"), SFOL.tp.term),
        VarDecl(LocalName("Ts"), Arrow(SFOL.tm(OMV("Tp")), SFOL.prop.term)),
        VarDecl(LocalName("tp"), SFOL.tm(OMV("Tp")))
      )
      Lambda(ctx, SFOL.ded(ApplySpine(OMV("Ts"), OMV("tp"))))
    case SFOL.ded.term =>
      Lambda(
        Context(
          VarDecl(LocalName("Fp"), SFOL.prop.term),
          VarDecl(LocalName("Fs"), SFOL.prop.term),
          VarDecl(LocalName("pf"), SFOL.ded(OMV("Fp")))
        ),
        SFOL.ded(OMV("Fs"))
      )

    case OMS(p) if visibleSymbols.contains(p) => subRenamer(OMS(p))
    case OMV(x) => OMV(x / "s")
    case t@FunType(_, _) =>
      val Some((originallyBoundCtx, boundCtx, out)) = FullyNamedFunType.createNumbered(ctx)(t)
      val isPredicate = retType(out) == SFOL.prop.term

      val newBoundCtx: List[(VarDecl, VarDecl, Option[VarDecl])] = boundCtx.map(vd => {
        (
          VarDecl(vd.name / "f", vd.tp.map(p).orNull),
          VarDecl(vd.name / "p", vd.tp.map(p).orNull),
          vd.tp.flatMap {
            case SFOL.tm(_) if isPredicate => None
            case _ => Some(VarDecl(vd.name / "s", vd.tp.map(tp => ApplySpine(translate(ctx, tp), OMV(vd.name / "p"))).orNull))
          }
        )
      })

      newBoundCtx.foldRight(translate(ctx ++ originallyBoundCtx, out)) {
        case ((lambdaVd, argVd, relationVd), t) =>
          val piCtx = Context(argVd) ::: relationVd.toList
          Lambda(lambdaVd, Pi(piCtx, ApplySpine(t, lambdaVd.toTerm)))
      }

    case ApplySpine(f, args) =>
      def handleSingleArgument(f: Term, arg: Term): Term = {
        Solver.infer(ctrl, ctx, f, None).get match {
          case FunType((_, SFOL.tm(_)) :: _, body) if retType(body) == SFOL.prop.term =>
            ApplySpine(translate(ctx, f), p(arg))

          case _ =>
            ApplySpine(translate(ctx, f), p(arg), translate(ctx, arg))
        }
      }

      @tailrec
      def recurse(f: Term, args: List[Term]): Term = {
        if (args.isEmpty) translate(ctx, f)
        else if (args.size <= 1) handleSingleArgument(f, args.head)
        else recurse(ApplySpine(f, args.head), args.tail)
      }
      recurse(f, args)
  }
}

object FullyNamedFunType {
  def createNumbered(ctx: Context)(t: Term): Option[(Context, Context, Term)] = create(ctx, i => LocalName(i.toString))(t)

  /**
    *
    * @param ctx
    * @param makeName
    * @param t
    * @return (originally bound context, new context, body)
    */
  def create(ctx: Context, makeName: Integer => LocalName)(t: Term): Option[(Context, Context, Term)] = t match {
    case FunType(in, out) =>
      val newParameters: Context = in.zipWithIndex.foldLeft(Context.empty) {
        case (newCtx, ((Some(n), tp), _)) => newCtx ++ VarDecl(n, tp)
        case (newCtx, ((None, tp), idx)) => newCtx ++ VarDecl(Context.pickFresh(ctx ++ newCtx, makeName(idx))._1, tp)
      }
      Some((in.collect { case (Some(n), tp) => VarDecl(n, tp) }, newParameters, out))

    case _ => None
  }
}

object SubParentConnector extends InwardsLinearConnector {
  override val out: Functor = SubFunctor
  override def applyDomainModule(thy: MPath): MPath = thy
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_SubParent")

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] =
    List(assgn(c.path, SubFunctor.parentRenamer(c)))
}