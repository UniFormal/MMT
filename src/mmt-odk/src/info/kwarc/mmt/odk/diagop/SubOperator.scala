package info.kwarc.mmt.odk.diagop

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.{ApplySpine, FunType, Lambda}
import info.kwarc.mmt.odk.diagop.OpUtils.{GeneralApplySpine, GeneralLambda}

/**
  * Creates the theory `Sub(X)` of substructures for every SFOL theory `X`.
  */
object SubOperator extends SimpleLinearOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?sub_operator")
  override val operatorDomain: MPath = SFOL.sfoleqnd
  override val operatorCodomain: MPath = SFOL.sfoleqnd

  val par: Renamer[LinearState] = getRenamerFor("ᵖ") // parent symbol copy
  val sub: Renamer[LinearState] = getRenamerFor("ˢ") // substructure symbol/condition

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_sub")

  object ClosureCreator extends ModRelClosureCreator[LinearState] {
    override val relationArity: Int = 1

    override protected def applyTypeSymbolRef(structureIdx: Int, s: GlobalName)(implicit state: LinearState): Term = {
      assert(structureIdx == 0)
      OMS(par(s))
    }

    override protected def inRelation(tp: GlobalName, arguments: List[Term])(implicit state: LinearState): Term = {
      assert(arguments.size == 1)
      ApplySpine(OMS(sub(tp)), arguments.head)
    }
  }

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Option[Term])] = {
    val parCopy = (par(name), par(tp), df.map(par(_)))

    parCopy :: (tp match {
      case SFOL.TypeSymbolType() =>
        // input:  t^p: tp
        // output: t^s: tp, t^s: tm t^p -> prop
        val tsType = FunType(
          List((None, SFOL.tm(par(c)))),
          SFOL.prop.term
        )

        List((sub(name), tsType, df.map(sub(_))))

      case SFOL.FunctionSymbolType(argTypes, retType) =>
        // input:
        //   f: tm t_1 ⟶ … ⟶ tm t_n ⟶ tm t  (here: argTypes = List(t_1, ..., t_n), retType = t)
        //
        // output:
        //   f^p: tm t_1^p ⟶ … ⟶ tm t_n^p ⟶ tm t^p
        //   f^s: |- ∀ [x_1 … x_n] (t_1^s x_1) ∧ … ∧ (t_n^s x_n) ⇒ t^s (f^p x_1 … x_n)

        List((
          sub(name),
          ClosureCreator.applyFunctionSymbol(c.path, argTypes, retType),
          df.map(_ => SFOL.sketchLazy("provable"))
        ))

      case SFOL.PredicateSymbolType(_) =>
        Nil

      case SFOL.AxiomSymbolType() =>
        // todo: alternative: use predicate subtypes instead of relativation
        val relativizedAxiom = (
          sub(name),
          relativizeQuantifiers(tp, state.outerContext, par.apply(_), sub.apply(_)),
          df.map(_ => SFOL.sketchLazy("provable"))
        )

        List(parCopy, relativizedAxiom)

      case _ =>
        NotApplicable(c)
    })
  }

  private def relativizeQuantifiers(t: Term, context: Context, par: Term => Term, sub: Term => Term): Term = {
    new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case SFOL.forall(tp, oldBody) =>

          val newName = LocalName.random("relativized_var")
          SFOL.forall(par(tp), Lambda(
            name = newName,
            par(tp),
            SFOL.impl(
              ApplySpine(sub(tp), OMV(newName)),
              ApplySpine(Traverser(this, oldBody), OMV(newName))
            )
          ))

        case _ => par(Traverser(this, t))
      }
    }.apply(t, context)
  }
}

/**
  * Creates the view `full: SUB(X) -> X` representing the full submodel of a model.
  */
object SubFullConnector extends SimpleOutwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?sub_full_connector"),
  SubOperator
) with SystematicRenamingUtils {
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_full")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val par = SubOperator.par.coercedTo(state)
    val sub = SubOperator.sub.coercedTo(state)

    val parStructureCopy = (par(name), c.toTerm)

    parStructureCopy :: (tp match {
      case SFOL.TypeSymbolType() =>
        List(
          // construct assignment `U^s = [x] true`
          (sub(name), Lambda(LocalName("x"), SFOL.tm(c.toTerm), SFOL.true_))
        )

      case SFOL.FunctionSymbolType(_, _) =>
        List(
          (sub(name), SFOL.sketchLazy("effectively by trueI"))
        )

      case SFOL.PredicateSymbolType(_) =>
        // nothing to do
        Nil

      case SFOL.AxiomSymbolType() =>
        List(
          (sub(name), SFOL.sketchLazy(s"effectively by axiom `${name}` and trueI"))
        )

      case _ =>
        NotApplicable(c)
    })
  }
}

/**
  * Creates the view `sub_par: X -> SUB(X)` projecting out the parent model.
  */
object SubParentConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?sub_par_connector"),
  SubOperator
) with SystematicRenamingUtils {
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_sub_par")

  override protected def applyConstantSimple(container: SubParentConnector.Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val par = SubOperator.par.coercedTo(state)
    List((name, par(c)))
  }
}

/**
  * Creates the view `sub_mod: X -> SUB(X)` taking a `SUB(X)`-model and realizing an `X`-model by it
  * via predicate subtypes.
  */
object SubModelConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?sub_mod_connector"),
  SubOperator
) with SystematicRenamingUtils {
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_sub_mod")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val par = SubOperator.par.coercedTo(state)
    val sub = SubOperator.sub.coercedTo(state)

    tp match {
      case SFOL.TypeSymbolType() =>
        List((name, SFOL.predicateSubTp(par(c), sub(c))))

      // todo: unify function symbol and predicate symbol case nicely (see QuotOperator where this was done)
      case SFOL.FunctionSymbolType(argTypes, retType) =>
        // input:
        //   f: tm t_1 ⟶ ... ⟶ tm t_n ⟶ tm t
        //
        // output:
        //   f := [x_1: ⦃t_1^s⦄, ..., x_n: ⦃t_n^s⦄] downcast of (f^p (x_1 ↑ …) … (x_n ↑ …)) via closure property f^s

        val bindingCtx = OpUtils.bindFresh(Context.empty, argTypes.map(tp => { // todo: replace Context.empty
          SFOL.tm(SFOL.predicateSubTp(OMS(par(tp)), OMS(sub(tp))))
        }))

        val body = SFOL.downcastParentElementToSubtype(
          parentTp = OMS(par(retType)),
          selectionFun = OMS(sub(retType)),
          parentElem = GeneralApplySpine(par(c), argTypes.zip(bindingCtx).map {
            case (tp, vd) => SFOL.injectSubtypeElementIntoParent(
              parentTp = OMS(par(tp)),
              selectionFun = OMS(sub(tp)),
              subElem = OMV(vd.name)
            )
          }: _*),
          containmentProof = SFOL.sketchLazy(s"provable via ${sub(name)}")
        )

        val assignment = GeneralLambda(bindingCtx, body)
        List((name, assignment))

      case SFOL.PredicateSymbolType(argTypes) =>
        // input:  c: tm t_1 ⟶ ... ⟶ tm t_n ⟶ prop
        // output: p := [x_1: ⦃t_1^s⦄, ..., x_n: ⦃t_n^s⦄] f^p (x_1 ↑ …) … (x_n ↑ …)

        val bindingCtx = OpUtils.bindFresh(Context.empty, argTypes.map(tp => { // todo: replace Context.empty
          SFOL.tm(SFOL.predicateSubTp(OMS(par(tp)), OMS(sub(tp))))
        }))

        val body = GeneralApplySpine(par(c), argTypes.zip(bindingCtx).map {
          case (tp, vd) => SFOL.injectSubtypeElementIntoParent(
            parentTp = OMS(par(tp)),
            selectionFun = OMS(sub(tp)),
            subElem = vd.toTerm
          )
        }: _*)

        val assignment = GeneralLambda(bindingCtx, body)
        List((name, assignment))

      case SFOL.AxiomSymbolType() =>
        List((name, SFOL.sketchLazy("provable")))

      case _ =>
        NotApplicable(c)
    }
  }
}
