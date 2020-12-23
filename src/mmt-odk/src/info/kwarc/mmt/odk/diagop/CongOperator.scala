package info.kwarc.mmt.odk.diagop

import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.modules.diagops.{Renamer, SimpleInwardsConnector, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.{Getfield, ModelsOf}
import info.kwarc.mmt.odk.diagop.OpUtils.GeneralApplySpine

/**
  * Creates the theory `Cong(X)` of congruences over `X` for every SFOL theory `X`.
  */
object CongOperator extends SimpleLinearOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?cong_operator")
  override val operatorDomain: MPath = SFOL.sfoleqnd

  // strengthened SFOL because [[CongQuotientConnector]] needs this as its codomain
  // (due to quotient types) and the diagop framework cannot handle this nicely yet
  override val operatorCodomain: MPath = SFOL.Strengthened

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_cong")

  val par : Renamer[LinearState] = getRenamerFor("ᵖ")
  val quot : Renamer[LinearState] = getRenamerFor("_q")

  object ClosureCreator extends ModRelClosureCreator[LinearState] {
    // Although the QuotOperator only works on one model (the "parent model"),
    // it employs a *binary* (equivalence) relation on universes of the parent model.
    // Hence, define relationArity to be 2 and identify the models in the other overrided functions below
    override val relationArity: Int = 2

    override protected def applyTypeSymbolRef(structureIdx: Int, s: GlobalName)(implicit state: LinearState): Term =
      OMS(par(s))

    override protected def inRelation(tp: GlobalName, arguments: List[Term])(implicit state: LinearState): Term =
      ApplySpine(OMS(quot(tp)), arguments : _*)
  }

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Option[Term])] = {
    val parCopy = (par(name), par(tp), df.map(par(_)))

    parCopy :: (tp match {
      case SFOL.TypeSymbolType() =>
        // create t^q: Mod ?EqvRel (tm t^p)
        List((quot(name), ModelsOf(Path.parseM("latin:/?DummyEqvRel"), SFOL.tm(par(c))), None))

      case SFOL.FunctionSymbolType(argTypes, retType) =>
        // todo: work on definiens
        List((quot(name), ClosureCreator.applyFunctionSymbol(c.path, argTypes, retType), None))

      case SFOL.PredicateSymbolType(argTypes) =>
        // todo: work on definiens
        List((quot(name), ClosureCreator.applyPredicateSymbol(c.path, argTypes), None))

      case _ =>
        NotApplicable(c)
    })
  }
}

/**
  * Creates the view `cong_par: X -> Cong(X)` projecting out the parent model.
  */
object CongParentConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?cong_par_connector"),
  CongOperator
) with SystematicRenamingUtils {
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_cong_par")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val par = CongOperator.par.coercedTo(state)
    List((name, par(c)))
  }
}

/**
  * Creates the view `quot: X -> CONG(X)` realizing an `X`-model by taking a `CONG(X)`-model and
  * quotiening its parent model by its congruence.
  */
object CongQuotientConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?cong_quot_connector"),
  CongOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_quot")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val REL_ACCESSOR = LocalName("rel") // the relation field of the Mod type of the equivalence relation theory

    val par = CongOperator.par.coercedTo(state)
    val quot = CongOperator.quot.coercedTo(state)

    tp match {
      case SFOL.TypeSymbolType() =>
        List((name, SFOL.QuotientTypes.quotientTp(par(c), quot(c))))

      case SFOL.FunctionOrPredicateSymbolType(argTypes) =>
        val lambdaCtx = OpUtils.bindFresh(
          Context.empty,
          argTypes.map(argTp => SFOL.QuotientTypes.quotientTp(par(c), OMS(quot(argTp)))),
          None
        )

        // for function symbols this is a term of the function's return type (some SFOL type)
        //    => still needs to be put into the respective equivalence class (cf. below)
        // for predicate symbols this is a term of type prop
        //    => can be returned as-is (cf. below)
        val rawReturnValue = GeneralApplySpine(
          par(c),
          argTypes.zip(lambdaCtx).map {
            case (argTp, v) =>
              SFOL.QuotientTypes.quot_inj(OMS(par(argTp)), Getfield(OMS(quot(argTp)), REL_ACCESSOR), v.toTerm)
          } : _*
        )

        tp match {
          case SFOL.FunctionSymbolType(_, retType) =>
            val functionEqvClassValue = SFOL.QuotientTypes.quot_project(
              OMS(par(retType)),
              Getfield(OMS(quot(retType)), REL_ACCESSOR),
              rawReturnValue
            )
            List((name, functionEqvClassValue))

          case SFOL.PredicateSymbolType(_) =>
            List((name, rawReturnValue))
        }

      case SFOL.AxiomSymbolType() =>
        // todo: what to do?
        NotApplicable(c, "Action on axioms not yet implemented")

      case _ => NotApplicable(c)
    }
  }
}
