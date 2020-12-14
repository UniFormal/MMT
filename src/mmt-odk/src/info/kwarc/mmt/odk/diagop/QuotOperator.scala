package info.kwarc.mmt.odk.diagop

import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.lf.{ApplySpine, Lambda}
import info.kwarc.mmt.odk.LFX.{Getfield, ModelsOf, RecExp}
import info.kwarc.mmt.odk.diagop.OpUtils.GeneralApplySpine


/**
  {{{
    Quotient operator (needs quotient types in logic)

    t: tp
      |-> t^p: tp
          t^q: Mod ?EqvRel (tm t^p)

    f: tm t_1 ⟶ ... ⟶ tm t_n ⟶ tm t
      |-> f^p: tm t_1^p ⟶ ... ⟶ tm t_n^p ⟶ tm t^p
          f^q: |- forall [x_1: tm t_1^p ... x_n: tm t_n^p             "t^q is congruence wrt. f"
                          x_1': tm t_1^p ... x_n': tm t_n^p]
                       t_1^q x_1 x_1' ∧ … ∧ t_n^q x_n x_n' ⇒ t^q (f^p x_1 … x_n) (f^p x_1' … x_n')

    c: tm t_1 ⟶ ... ⟶ tm t_n ⟶ prop
      |-> c^p: tm t_1^p ⟶ ... ⟶ tm t_n^p ⟶ prop
          c^q: |- forall [x_1: tm t_1^p ... x_n: tm t_n^p]
                    forall [x_1': tm t_1^p ... x_n': tm t_n^p]
                       t_1^q x_1 x_1' ⟶ ... ⟶ t_n^q x_n x_n' ⟶ (c^p x_1 ... x_n) <-> (c^p x_1' ... x_n')
  }}}
 */
object QuotOperator extends SimpleLinearOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?quot_operator")
  override val operatorDomain: MPath = SFOL.Strengthened
  override val operatorCodomain: MPath = SFOL.Strengthened

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_quot")

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

object QuotParentConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?quot_par_connector"),
  QuotOperator
) with SystematicRenamingUtils {
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_quot_par")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val par = QuotOperator.par.coercedTo(state)
    List((name, par(c)))
  }
}

/**
  *
  * not implemented yet
  *
  * {{{
  * map `mod: T -> Quot(T)`
  *
  * t |-> t^p quot. t^q
  * f |-> [x_1: tm (t_1^p quot. t_1^q), ..., x_n: tm (t_n^p quot. t_n^q)]
  * eqv. class of (f^p (some repr of x_1 in t_1^p) ... (some repr of x_n in t_n^p))
  * c |-> [x_1: tm (t_1^p quot. t_1^q), ..., x_n: tm (t_n^p quot. t_n^q)]
  * (c^p (some repr of x_1 in t_1^p) ... (some repr of x_n in t_n^p))
  * ax |-> ???
  * }}}
  */
object QuotModConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?quot_mod_connector"),
  QuotOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_quot_mod")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val REL_ACCESSOR = LocalName("rel") // the relation field of the Mod type of the equivalence relation theory

    val par = QuotOperator.par.coercedTo(state)
    val quot = QuotOperator.quot.coercedTo(state)

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
