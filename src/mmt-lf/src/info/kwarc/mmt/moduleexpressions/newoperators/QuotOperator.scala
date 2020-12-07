package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects.{Context, OMA, OMPMOD, OMS, OMV, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.moduleexpressions.newoperators.OpUtils.GeneralApplySpine


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
  override val operatorDomain: MPath = Path.parseM("latin:/?SFOLEQND")
  override val operatorCodomain: MPath = Path.parseM("latin:/?SFOLEQND")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_quot")

  val par : Renamer[LinearState] = getRenamerFor("p")
  val quot : Renamer[LinearState] = getRenamerFor("q")

  object ClosureCreator extends NRelClosureCreator[LinearState] {
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
        val eqvRelType = OMA(
          OMS(Path.parseS("http://gl.mathhub.info/MMT/LFX/Records?Symbols?ModelsOf")),
          List(OMPMOD(
            Path.parseM("latin:/?DummyEqvRel"),
            List(SFOL.tm(par(c)))
          ))
        )

        List((quot(name), eqvRelType, None))

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
    val par = QuotOperator.par.coercedTo(state)
    val quot = QuotOperator.quot.coercedTo(state)

    tp match {
      case SFOL.TypeSymbolType() =>
        List((name, SFOL.QuotientTypes.quotientTp(par(c), quot(c))))

      case SFOL.FunctionOrPredicateSymbolType(argTypes) =>
        val lambdaCtx = OpUtils.bindFresh(
          Context.empty,
          argTypes.map(tp => SFOL.QuotientTypes.quotientTp(par(c), OMS(quot(tp)))),
          None
        )

        // todo: replace OMV("rel") by access to relation within structure:
        //   "quot(...) / rel" <-- something like that
        // for function symbols this is a term of the function's return type (some SFOL type)
        //    => still needs to be put into the respective equivalence class (cf. below)
        // for predicate symbols this is a term of type prop
        //    => can be returned as-is (cf. below)
        val rawReturnValue = GeneralApplySpine(
          par(c),
          lambdaCtx.map(v => SFOL.QuotientTypes.quot_inj(par(v.tp.get), OMV("rel"), v.toTerm)) : _*
        )

        tp match {
          case SFOL.FunctionSymbolType(_, retType) =>
            val functionEqvClassValue = SFOL.QuotientTypes.quot_project(OMS(par(retType)), OMV("rel"), rawReturnValue)
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