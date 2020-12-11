package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Renamer, SimpleInwardsConnector, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects.{Context, OMA, OMPMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.moduleexpressions.newoperators.OpUtils.GeneralApplySpine

// Not yet created, but useful comments/spec in this file

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

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Option[Term])] = {
    val parCopy = (par(name), par(tp), df.map(par(_)))

    parCopy :: (tp match {
      case SFOL.TypeSymbolType() =>
        // t: tp
        // ↓
        // t^q: Mod ?EqvRel (tm t^p)

        val eqvRelType = OMA(
          OMS(Path.parseS("http://gl.mathhub.info/MMT/LFX/Records?Symbols?ModelsOf")),
          List(OMPMOD(
            Path.parseM("latin:/?DummyEqvRel"),
            List(SFOL.tm(par(c)))
          ))
        )

        List((quot(name), eqvRelType, None))

      case SFOL.FunctionOrPredicateSymbolType(argTypes) =>
        // Uniformly handle function symbols and predicate symbols as follows:
        //
        // function symbols
        //
        //   f: tm t_1 ⟶ … ⟶ tm t_n ⟶ tm t
        //   ↓
        //   f^q: ⊦ ∀ [x_1: tm t_1^p, …, x_n: tm t_n^p
        //             x_1': tm t_1^p, …, x_n': tm t_n^p]
        //               t_1^q x_1 x_1' ∧ … ∧ t_n^q x_n x_n' ⇒ t^q (f^p x_1 … x_n) (f^p x_1' … x_n')
        //
        //     f^q can be read as "t^q is congruence wrt. f"
        //
        // predicate symbols
        //
        //   c: tm t_1 ⟶ ... ⟶ tm t_n ⟶ prop
        //   ↓
        //   c^q: ⊦ ∀ [x_1: tm t_1^p, …, x_n: tm t_n^p
        //            x_1': tm t_1^p, …, x_n': tm t_n^p]
        //              t_1^q x_1 x_1' ∧ … ∧ t_n^q x_n x_n' ⇒ (c^p x_1 … x_n) ⇔ (c^p x_1' … x_n')

        def repeatList[T](times: Int, list: List[T]): List[T] = List.fill(times)(list).flatten

        // create context of size `2 * argTypes.size`
        // [x_1: tm t_1^p, …, x_n: tm t_n^p,    x_1': tm t_1^p, …, x_n': tm t_n^p]
        val forallContext = OpUtils.bindFresh(
          Context.empty, // todo replace Context.empty
          repeatList(2, argTypes.map(tp => OMS(par(tp)))),
          hint = Some(i => if (i < argTypes.size) s"x_$i" else s"x__${i - argTypes.size}p") // MMT surface syntax doesn't support '
        )

        // build term `t_1^q x_1 x_1' ∧ … ∧ t_n^q x_n x_n'`
        val allArgumentsRelate = argTypes.zipWithIndex.map {
          case (tp, i) =>
            ApplySpine(OMS(quot(tp)), forallContext(i).toTerm, forallContext(argTypes.size + i).toTerm)
        }.reduceLeftOption(SFOL.and(_, _))

        val resultRelates = tp match {
          case SFOL.FunctionSymbolType(_, retType) =>
            // build term `t^q (f^p x_1 … x_n) (f^p x_1' … x_n')`
            ApplySpine(
              OMS(quot(retType)),
              GeneralApplySpine(par(c), forallContext.map(_.toTerm).take(argTypes.size): _*),
              GeneralApplySpine(par(c), forallContext.map(_.toTerm).drop(argTypes.size): _*),
            )

          case SFOL.PredicateSymbolType(_) =>
            // (c^p x_1 … x_n) ⇔ (c^p x_1' … x_n')
            SFOL.equiv(
              ApplySpine(par(c), forallContext.map(_.toTerm).take(argTypes.size): _*),
              ApplySpine(par(c), forallContext.map(_.toTerm).drop(argTypes.size): _*)
            )
        }

        val congruenceCondition = SFOL.ded(SFOL.forallMany(forallContext, allArgumentsRelate match {
          case Some(antecedent) => SFOL.impl(antecedent, resultRelates)
          case None =>
            assert(forallContext.isEmpty)
            resultRelates
        }))

        List((quot(name), congruenceCondition, None))

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
        List(
          (name, SFOL.QuotientTypes.quotientTp(par(c), quot(c)))
        )

      case _ => NotApplicable(c)
    }
  }
}