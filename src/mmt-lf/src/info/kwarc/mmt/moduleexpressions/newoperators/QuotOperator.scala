package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, Path}
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Renamer, SimpleInwardsConnector, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects.{OMA, OMPMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.Constant

// Not yet created, but useful comments/spec in this file

/**
  {{{
  Quotient operator (needs quotient types in logic)

  t: tp
    |-> t^p: tp
        t^q: Mod ?EqvRel (tm t^p)

  f: tm t_1 -> ... -> tm t_n -> tm t
    |-> f^p: tm t_1^p -> ... -> tm t_n^p -> tm t^p
        f^q: |- forall [x_1: tm t_1^p ... x_n: tm t_n^p]             "t^q is congruence wrt. f"
                  forall [x_1': tm t_1^p ... x_n': tm t_n^p]
                     t_1^q x_1 x_1' -> ... -> t_n^q x_n x_n' -> t^q (f^p x_1 ... x_n) (f^p x_1' ... x_n')

  c: tm t_1 -> ... -> tm t_n -> prop
    |-> c^p: tm t_1^p -> ... -> tm t_n^p -> prop
        c^q: |- forall [x_1: tm t_1^p ... x_n: tm t_n^p]
                  forall [x_1': tm t_1^p ... x_n': tm t_n^p]
                     t_1^q x_1 x_1' -> ... -> t_n^q x_n x_n' -> (c^p x_1 ... x_n) <-> (c^p x_1' ... x_n')
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
        //    |-> t^p: tp
        //        t^q: Mod ?EqvRel (tm t^p)

        val eqvRelType = OMA(
          OMS(Path.parseS("http://gl.mathhub.info/MMT/LFX/Records?Symbols?ModelsOf")),
          List(OMPMOD(
            Path.parseM("latin:/?DummyEqvRel"),
            List(SFOL.tm(par(c)))
          ))
        )

        List(parCopy, (quot(name), eqvRelType, None))

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

  override protected def applyConstantSimple(container: QuotModConnector.Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: QuotModConnector.LinearState): List[(LocalName, Term)] = {
    tp match {
      case _ => NotApplicable(c)
    }
  }
}