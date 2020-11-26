package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Renamer, SimpleInwardsConnector, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.{ApplySpine, FunType}
import info.kwarc.mmt.moduleexpressions.newoperators.OpUtils.GeneralApplySpine

/* todo: also generate connecting morphism `img: Sub(Magma) -> Hom(Magma)` (for image of homomorphism)
         then: Magma -- sub --> Sub(Magma) -- img --> Hom(Magma) would give us the image of the homomorphism
               as a Magma model

   Previous idea: linear operators may only produce connecting morphisms from input to output module or vv.
   Now: they also need to be able to produce morphisms connecting to OtherOperator(input module)

   Unclear if either Sub or Hom should generate img.

   U: tp
        |-> U^p := U^c, U^s := [y: U^c] exists [x: U^d] U^h x = y   (i.e. U^s = U^h(U^d) in math notation)

   f: tm t_1 -> ... -> tm t_n -> tm t
        |-> f^p := f^c, f^s := proof of image under homomorphism being closed wrt. f

   c: tm t_1 -> ... -> tm t_n -> tm t
        |-> c^p := c^c, c^s := proof of image under homomorphism being closed wrt. p

   a: |- F
        |-> a^p := a^c, a^s := proof using a^c, but only possible if a was monotone, right?

   */

object HomDomConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_dom_connector"),
  HomOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_dom")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Term)] = {
    val dom = HomOperator.dom.coercedTo(state)
    List((name, tp, dom(c)))
  }
}

object HomCodConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_cod_connector"),
  HomOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_dom")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Term)] = {
    val dom = HomOperator.dom.coercedTo(state)
    List((name, tp, dom(c)))
  }
}

object HomOperator extends SimpleLinearOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_operator")
  override val operatorDomain: MPath = Path.parseM("latin:/?SFOLEQND")
  override val operatorCodomain: MPath = Path.parseM("latin:/?SFOLEQND")

  // Hom(-) copies every input constant to two systematically renamed copies for domain and codomain of the homomorphism
  val dom: Renamer[LinearState] = getRenamerFor("d")
  val cod: Renamer[LinearState] = getRenamerFor("c")

  // and introduces for some input constants a new "homomorphism constant" accounting for a homomorphism condition
  val hom: Renamer[LinearState] = getRenamerFor("h")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_hom")

  override protected def applyConstantSimple(container: HomOperator.Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Option[Term])] = {
    def quantify(t: Term, argTypes: List[GlobalName]): (Context, Term, Term) = {
      val binding = OpUtils.bindFresh(t, argTypes.map(argTp => SFOL.tm(OMS(dom(argTp)))))

      // construct `c^{dom} x_1 ... x_n`
      val domExpr = GeneralApplySpine( // use GeneralApplySpine: the function symbol might be nullary
        OMS(dom(c.path)),
        binding.map(vd => OMV(vd.name)): _*
      )

      // construct `c^{cod} (c^h x_1) ... (c^h x_n)`
      val codExpr = GeneralApplySpine( // use GeneralApplySpine: the function symbol might be nullary
        OMS(cod(c.path)),
        binding.map(vd => ApplySpine(hom(vd.tp.get), OMV(vd.name))): _*
      )

      (binding, domExpr, codExpr)
    }

    // todo: tp in connectors!!!

    val mainConstantCopies = List((dom(name), dom(tp), df.map(dom(_))), (cod(name), cod(tp), df.map(cod(_))))

    mainConstantCopies ::: (tp match {
      case SFOL.TypeSymbolType() =>
        // input:  t: tp
        // output: t^d: tp, t^c: tp, t^h: tm t^d -> t^c
        val thType = FunType(
          List((None, SFOL.tm(dom(c)))),
          SFOL.tm(cod(c))
        )

        List((hom(name), thType, df.map(hom(_))))

      case SFOL.FunctionSymbolType(argTypes, retType) =>
        val (forallContext, domExpr, codExpr) = quantify(tp, argTypes)

        val homomorphismCondition = SFOL.ded(SFOL.forallMany(
          forallContext,
          SFOL.eq(
            SFOL.tm(OMS(cod(retType))),
            ApplySpine(OMS(hom(retType)), domExpr),
            codExpr
          )
        ))

        val homomorphismConstant = (
          hom(name),
          homomorphismCondition,
          df.map(_ => SFOL.sketch(OMV("<todo:implicit arg>"), "provable"))
        )

        List(homomorphismConstant)

      case SFOL.PredicateSymbolType(argTypes) =>
        val monotonicityOkay = df.forall(dfTerm => {
          val allowedReferences = state.processedDeclarations.map(_.path).toSet
          if (SFOL.isMonotone(dfTerm, state.outerContext, allowedReferences)) {
            true
          } else {
            interp.errorCont(InvalidElement(c, "Hom operator cannot process SFOL predicate symbol " +
              "definiens/assignment since it is not monotone. " +
              "See 'Structure-Preserving Diagram Operators' paper for reasons."
            ))
            false
          }
        })

        if (!monotonicityOkay) {
          Nil
        } else {
          val (forallContext, domExpr, codExpr) = quantify(tp, argTypes)

          val homomorphismCondition = SFOL.ded(SFOL.forallMany(
            forallContext,
            SFOL.impl(domExpr, codExpr)
          ))
          val homomorphismConstant = (
            name,
            homomorphismCondition,
            df.map(_ => SFOL.sketch(OMV("<todo:implicit arg>"), "provable"))
          )

          List(homomorphismConstant)
        }

      case SFOL.AxiomSymbolType() => Nil
      case _ =>
        NotApplicable(c)
    })
  }
}
