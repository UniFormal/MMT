package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{DiagramInterpreter, LinearModuleTransformer, Renamer, SimpleInwardsConnector, SimpleLinearConnector, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.{ApplySpine, FunType, Lambda}
import info.kwarc.mmt.moduleexpressions.newoperators.OpUtils.GeneralApplySpine

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

object HomDomConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_dom_connector"),
  HomOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_dom")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val dom = HomOperator.dom.coercedTo(state)
    List((name, dom(c)))
  }
}

object HomCodConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_cod_connector"),
  HomOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_dom")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val dom = HomOperator.dom.coercedTo(state)
    List((name, dom(c)))
  }
}

/* todo: also generate connecting morphism `img: Sub(Magma) -> Hom(Magma)` (for image of homomorphism)
         then: Magma -- sub --> Sub(Magma) -- img --> Hom(Magma) would give us the image of the homomorphism
               as a Magma model

   U: tp
        |-> U^p := U^c, U^s := [y: tm U^c] ∃ [x: tm U^d] U^h x ≐ y

   f: tm t_1 -> … -> tm t_n -> tm t
        |-> f^p := f^c, f^s := proof of image under homomorphism being closed wrt. f

   c: tm t_1 -> … -> tm t_n -> tm t
        |-> c^p := c^c, c^s := proof of image under homomorphism being closed wrt. p

   a: |- F
        |-> a^p := a^c, a^s := proof using a^c, but only possible if a was monotone, right?

   */
object HomImgConnector extends SimpleLinearConnector with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_img_connector")

  override val in: LinearModuleTransformer = SubOperator
  override val out: LinearModuleTransformer = HomOperator

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_hom_img")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val par = SubOperator.par.coercedTo(state)
    val sub = SubOperator.sub.coercedTo(state)
    val dom = HomOperator.dom.coercedTo(state)
    val cod = HomOperator.cod.coercedTo(state)
    val hom = HomOperator.hom.coercedTo(state)

    val codCopy = (par(name), par(c))

    tp match {
      case SFOL.TypeSymbolType() =>
        // input: U: tp
        // output:
        //   U^p := U^c
        //   U^s := [y: tm U^c] ∃ [x: tm U^d] U^h x ≐ y
        //
        //   (i.e. in math notation: choose subset U^h[U^d] of U^c)
        val inImagePredicate = Lambda(
          LocalName("y"),
          SFOL.tm(cod(c)),
          SFOL.exists(
            SFOL.tm(dom(c)),
            Lambda(
              LocalName("x"),
              SFOL.tm(dom(c)),
              SFOL.eq(
                SFOL.tm(cod(c)),
                ApplySpine(hom(c), OMV("x")),
                OMV("y")
              )
            )
          )
        )
        List(codCopy, (sub(name), inImagePredicate))

      case SFOL.FunctionSymbolType(_, _) =>
        // f: tm t_1 -> … -> tm t_n -> tm t
        //
        //   |-> f^p := f^c, f^s := proof of image under homomorphism being closed wrt. f
        List(codCopy, (sub(name), SFOL.sketch(OMV("<todo: insert type>"), "image is closed")))

      case SFOL.PredicateSymbolType(_) =>
        // c: tm t_1 -> … -> tm t_n -> tm t
        //
        //   |-> c^p := c^c, c^s := proof of image under homomorphism being closed wrt. p
        NotApplicable(c)
      case _ =>
        NotApplicable(c)
    }
  }
}