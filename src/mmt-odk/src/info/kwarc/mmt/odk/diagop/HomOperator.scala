package info.kwarc.mmt.odk.diagop

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.{ApplySpine, FunType, Lambda}
import info.kwarc.mmt.odk.diagop.OpUtils.GeneralApplySpine

/**
  * Linearly transforms SFOL theories T to Hom(T), the theory of homomorphisms
  * of T.
  *
  * @see [[HomDomConnector]], [[HomCodConnector]], [[HomImgConnector]]
  */
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
      val binding = OpUtils.bindFresh(Context.empty, argTypes.map(tp => SFOL.tm(OMS(dom(tp))))) // todo: replace context.empty

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
          df.map(_ => SFOL.sketchLazy("provable"))
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
          NotApplicable(c, "Axiom not monotone")
        } else {
          val (forallContext, domExpr, codExpr) = quantify(tp, argTypes)

          val homomorphismCondition = SFOL.ded(SFOL.forallMany(
            forallContext,
            SFOL.impl(domExpr, codExpr)
          ))
          val homomorphismConstant = (
            name,
            homomorphismCondition,
            df.map(_ => SFOL.sketchLazy("provable"))
          )

          List(homomorphismConstant)
        }

      case SFOL.AxiomSymbolType() => Nil
      case _ =>
        NotApplicable(c)
    })
  }
}

/**
  * Linearly transforms an SFOL theory T to morphism ''dom: T -> Hom(T)'' "projecting
  * the homomorphism's domain out."
  */
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

/**
  * Linearly transforms an SFOL theory T to morphism ''cod: T -> Hom(T)'' "projecting
  * the homomorphism's codomain out."
  */
object HomCodConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_cod_connector"),
  HomOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_cod")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val dom = HomOperator.dom.coercedTo(state)
    List((name, dom(c)))
  }
}

/**
  * Linearly transforms an SFOL theory T to morphism ''img: Sub(T) -> Hom(T)''.
  *
  * Assumes Sub and Hom have already been applied before.
  *
  * Maps as follows:
  *
  * {{{
  *   U: tp
  *     |-> U^p := U^c, U^s := [y: tm U^c] ∃ [x: tm U^d] U^h x ≐ y
  *
  *   f: tm t_1 -> … -> tm t_n -> tm t
  *     |-> f^p := f^c, f^s := proof of image under homomorphism being closed wrt. f
  *
  *   c: tm t_1 -> … -> tm t_n -> tm t
  *     |-> c^p := c^c
  *
  *   a: |- F
  *     |-> a^p := a^c, a^s := proof using a^c, but only possible if a was monotone, right?
  * }}}
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

    val codCopy = (par(name), cod(c))

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

      case SFOL.FunctionSymbolType(argTypes, retType) =>
        // f: tm t_1 -> … -> tm t_n -> tm t
        //
        //   |-> f^p := f^c, f^s := proof of image under homomorphism being closed wrt. f

        val proofSketch = {
          val vars = argTypes.zipWithIndex.map {
            case (tp, i) => ("y" + StringUtils.subscriptInteger(i), tp)
          }
          val varPreimages = argTypes.zipWithIndex.map {
            case (tp, i) => ("x" + StringUtils.subscriptInteger(i), tp)
          }


          val varIntros = vars.map(v => v._1 + ": " + par(v._2).name).mkString(", ")
          val preimagesIntros = varPreimages.map(v => v._1 + ": " + dom(v._2).name).mkString(", ")
          val varsInSubset = vars.map(v => sub(v._2).name + " " + v._1).mkString(", ")

          s"Goal is to show our homomorphism's image in ${cod(name)} is closed under multiplication in its parent structure.\n" +
            s"Hence let $varIntros be variables with $varsInSubset (*). " +
            s"We need to show: ${sub(retType).name} (${cod(name)} ${vars.map(_._1).mkString(" ")}), i.e. the existence of a preimage for the parenthesized term.\n" +
            s"Due to (*), there are preimages $preimagesIntros. " +
            s"Claim: ${dom(name)} ${varPreimages.map(_._1).mkString(" ")} is the preimage we are looking for.\n" +
            s"Namely: ${hom(retType).name} (${cod(name)} ${varPreimages.map(_._1).mkString(" ")}) ≐ ${cod(name)} ${varPreimages.map(v => "(" + hom(v._2).name + " " + v._1 + ")").mkString(" ")} ≐ ${cod(name)} ${vars.map(_._1).mkString(" ")}. qed."
        }

        List(codCopy, (sub(name), SFOL.sketchLazy(proofSketch)))

      case SFOL.PredicateSymbolType(_) =>
        // c: tm t_1 -> … -> tm t_n -> tm t
        //
        //   |-> c^p := c^c
        List(codCopy)

      case SFOL.AxiomSymbolType() =>
        val allowedReferences = state.processedDeclarations.map(_.path).toSet
        if (!SFOL.isMonotone(tp, state.outerContext, allowedReferences)) {
          NotApplicable(c, "Axiom not monotone, skipping")
        } else {
          List(codCopy, (sub(name), SFOL.sketchLazy("probably provable")))
        }

      case _ =>
        NotApplicable(c)
    }
  }
}
