package info.kwarc.mmt.odk.diagop

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.UnicodeStrings
import info.kwarc.mmt.lf.{ApplySpine, FunTerm, FunType, Lambda}
import info.kwarc.mmt.odk.LFX.RecExp
import info.kwarc.mmt.odk.diagop.OpUtils.GeneralApplySpine

/**
  * Creates the theory `HOM(X)` of homomorphisms between `X` models for every SFOL theory `X`.
  *
  * @see [[HomDomConnector]], [[HomCodConnector]], [[HomImgConnector]]
  */
object HomOperator extends SimpleLinearOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_operator")
  override val operatorDomain: MPath = SFOL.sfoleqnd
  override val operatorCodomain: MPath = SFOL.sfoleqnd

  // Hom(-) copies every input constant to two systematically renamed copies for domain and codomain of the homomorphism
  val dom: Renamer[LinearState] = getRenamerFor("ᵈ")
  val cod: Renamer[LinearState] = getRenamerFor("ᶜ")

  // and introduces for some input constants a new "homomorphism constant" accounting for a homomorphism condition
  val hom: Renamer[LinearState] = getRenamerFor("ʰ")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_hom")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Option[Term])] = {

    val structureCopies = List(
      (dom(name), dom(tp), df.map(dom(_))),
      (cod(name), cod(tp), df.map(cod(_)))
    )

    structureCopies ::: (tp match {
      case SFOL.TypeSymbolType() =>
        // input:  t: tp
        // output: t^d: tp, t^c: tp, t^h: tm t^d -> t^c
        val thType = FunType(
          List((None, SFOL.tm(dom(c)))),
          SFOL.tm(cod(c))
        )

        List((hom(name), thType, df.map(hom(_))))

      case SFOL.FunctionOrPredicateSymbolType(argTypes) =>
        val forallContext = OpUtils.bindFresh(
          Context.empty,// todo: replace context.empty
          argTypes.map(tp => SFOL.tm(OMS(dom(tp))))
        )

        // construct `c^{dom} x_1 ... x_n`
        val lhs = GeneralApplySpine(
          OMS(dom(c.path)),
          forallContext.map(_.toTerm) : _*
        )

        // construct `c^{cod} (c^h x_1) ... (c^h x_n)`
        val rhs = GeneralApplySpine(
          OMS(cod(c.path)),
          forallContext.map(_.toTerm).zipWithIndex.map {
            case (t, idx) => ApplySpine(OMS(hom(argTypes(idx))), t)
          } : _*
        )

        val closureConstantType: Term = tp match {
          case SFOL.FunctionSymbolType(_, retType) =>
            SFOL.ded(SFOL.forallMany(
              forallContext,
              SFOL.eq(
                SFOL.tm(OMS(cod(retType))),
                ApplySpine(OMS(hom(retType)), lhs),
                rhs
              )
            ))

          case SFOL.PredicateSymbolType(_) =>
            SFOL.ded(SFOL.forallMany(
              forallContext,
              SFOL.impl(lhs, rhs)
            ))
        }

        val closureConstantDef: Option[Term] = df.flatMap(dfTerm => tp match {
          case SFOL.PredicateSymbolType(_) =>
            // check monotonicity
            val allowedReferences = state.processedDeclarations.map(_.path).toSet
            if (SFOL.isMonotone(dfTerm, state.outerContext, allowedReferences)) {
              Some(SFOL.sketchLazy("provable"))
            } else {
              interp.errorCont(InvalidElement(c, "Hom operator cannot process definiens of SFOL predicate symbol " +
                "that is *not* monotone. Definiens is skipped. " +
                "See 'Structure-Preserving Diagram Operators' paper for reasons."
              ))

              None
            }

          // function symbol
          case _ => Some(SFOL.sketchLazy("provable"))
        })

        val closureConstant = (hom(name), closureConstantType, closureConstantDef)
        List(closureConstant)

      case SFOL.AxiomSymbolType() => Nil
      case _ =>
        NotApplicable(c)
    })
  }
}

/**
  * Creates the view `dom: X -> HOM(X)` projecting out the homomorphism's domain model.
  */
object HomDomConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_dom_connector"),
  HomOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_hom_dom")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val dom = HomOperator.dom.coercedTo(state)
    List((name, dom(c)))
  }
}

/**
  * Creates the view `cod: X -> HOM(X)` projecting out the homomorphism's codomain model.
  */
object HomCodConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_cod_connector"),
  HomOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_hom_cod")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val dom = HomOperator.dom.coercedTo(state)
    List((name, dom(c)))
  }
}

/**
  * Creates the view `hom_id: HOM(X) -> X` representing the identity homomorphism on X.
  */
object HomIdConnector extends SimpleOutwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_id_connector"),
  HomOperator
) with SystematicRenamingUtils {

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_hom_id")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val dom = HomOperator.dom.coercedTo(state)
    val cod = HomOperator.cod.coercedTo(state)
    val hom = HomOperator.hom.coercedTo(state)

    val structureCopiesAssignments = List(
      (dom(name), c.toTerm),
      (cod(name), c.toTerm)
    )

    structureCopiesAssignments ::: (tp match {
      case SFOL.TypeSymbolType() =>
        List(
          (hom(name), FunTerm.identity(SFOL.tm(c.toTerm)))
        )

      case SFOL.FunctionSymbolType(argTypes, retType) =>
        // e.g. we are in Magma and c is `op: tm U -> tm U -> tm U`
        // Then Hom(Magma) contains: `op_h: ⊦∀[x_0:tm U_d]∀[x_1:tm U_d](U_h (op_d x_0 x_1))≐(op_c (U_h x_0) (U_h x_1))`.
        //
        // Ideally we would generate the assignment `op_h = forallI ([x] forallI ([y] refl))`
        // But constructing the types in the forallIs (that are implicit in the surface syntax) is tedious.
        List(
          (hom(name), SFOL.sketchLazy("prove via `forallI [x_0] ... forallI [x_n] refl`"))
        )

      case SFOL.PredicateSymbolType(_) =>
        // As for function symbol types above, constructing the actual proof is easy in surface syntax,
        // but tedious when constructing actual [[Term]]s.
        List(
          (hom(name), SFOL.sketchLazy("prove via `forallI [x_0] ... forallI [x_n] implI [y] y`"))
        )

      case SFOL.AxiomSymbolType() =>
        // nothing to do
        Nil

      case _ =>
        NotApplicable(c)
    })
  }
}

/**
  * Creates the view `img: SUB(X) -> HOM(X)` representing the homomorphism's image as a submodel of `X`.
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
            case (tp, i) => ("y" + UnicodeStrings.subscriptInteger(i), tp)
          }
          val varPreimages = argTypes.zipWithIndex.map {
            case (tp, i) => ("x" + UnicodeStrings.subscriptInteger(i), tp)
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

/**
  * Creates the view `ker: CONG(X) -> HOM(X)` representing the kernel of the homomorphism.
  *
  * At every SFOL type `U: tp` in `X`, the congruence will be `x ~^U y <=> h^U(x) ≐ h^U(y)`
  * for `x, y: tm U`.
  */
object HomKernelConnector extends SimpleLinearConnector with SystematicRenamingUtils {
  final override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_ker_connector")
  final override val in: LinearModuleTransformer = CongOperator
  final override val out: LinearModuleTransformer = HomOperator

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_hom_ker")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)] = {
    val REL_ACCESSOR = LocalName("rel") // the relation field of the Mod type of the equivalence relation theory

    val par = CongOperator.par.coercedTo(state)
    val quot = CongOperator.quot.coercedTo(state)

    val dom = HomOperator.dom.coercedTo(state)
    val cod = HomOperator.cod.coercedTo(state)
    val hom = HomOperator.hom.coercedTo(state)

    val parentStructureCopies = List(
      (par(name), dom(c))
    )

    parentStructureCopies ::: (tp match {
      case SFOL.TypeSymbolType() =>
        // construct assignment `U^q = [| rel = [x y: tm Uᵈ] Uʰ x ≐ Uʰ y |]`
        val r = RecExp(
          OML(REL_ACCESSOR, tp = None, df = Some(
            Lambda(LocalName("x"), SFOL.tm(dom(c)),
              Lambda(LocalName("y"), SFOL.tm(dom(c)),
                SFOL.eq(
                  SFOL.tm(cod(c)),
                  ApplySpine(hom(c), OMV("x")),
                  ApplySpine(hom(c), OMV("y"))
                )
              )
            )
          ))
        )

        List(
          (quot(name), r)
        )

      case SFOL.FunctionSymbolType(_, _) =>
        List(
          //  ⊦∀[x⁰₀:Uᵖ]∀[x⁰₁:Uᵖ]∀[x¹₀:Uᵖ]∀[x¹₁:Uᵖ](U_q x⁰₀ x⁰₁)∧(U_q x¹₀ x¹₁)⇒(U_q (opᵖ x⁰₀ x¹₀) (opᵖ x⁰₁ x¹₁))
          (quot(name), SFOL.sketchLazy(s"""
Prove $name is a congruence wrt. kernel property.
Consider the case of a binary operator op. There, our proof goal would be `(U_q (opᵖ x⁰₀ x¹₀) (opᵖ x⁰₁ x¹₁))`,
or with assignments of this morphism inserted, `Uʰ (opᵈ x⁰₀ x¹₀) ≐ Uʰ (opᶜ x⁰₁ x¹₁)`.

   Uʰ (opᵈ x⁰₀ x¹₀)
≐ opᶜ (Uʰ x⁰₀) (Uʰ x¹₀)    // by hom. condition on Uʰ
≐ opᶜ (Uʰ x⁰₁) (Uʰ x¹₁)    // by relatedness of arguments
≐  Uʰ (opᶜ x⁰₁ x¹₁)        // by hom. condition on Uʰ, qed.
"""))
        )

      case SFOL.PredicateSymbolType(_) =>
        NotApplicable(c, s"""
The kernel of homomorphisms is *not* a congruence if the original signature
contained a predicate symbol.
Namely, homomorphism models as from ${HomOperator.getClass.getSimpleName} only preserve
applied predicate symbols, but not need reflect them.
But such reflection is needed to prove the congruence condition on the predicate symbol here.
""")

      case _ =>
        NotApplicable(c)
    })
  }
}
