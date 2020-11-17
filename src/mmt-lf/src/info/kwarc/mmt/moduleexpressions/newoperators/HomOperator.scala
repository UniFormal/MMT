package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{DefaultLinearStateOperator, DiagramInterpreter, Module, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.{ApplySpine, FunType, Lambda}
import info.kwarc.mmt.moduleexpressions.newoperators.OpUtils.GeneralApplySpine

object HomOperator extends SimpleLinearOperator with DefaultLinearStateOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?hom_operator")

  override protected val operatorDomain: MPath = Path.parseM("latin:/?SFOLEQND")
  override protected val operatorCodomain: MPath = Path.parseM("latin:/?SFOLEQND")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_hom")

  override protected val connectionTypes = List(
    InToOutMorphismConnectionType.suffixed("_dom"),
    InToOutMorphismConnectionType.suffixed("_cod"),
  )

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

  override protected def applyConstantSimple(module: Module, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: HomOperator.LinearState): List[List[SimpleConstant]] = {

    // Hom(-) copies every input constant to two systematically renamed copies for domain and codomain of the homomorphism
    val dom = getRenamerFor("d")
    val cod = getRenamerFor("c")

    // and introduces for some input constants a new "homomorphism constant" accounting for a homomorphism condition
    val hom = getRenamerFor("h")

    // Some abbreviations for things we return that are common to all forms of input constants that we treat below
    //
    // Below, we then either return `MainResults(mainConstantCopies) ::: connResults` or
    //                              `MainResults(mainConstantCopies, homomorphismConstant) ::: connResults`
    val mainConstantCopies = (dom(name), dom(tp), df.map(dom(_))) :: (cod(name), cod(tp), df.map(cod(_))) :: Nil
    val connResults = ConnResults((name, tp, dom(c))) ::: ConnResults((name, tp, cod(c)))

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

    tp match {
      case SFOL.TypeSymbolType() =>
        // input:  t: tp
        // output: t^d: tp, t^c: tp, t^h: tm t^d -> t^c
        val thType = FunType(
          List((None, SFOL.tm(OMS(dom(c.path))))),
          SFOL.tm(OMS(applyModulePath(module.path) ? cod(name)))
        )

        MainResults(mainConstantCopies, (hom(name), thType, df.map(hom(_)))) ::: connResults

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

        MainResults(mainConstantCopies, homomorphismConstant) ::: connResults

      case SFOL.PredicateSymbolType(argTypes) =>
        if (!df.forall(isMonotone)) {
          throw GeneralError(s"Hom operator cannot process SFOL predicate symbol (assignment) ${c.path} " +
            s"that is not monotone. See 'Structure-Preserving Diagram Operators' paper for reasons.")
        }

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

        MainResults(mainConstantCopies, homomorphismConstant) ::: connResults

      case SFOL.AxiomSymbolType() => MainResults(mainConstantCopies) ::: connResults

      case _ =>
        throw GeneralError(s"Hom operator cannot process SFOL constant ${c.path} of unknown form (neither type, " +
          s"function, predicate, nor axiom symbol.")
    }
  }

  private def isMonotone(t: Term)(implicit operatorState: LinearState): Boolean = {
    // allowed operations apart from all the symbols from operatorState.processedDeclarations.
    val allowedOps: List[GlobalName] = List(
      Lambda.path,
      SFOL.and.path, SFOL.or.path, SFOL.eq.path, SFOL.exists.path
    )
    sealed class MonotonicityStatus(var isMonotone: Boolean)

    val monotonicityTraverser = new Traverser[MonotonicityStatus] {
      override def traverse(t: Term)(implicit con: Context, state: MonotonicityStatus): Term = {
        if (!state.isMonotone) {
          // no need to recurse further
          t
        } else t match {
          case OMS(p) if operatorState.processedDeclarations.exists(_.path == p) => t
          case OMS(p) if allowedOps.contains(p) => t
          case OMS(_) =>
            // todo: log the path to the non-monotone op that occurred here
            state.isMonotone = false
            t

          case _ => Traverser(this, t)
        }
      }
    }

    val monotonicity = new MonotonicityStatus(true)
    monotonicityTraverser(t, monotonicity, operatorState.outerContext)

    monotonicity.isMonotone
  }
}

private[newoperators] object OpUtils {
  def bindFresh(t: Term, argTypes: List[Term]): Context = {
    val vars: List[OMV] = Range(1, argTypes.size + 1).map(idx => OMV(s"x_${idx}")).toList

    Context(vars.zip(argTypes).map {
      case (boundVar, arg) => VarDecl(boundVar.name, arg)
    } : _*)
  }

  /**
    * Like [[ApplySpine]] but doesn't generate an [[OMA]] upon application with 0 arguments.
    * Instead it just returns 'f' in that case.
    */
  def GeneralApplySpine(f: Term, a: Term*): Term = if (a.isEmpty) f else ApplySpine(f, a : _*)

  /**
    * Like [[Lambda.apply()]] but doesn't generate an empty [[OMBINDC]] upon application with
    * an empty context. Instead it just returns 'body' in that case.
    */
  def GeneralLambda(ctx: Context, body: Term): Term = if (ctx.isEmpty) body else Lambda(ctx, body)
}