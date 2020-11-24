package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.{ApplySpine, FunType, Lambda}
import info.kwarc.mmt.moduleexpressions.newoperators.OpUtils.{GeneralApplySpine, GeneralLambda}

object SubOperator extends SimpleLinearOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?sub_operator")
  override val operatorDomain: MPath = Path.parseM("latin:/?SFOLEQND")
  override val operatorCodomain: MPath = Path.parseM("latin:/?SFOLEQND")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_sub")

  override protected def applyConstantSimple(container: SubOperator.Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: SubOperator.LinearState): List[(LocalName, Term, Option[Term])] = {
    val par = getRenamerFor("p") // parent symbol copy
    val sub = getRenamerFor("s") // substructure symbol/condition
    val parCopy = (par(name), par(tp), df.map(par(_)))

    tp match {
      case SFOL.TypeSymbolType() =>
        // input:  t^p: tp
        // output: t^s: tp, t^s: tm t^p -> prop
        val tsType = FunType(
          List((None, SFOL.tm(OMS(par(c.path))))),
          SFOL.prop.term
        )

        List(parCopy, (sub(name), tsType, df.map(sub(_))))

      case SFOL.FunctionSymbolType(argTypes, retType) =>
        // input:   f: tm t_1 -> ... -> tm t_n -> tm t  (here: argTypes = List(t_1, ..., t_n), retType = t)
        //

          // main results:
          //       f^p: tm t_1^p -> ... -> tm t_n^p -> tm t^p
          //       f^s: |- forall x_1 ... x_n: (t_1^s x_1) /\ ... /\ (t_n^s x_n) => t^s (f^p x_1 ... x_n)
          val closureConstant = {
            val forallCtx = OpUtils.bindFresh(tp, argTypes.map(argTp => SFOL.tm(OMS(par(argTp)))))

            // construct `(t_1^s x_1) /\ ... /\ (t_n^s x_n)` if n >= 1
            val antecedent: Option[Term] = if (argTypes.isEmpty) None else Some(argTypes.zip(forallCtx).map {
              case (argTp, vd) => ApplySpine(OMS(sub(argTp)), OMV(vd.name))
            }.reduceLeft(SFOL.and(_, _)))

            // construct `t^s (f^p x_1 ... x_n)`
            val consequence = ApplySpine(
              OMS(sub(retType)),
              GeneralApplySpine(OMS(par(c.path)), forallCtx.map(vd => OMV(vd.name)): _*)
            )

            val closureCondition = SFOL.ded(antecedent match {
              case Some(antecedent) => SFOL.forallMany(forallCtx, SFOL.impl(antecedent, consequence))
              case None =>
                assert(forallCtx.isEmpty)
                consequence
            })

            (
              sub(name),
              closureCondition,
              df.map(_ => SFOL.sketch(OMV("<todo:implicit arg>"), "provable"))
            )
          }

        List(parCopy, closureConstant)

      case SFOL.PredicateSymbolType(_) =>
        // input: p: tm t_1 -> ... -> tm t_n -> prop
        // outputs:
        //   - main results: parCopy
        List(parCopy)

      case SFOL.AxiomSymbolType() =>
        // todo: alternative: use predicate subtypes instead of relativation
        val relativizedAxiom = (
          sub(name),
          relativizeQuantifiers(tp, state.outerContext, par.apply(_), sub.apply(_)),
          df.map(_ => SFOL.sketch(OMV("<todo:implicit arg>"), "provable"))
        )

        List(parCopy, relativizedAxiom)

      case _ =>
        state.registerSkippedDeclaration(c)

        diagInterp.errorCont(InvalidElement(c, "Sub operator cannot process element: " +
          "its type is not well-patterned (i.e. has one of the forms Sub is applicable on), skipping element."))
        Nil
    }
  }

  private def relativizeQuantifiers(t: Term, context: Context, par: Term => Term, sub: Term => Term): Term = {
    new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case SFOL.forall(tp, oldBody) =>

          val newName = LocalName.random("relativized_var")
          SFOL.forall(par(tp), Lambda(
            name = newName,
            par(tp),
            SFOL.impl(
              ApplySpine(sub(tp), OMV(newName)),
              ApplySpine(Traverser(this, oldBody), OMV(newName))
            )
          ))

        case _ => par(Traverser(this, t))
      }
    }.apply(t, context)
  }
}

object SubParentConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?sub_submodel_conector"),
  SubOperator
) with SystematicRenamingUtils {
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_parmodel")

  override protected def applyConstantSimple(container: SubParentConnector.Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Term)] = {
    // todo: SubOperator already declares par, we shouldn't need to redeclare this here!
    val par = getRenamerFor("p")
    List((name, tp, par(c)))
  }
}

object SubSubmodelConnector extends SimpleInwardsConnector(
  Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?submodel_conector"),
  SubOperator
) with SystematicRenamingUtils {
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_submodel")

  override protected def applyConstantSimple(container: SubSubmodelConnector.Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: SubSubmodelConnector.LinearState): List[(LocalName, Term, Term)] = {
    val par = getRenamerFor("p") // parent symbol copy
    val sub = getRenamerFor("s") // substructure symbol/condition

    tp match {
      case SFOL.TypeSymbolType() =>
        List((name, tp, SFOL.predicateSubTp(par(c), sub(c))))

      case SFOL.FunctionSymbolType(argTypes, retType) =>
          // input (recall): f: tm t_1 -> ... -> tm t_n -> tm t
          // connection sub results:
          //       f := [x_1: ⦃t_1^s⦄, ..., x_n: ⦃t_n^s⦄] downcast of (f^p (x_1 ↑ ...) ... (x_n ↑ ... )) via closure property f^s

          val bindingCtx = OpUtils.bindFresh(tp, argTypes.map(argTp => {
            SFOL.tm(SFOL.predicateSubTp(OMS(par(argTp)), OMS(sub(argTp))))
          }))

          val body = SFOL.downcastParentElementToSubtype(
            parentTp = OMS(par(retType)),
            selectionFun = OMS(sub(retType)),
            parentElem = GeneralApplySpine(par(c), argTypes.zip(bindingCtx).map {
              case (argTp, vd) => SFOL.injectSubtypeElementIntoParent(
                parentTp = OMS(par(argTp)),
                selectionFun = OMS(sub(argTp)),
                subElem = OMV(vd.name)
              )
            } : _*),
            containmentProof = SFOL.sketch(OMV("<todo:implicit arg>"), s"provable via ${sub(name)}")
          )

          val assignment = GeneralLambda(bindingCtx, body)
          List((name, tp, assignment))

      case SFOL.PredicateSymbolType(argTypes) =>
        // output: p := [x_1: ⦃t_1^s⦄, ..., x_n: ⦃t_n^s⦄] f^p (x_1 ↑ ...) ... (x_n ↑ ...)

        val bindingCtx = OpUtils.bindFresh(tp, argTypes.map(argTp => {
          SFOL.tm(SFOL.predicateSubTp(OMS(par(argTp)), OMS(sub(argTp))))
        }))

        val body = GeneralApplySpine(par(c), argTypes.zip(bindingCtx).map {
          case (argTp, vd) => SFOL.injectSubtypeElementIntoParent(
            parentTp = OMS(par(argTp)),
            selectionFun = OMS(sub(argTp)),
            subElem = OMV(vd.name)
          )
        }: _*)

        val assignment = GeneralLambda(bindingCtx, body)
        List((name, tp, assignment))

      case SFOL.AxiomSymbolType() =>
        List((name, tp, SFOL.sketch(OMV("<todo: implicit arg>"), "provable")))

      case _ =>
        NotApplicable(c)
    }
  }
}

/*object SubOperator extends SimpleLinearOperator with DefaultLinearStateOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?sub_operator")

  override protected val operatorDomain: MPath = Path.parseM("latin:/?SFOLEQND")
  override protected val operatorCodomain: MPath = Path.parseM("latin:/?SFOLEQND")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_sub")

  override protected val connectionTypes = List(
    InToOutMorphismConnectionType.suffixed("_parentmodel"),
    InToOutMorphismConnectionType.suffixed("_submodel")
  )

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: SubOperator.LinearState): List[List[SimpleConstant]] = {
    val par = getRenamerFor("p") // parent symbol copy
    val sub = getRenamerFor("s") // substructure symbol/condition

    val parCopy = (par(name), par(tp), df.map(par(_)))
    val connParResults = ConnResults((name, tp, par(c)))

    tp match {
      case SFOL.TypeSymbolType() =>
        // input:  t^p: tp
        // output: t^s: tp, t^s: tm t^p -> prop
        val tsType = FunType(
          List((None, SFOL.tm(OMS(par(c.path))))),
          SFOL.prop.term
        )

        MainResults(parCopy, (sub(name), tsType, df.map(sub(_)))) ::: connParResults :::
          ConnResults((name, tp, SFOL.predicateSubTp(par(c), sub(c))))

      case SFOL.FunctionSymbolType(argTypes, retType) =>
        // input:   f: tm t_1 -> ... -> tm t_n -> tm t  (here: argTypes = List(t_1, ..., t_n), retType = t)
        //
        val mainResults = {
          // main results:
          //       f^p: tm t_1^p -> ... -> tm t_n^p -> tm t^p
          //       f^s: |- forall x_1 ... x_n: (t_1^s x_1) /\ ... /\ (t_n^s x_n) => t^s (f^p x_1 ... x_n)
          val closureConstant = {
            val forallCtx = OpUtils.bindFresh(tp, argTypes.map(argTp => SFOL.tm(OMS(par(argTp)))))

            // construct `(t_1^s x_1) /\ ... /\ (t_n^s x_n)` if n >= 1
            val antecedent: Option[Term] = if (argTypes.isEmpty) None else Some(argTypes.zip(forallCtx).map {
              case (argTp, vd) => ApplySpine(OMS(sub(argTp)), OMV(vd.name))
            }.reduceLeft(SFOL.and(_, _)))

            // construct `t^s (f^p x_1 ... x_n)`
            val consequence = ApplySpine(
              OMS(sub(retType)),
              GeneralApplySpine(OMS(par(c.path)), forallCtx.map(vd => OMV(vd.name)): _*)
            )

            val closureCondition = SFOL.ded(antecedent match {
              case Some(antecedent) => SFOL.forallMany(forallCtx, SFOL.impl(antecedent, consequence))
              case None =>
                assert(forallCtx.isEmpty)
                consequence
            })

            (
              sub(name),
              closureCondition,
              df.map(_ => SFOL.sketch(OMV("<todo:implicit arg>"), "provable"))
            )
          }

          MainResults(parCopy, closureConstant)
        }

        val connSubResults = {
          // input (recall): f: tm t_1 -> ... -> tm t_n -> tm t
          // connection sub results:
          //       f := [x_1: ⦃t_1^s⦄, ..., x_n: ⦃t_n^s⦄] downcast of (f^p (x_1 ↑ ...) ... (x_n ↑ ... )) via closure property f^s

          val bindingCtx = OpUtils.bindFresh(tp, argTypes.map(argTp => {
            SFOL.tm(SFOL.predicateSubTp(OMS(par(argTp)), OMS(sub(argTp))))
          }))

          val body = SFOL.downcastParentElementToSubtype(
            parentTp = OMS(par(retType)),
            selectionFun = OMS(sub(retType)),
            parentElem = GeneralApplySpine(par(c), argTypes.zip(bindingCtx).map {
              case (argTp, vd) => SFOL.injectSubtypeElementIntoParent(
                parentTp = OMS(par(argTp)),
                selectionFun = OMS(sub(argTp)),
                subElem = OMV(vd.name)
              )
            } : _*),
            containmentProof = SFOL.sketch(OMV("<todo:implicit arg>"), s"provable via ${sub(name)}")
          )

          val assignment = GeneralLambda(bindingCtx, body)
          ConnResults((name, tp, assignment))
        }

        mainResults ::: connParResults ::: connSubResults

      case SFOL.PredicateSymbolType(argTypes) =>
        // input: p: tm t_1 -> ... -> tm t_n -> prop
        // outputs:
        //   - main results: parCopy
        //   - connection results:
        //     - for parent model: connParResults
        //     - for submodel: connSubResults as defined below

        val connSubResults = {
          // output: p := [x_1: ⦃t_1^s⦄, ..., x_n: ⦃t_n^s⦄] f^p (x_1 ↑ ...) ... (x_n ↑ ...)

          val bindingCtx = OpUtils.bindFresh(tp, argTypes.map(argTp => {
            SFOL.tm(SFOL.predicateSubTp(OMS(par(argTp)), OMS(sub(argTp))))
          }))

          val body = GeneralApplySpine(par(c), argTypes.zip(bindingCtx).map {
            case (argTp, vd) => SFOL.injectSubtypeElementIntoParent(
              parentTp = OMS(par(argTp)),
              selectionFun = OMS(sub(argTp)),
              subElem = OMV(vd.name)
            )
          }: _*)

          val assignment = GeneralLambda(bindingCtx, body)
          ConnResults((name, tp, assignment))
        }

        MainResults(parCopy) ::: connParResults ::: connSubResults

      case SFOL.AxiomSymbolType() =>
        // todo: alternative: use predicate subtypes instead of relativation
        val relativizedAxiom = (
          sub(name),
          relativizeQuantifiers(tp, state.outerContext, par.apply(_), sub.apply(_)),
          df.map(_ => SFOL.sketch(OMV("<todo:implicit arg>"), "provable"))
        )

        MainResults(parCopy, relativizedAxiom) ::: connParResults :::
          ConnResults((name, tp, SFOL.sketch(OMV("<todo: implicit arg>"), "provable")))

      case _ =>
        state.registerSkippedDeclaration(c)

        diagInterp.errorCont(InvalidElement(c, "Sub operator cannot process element: " +
          "its type is not well-patterned (i.e. has one of the forms Sub is applicable on), skipping element."))
        NoResults
    }
  }

  private def relativizeQuantifiers(t: Term, context: Context, par: Term => Term, sub: Term => Term): Term = {
    new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case SFOL.forall(tp, oldBody) =>

          val newName = LocalName.random("relativized_var")
          SFOL.forall(par(tp), Lambda(
            name = newName,
            par(tp),
            SFOL.impl(
              ApplySpine(sub(tp), OMV(newName)),
              ApplySpine(Traverser(this, oldBody), OMV(newName))
            )
          ))

        case _ => par(Traverser(this, t))
      }
    }.apply(t, context)
  }
}
*/