package info.kwarc.mmt.moduleexpressions.newoperators

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{DefaultLinearStateOperator, DiagramInterpreter, Module, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.{ApplySpine, FunType, Lambda}
import info.kwarc.mmt.moduleexpressions.newoperators.OpUtils.{GeneralApplySpine, GeneralLambda}

/*
  Quotient operator (needs quotient types in logic)

  t: tp
    |-> t^p: tp
        t^q: tm t^p -> tm t^p -> prop
        <axioms on t^q>

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

  map `mod: T -> Quot(T)`

  t |-> t^p quot. t^q
  f |-> [x_1: tm (t_1^p quot. t_1^q), ..., x_n: tm (t_n^p quot. t_n^q)]
           eqv. class of (f^p (some repr of x_1 in t_1^p) ... (some repr of x_n in t_n^p))
  c |-> [x_1: tm (t_1^p quot. t_1^q), ..., x_n: tm (t_n^p quot. t_n^q)]
           (c^p (some repr of x_1 in t_1^p) ... (some repr of x_n in t_n^p))
  ax |-> ???
 */
object QuotOperator {} // extends SimpleLinearOperator with DefaultStateOperator with SystematicRenamingUtils {

object SubOperator extends SimpleLinearOperator with DefaultLinearStateOperator with SystematicRenamingUtils {
  override val head: GlobalName = Path.parseS("latin:/algebraic/diagop-test?AlgebraicDiagOps?sub_operator")

  override protected val operatorDomain: MPath = Path.parseM("latin:/?SFOLEQND")
  override protected val operatorCodomain: MPath = Path.parseM("latin:/?SFOLEQND")

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_sub")

  override protected val connectionTypes = List(
    InToOutMorphismConnectionType.suffixed("_parentmodel"),
    InToOutMorphismConnectionType.suffixed("_submodel")
  )

  override protected def applyConstantSimple(module: Module, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: SubOperator.LinearState): List[List[SimpleConstant]] = {
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
