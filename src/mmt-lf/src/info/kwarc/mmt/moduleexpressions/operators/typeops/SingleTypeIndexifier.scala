package info.kwarc.mmt.moduleexpressions.operators.typeops

import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, SimpleStep}
import info.kwarc.mmt.lf.{ApplySpine, FunTerm, FunType}
import info.kwarc.mmt.moduleexpressions.operators._
import info.kwarc.mmt.moduleexpressions.operators.typeops.ComputeSingleTypeIndexed.renameUndefinedSortDeclaration

import scala.collection.mutable

private object TypeOperator extends TheoryScala {
  val _base = DPath(URI("https://example.com/diagops"))
  val _name = LocalName("TypeOperator")

  val typeOp: GlobalName = _path ? "typeOp"
}

object SingleTypeIndexifier extends UnaryConstantScala(Combinators._path, "single_typeindexifier")

final class ComputeSingleTypeIndexedHelperContext(val abstractedDeclsSoFar: mutable.ListBuffer[LocalName] = mutable.ListBuffer.empty) extends LinearUnaryTheoryOperatorContext

object ComputeSingleTypeIndexed extends FunctorialDiagramOperatorComputationRule[ComputeSingleTypeIndexedHelperContext](SingleTypeIndexifier) {
  override val unaryConstant: UnaryConstantScala = SingleTypeIndexifier

  override protected def initialTheoryHelperContext: ComputeSingleTypeIndexedHelperContext
  = new ComputeSingleTypeIndexedHelperContext()

  override def applicableOnTheory(thy: AnonymousTheory): Boolean = {
    // TODO: Check that the meta-theory contains SFOL (ex. of implicit morphism, I guess) */
    true
  }

  def renameUndefinedSortDeclaration(oldName: LocalName): LocalName = oldName.steps match {
    case someSteps :+ SimpleStep(lastName) => LocalName(someSteps :+ SimpleStep(lastName + "_indirection"))
    case _ => ???
  }

  /**
    * The precedence of the type indirection operator replacing undefined sort declarations.
    *
    * E.g. `a: tp` is an undefined sort declaration, hence is being replaced by `a_indirection: tp -> tp` and
    * this variable specifies the precedence of the [[TextNotation]] of that new declaration, which is being
    * produced in [[transformSingleDeclaration()]] below.
    */
  private val TYPE_INDIRECTION_OPERATOR_PRECEDENCE = Precedence(Finite(-1000000), loseTie = false)

  override def transformSingleDeclaration(decl: OML, helperContext: ComputeSingleTypeIndexedHelperContext)
  : OperatorResult[(OML, ComputeSingleTypeIndexedHelperContext)] = decl match {
    case SFOL.UndefinedSortDeclaration() =>
      val newNotation = TextNotation(
        fixity = Mixfix(List(
          Delim("&" + decl.name.last),
          SimpArg(1)
        )),
        precedence = TYPE_INDIRECTION_OPERATOR_PRECEDENCE, // lose-tie flag being false correct?
        meta = None // None signals same meta theory as surrounding theory
      )

      val newDecl = OML(
        name = renameUndefinedSortDeclaration(decl.name),

        // Old type was just `tp`, new type is `tp -> tp`
        tp = Some(FunType(List((None, OMID(TypedTerms.typeOfSorts))), OMID(TypedTerms.typeOfSorts))),
        df = None,
        Some(newNotation),
        decl.featureOpt,
      )

      helperContext.abstractedDeclsSoFar += decl.name

      TransformedResult((newDecl, helperContext))

    case OML(_, Some(SFOL.FunctionOrPredicateType(_)), _, _, _) =>
      // Sample input declaration: `c: tm a -> tm b`
      // Desired output declaration: `c: {u: tp} tm (&a u) -> tm (&b u)`
      val newDecl = SingleTypeIndexer.typeIndex(decl, helperContext.abstractedDeclsSoFar.toList)
      helperContext.abstractedDeclsSoFar += newDecl.name

      TransformedResult((newDecl, helperContext))

    case _ => NotApplicable()
  }
}

private object SingleTypeIndexer {
  final private val TYPE_INDIRECTION_VARIABLE_NAME = LocalName("U")

  def typeIndex(decl: OML, abstractedDeclsSoFar: List[LocalName]): OML = {
    val adder = new DependencyAndTypeOperatorAdder(abstractedDeclsSoFar)

    val tmpDecl = adder.traverse(decl)(Context.empty, Unit).asInstanceOf[OML] // return value is indeed an OML by recursion

    tmpDecl.copy(
      tp = tmpDecl.tp.map(dependentlyTypeTypeComponent),
      df = tmpDecl.df.map(lambdaBindDefComponent),

      // Adjust notation for the single dependent type we added
      nt = tmpDecl.nt.map(notation => notation.copy(fixity = notation.fixity.addInitialImplicits(1)))
    )
  }

  def dependentlyTypeTypeComponent(typeComponent: Term): Term = {
    // Transform every type `t` to `{u: tp} t`
    FunType(List(
      (Some(TYPE_INDIRECTION_VARIABLE_NAME), OMID(TypedTerms.typeOfSorts))
    ), typeComponent)
  }

  def lambdaBindDefComponent(defComponent: Term): Term = {
    // Transform every definiens `d` to `[u: tp] d`
    FunTerm(List(
      (TYPE_INDIRECTION_VARIABLE_NAME, OMID(TypedTerms.typeOfSorts))
    ), defComponent)
  }

  /**
    * This [[StatelessTraverser]] adds dependency and type operators to a [[Term]].
    *
    * Concretely,
    *
    *  - if the term contains an OML reference to another declaration needing the type indirection, then it's added as
    * an argument.
    * Say we previously had `b: tp` and thus abstractedDeclsSoFar contains LocalName("b"). Then if the term to be transformed
    * contains `b`, then that is replaced by `b u`, where `u` is the type indirection.
    *
    *  - if the term contains `tm x`, where `x` is an OML reference, then that is replaced by `tm (&x u)`, where
    * `&x` is the type indirection operator for `x` as given by [[ComputeSingleTypeIndexed.renameUndefinedSortDeclaration()]].
    *  - adding
    *
    * @param abstractedDeclsSoFar
    */
  final private class DependencyAndTypeOperatorAdder(private val abstractedDeclsSoFar: List[LocalName]) extends StatelessTraverser {
    def traverse(t: Term)(implicit con: Context, state: Unit): Term = t match {
      // Transform `b: tp ... b` into `... b u`
      case ApplySpine(referencedDecl@OML(name, _, _, _, _), args) if abstractedDeclsSoFar.contains(name) =>
        ApplySpine(referencedDecl, OMV(LocalName("u")) :: args: _*)

      // Transform `tm a` into `tm (&a u)`
      case ApplySpine(OMID(TypedTerms.termsOfSort), List(sort: OML)) if abstractedDeclsSoFar.contains(sort.name) =>
        ApplySpine(
          OMID(TypedTerms.termsOfSort),
          ApplySpine(
            OML(renameUndefinedSortDeclaration(sort.name)), // this gives us `&a`
            OMV(TYPE_INDIRECTION_VARIABLE_NAME)
          )
        )

      case t => Traverser(this, t)
    }
  }

}