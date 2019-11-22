package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName}
import info.kwarc.mmt.lf.{ApplySpine, FunTerm, FunType}
import info.kwarc.mmt.moduleexpressions.operators.ComputeSingleTypeIndexed.renameUndefinedSortDeclaration

import scala.collection.mutable

private object TypeOperator extends TheoryScala {
  val _base = DPath(URI("https://example.com/diagops"))
  val _name = LocalName("TypeOperator")

  val typeOp: GlobalName = _path ? "typeOp"
}

object SingleTypeIndexifier extends UnaryConstantScala(Combinators._path, "single_typeindexifier")

final class ComputeSingleTypeIndexedHelperContext(val knownSorts: mutable.ListBuffer[LocalName] = mutable.ListBuffer.empty) extends LinearUnaryTheoryOperatorContext

object ComputeSingleTypeIndexed extends FunctorialDiagramOperatorComputationRule[ComputeSingleTypeIndexedHelperContext](SingleTypeIndexifier) {
  override val unaryConstant: UnaryConstantScala = SingleTypeIndexifier

  override protected def initialTheoryHelperContext: ComputeSingleTypeIndexedHelperContext
  = new ComputeSingleTypeIndexedHelperContext()

  override def applicableOnTheory(thy: AnonymousTheory): Boolean = {
    // TODO: Check that the meta-theory contains SFOL (ex. of implicit morphism, I guess) */
    true
  }

  def renameUndefinedSortDeclaration(oldName: LocalName): LocalName = {
    oldName / "_indirection"
  }

  override def transformSingleDeclaration(decl: OML, helperContext: ComputeSingleTypeIndexedHelperContext)
  : OperatorResult[(OML, ComputeSingleTypeIndexedHelperContext)] = decl match {
    case SFOL.UndefinedSortDeclaration() =>
      val newDecl = OML(
        name = renameUndefinedSortDeclaration(decl.name),

        // Old type was just `tp`, new type is `tp -> tp`
        tp = Some(FunType(List((None, OMID(TypedTerms.typeOfSorts))), OMID(TypedTerms.typeOfSorts))),
        df = None,
        decl.nt, // TODO adjust notation?
        decl.featureOpt,
      )

      helperContext.knownSorts += decl.name

      TransformedResult((newDecl, helperContext))

    case OML(_, Some(SFOL.FunctionOrPredicateType(_)), _, _, _) =>
      // Sample input declaration: `c: tm a -> tm b`
      // Desired output declaration: `c: {u: tp} tm (&a u) -> tm (&b u)`
      val newDecl = SingleTypeIndexer.typeIndex(decl, helperContext.knownSorts.toList)

      // TODO: Adjust indices in notation container
      TransformedResult((newDecl, helperContext))

    case _ => NotApplicable()
  }
}

private object SingleTypeIndexer {
  def typeIndex(decl: OML, declaredSorts: List[LocalName]): OML = {
    val adder = new DependencyAndTypeOperatorAdder(declaredSorts)

    val tmpDecl = adder.traverse(decl)(Context.empty, Unit).asInstanceOf[OML] // return value is indeed an OML by recursion

    tmpDecl.copy(
      tp = tmpDecl.tp.map(dependentlyTypeTypeComponent),
      df = tmpDecl.df.map(lambdaBindDefComponent)
    )
  }

  def dependentlyTypeTypeComponent(typeComponent: Term): Term = {
    // Transform every type `t` to `{u: tp} t`
    FunType(List(
      (Some(LocalName("u")), OMID(TypedTerms.typeOfSorts))
    ), typeComponent)
  }

  def lambdaBindDefComponent(defComponent: Term): Term = {
    // Transform every definiens `d` to `[u: tp] d`
    FunTerm(List(
      (LocalName("u"), OMID(TypedTerms.typeOfSorts))
    ), defComponent)
  }

  final private class DependencyAndTypeOperatorAdder(private val declaredSorts: List[LocalName]) extends StatelessTraverser {
    def traverse(t: Term)(implicit con: Context, state: Unit): Term = t match {
      // Transform `b: tp ... b` into `... b u`
      case ApplySpine(referencedDecl@OML(name, _, _, _, _), args) if declaredSorts.contains(name) =>
        ApplySpine(referencedDecl, OMV(LocalName("u")) :: args: _*)

      // Transform `tm a` into `tm (&a u)`
      case ApplySpine(OMID(TypedTerms.termsOfSort), List(sort: OML)) if declaredSorts.contains(sort.name) =>
        ApplySpine(
          OMID(TypedTerms.termsOfSort),
          ApplySpine(
            OML(renameUndefinedSortDeclaration(sort.name)), // this gives us `&a`
            OMV(LocalName("u"))
          )
        )

      case t => Traverser(this, t)
    }
  }

}