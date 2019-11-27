/**
  * Typifier employed to transform the file logic/fol.mmt in archive MMT/LATIN2 to logic/sfol.mmt
  */

package info.kwarc.mmt.moduleexpressions.operators.typeops

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.objects.{AnonymousTheory, Context, OML, StatelessTraverser, Term, Traverser}
import info.kwarc.mmt.api.uom.UnaryConstantScala
import info.kwarc.mmt.lf.{FunTerm, FunType}
import info.kwarc.mmt.moduleexpressions.operators._

object FOLTypifier extends UnaryConstantScala(Combinators._path, "fol_typifier")

final class ComputeFOLTypifiedHelperContext() extends LinearTheoryOperatorContext

/**
  * Typifier for LATIN2 FOL to LATIN2 SFOL.
  *
  * @example The diagram operator operates decl-by-decl and e.g.
  *          turns `forall : (term ⟶ prop) ⟶ prop❘# ∀ 1❙` into `forall : {A} (tm A ⟶ prop) ⟶ prop❘# ∀ 2❙`.
  */
object ComputeFOLTypified extends FunctorialLinearDiagramOperator[ComputeFOLTypifiedHelperContext](FOLTypifier) {
  override protected def initialTheoryHelperContext: ComputeFOLTypifiedHelperContext
  = new ComputeFOLTypifiedHelperContext()

  override def applicableOnTheory(thy: AnonymousTheory): Boolean = {
    // TODO: Do an appropriate check
    true
  }

  override def transformSingleDeclaration(decl: OML, helperContext: ComputeFOLTypifiedHelperContext)
  : OperatorResult[(OML, ComputeFOLTypifiedHelperContext)] = {
    TransformedResult((FOLTyper.typify(decl), helperContext))
  }
}

private object FOLTyper {
  final private val TYPE_VARIABLE_NAME = LocalName("A")

  def typify(decl: OML): OML = {
    val adder = new DependenciesAndTypeOperatorAdder

    val tmpDecl = adder.traverse(decl)(Context.empty, Unit).asInstanceOf[OML] // return value is indeed an OML by recursion

    tmpDecl.copy(
      tp = tmpDecl.tp.map(dependentlyTypeTypeComponent),
      df = tmpDecl.df.map(lambdaBindDefComponent),

      // Adjust notation for the dependent type we added
      nt = tmpDecl.nt.map(notation => notation.copy(fixity = notation.fixity.addInitialImplicits(1)))
    )
  }

  def dependentlyTypeTypeComponent(typeComponent: Term): Term = {
    val dependentlyBoundVariables = List(
      (Some(TYPE_VARIABLE_NAME), LATIN2Environment.Logic.Types.tp())
    )

    FunType(dependentlyBoundVariables, typeComponent)
  }

  def lambdaBindDefComponent(defComponent: Term): Term = {
    FunTerm(List(
      (TYPE_VARIABLE_NAME, LATIN2Environment.Logic.Types.tp())
    ), defComponent)
  }

  final private class DependenciesAndTypeOperatorAdder extends StatelessTraverser {
    def traverse(t: Term)(implicit con: Context, state: Unit): Term = t match {
      case LATIN2Environment.Logic.Terms.term() =>
        LATIN2Environment.Logic.TypedTerms.tm(OML(TYPE_VARIABLE_NAME))
      case t => Traverser(this, t)
    }
  }

}
