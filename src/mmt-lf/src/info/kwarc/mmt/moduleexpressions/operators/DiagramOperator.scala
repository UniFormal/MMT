package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._

import scala.collection.mutable

sealed abstract class OperatorResult[T]

case class TransformedResult[T](result: T) extends OperatorResult[T]

case class NotApplicable[T]() extends OperatorResult[T]

// Diagram operators
// ==========================
trait DiagramOperator {
  def transformDiagram(diag: AnonymousDiagram): OperatorResult[AnonymousDiagram]
}

// Theory operators
// ==========================

trait UnaryTheoryOperator {
  def transformTheory(thy: AnonymousTheory): OperatorResult[AnonymousTheory]
}

trait SublinearUnaryTheoryOperator[T] extends UnaryTheoryOperator {
  protected def initHelperContext: T

  override final def transformTheory(thy: AnonymousTheory): OperatorResult[AnonymousTheory] = {
    if (!applicableOnTheory(thy)) {
      return NotApplicable()
    }

    // I planned to use reduceLeft in this method, but due to very strange typechecking issues
    // I could not resolve within an hour (e.g. see https://imgur.com/a/velJzt6), I just switched
    // to an ordinary for loop.

    val declarations = thy.getDeclarations
    val transformedDeclarations = mutable.ListBuffer[OML]()
    var context = Context.empty
    var helperContext = initHelperContext

    for (currentDecl <- declarations) {
      transformDeclaration(currentDecl, context, helperContext) match {
        case TransformedResult((newDecls, newHelperContext)) =>
          transformedDeclarations ++= newDecls
          context = context ++ currentDecl.vd
          helperContext = newHelperContext

        case _ => /* do nothing, just skip declaration */
      }
    }

    TransformedResult(AnonymousTheory(thy.mt, transformedDeclarations.toList))
  }

  def applicableOnTheory(thy: AnonymousTheory): Boolean = {
    true
  }

  def transformDeclaration(decl: OML, context: Context, helperContext: T): OperatorResult[(List[OML], T)]
}

trait LinearUnaryTheoryOperator[T] extends SublinearUnaryTheoryOperator[T] {
  override final def transformDeclaration(decl: OML, context: Context, helperContext: T): OperatorResult[(List[OML], T)] = {
    transformSingleDeclaration(decl, context, helperContext) match {
      case TransformedResult((newDecl, newHelperContext)) =>
        TransformedResult((List(newDecl), newHelperContext))
      case NotApplicable() => NotApplicable()
    }
  }

  def transformSingleDeclaration(decl: OML, context: Context, helperContext: T): OperatorResult[(OML, T)]
}

// Morphism operators
// ==========================
trait UnaryMorphismOperator {
  def transformMorphism(
                         morphism: AnonymousMorphism,
                         transformedDomain: AnonymousTheory,
                         transformedCodomain: AnonymousTheory
                       ): OperatorResult[AnonymousMorphism]
}

trait MorphismOperatorFromLinearTheoryOperatorMixin[T] extends UnaryMorphismOperator with LinearUnaryTheoryOperator[T] {
  override def transformMorphism(morphism: AnonymousMorphism, transformedDomain: AnonymousTheory, transformedCodomain: AnonymousTheory): OperatorResult[AnonymousMorphism] = {
    ???
  }
}

trait FunctorialLinearDiagramOperatorMixin[T] extends DiagramOperator with MorphismOperatorFromLinearTheoryOperatorMixin[T] {

  // Suffix nodes and arrows so that they really get available at the controller
  // by [[InstanceElaborator]], which namely skips adding elements whose name already
  // exists in the global namespace

  private def permuteLabel(label: LocalName, isDistNode: Boolean): LocalName = {
    if (isDistNode) {
      LocalName("pres")
    } else {
      label
    }
  }

  override def transformDiagram(diag: AnonymousDiagram): OperatorResult[AnonymousDiagram] = {
    val newNodes = diag.nodes.map(node => transformTheory(node.theory) match {
      case TransformedResult(newTheory) => DiagramNode(permuteLabel(node.label, diag.distNode.contains(node.label)), newTheory)
      case _ => ???
    })

    val newArrows = diag.arrows.map(arrow => {
      val transformedDomain = newNodes.find(_.label == arrow.from).get.theory
      val transformedCodomain = newNodes.find(_.label == arrow.to).get.theory

      transformMorphism(arrow.morphism, transformedDomain, transformedCodomain) match {
        case TransformedResult(newMorphism) => arrow.copy(
          label = arrow.label,
          morphism = newMorphism,
          from = permuteLabel(arrow.from, diag.distNode.contains(arrow.from)),
          to = permuteLabel(arrow.to, diag.distNode.contains(arrow.to))
        )
        case _ => ???
      }
    })

    TransformedResult(AnonymousDiagram(newNodes, newArrows, diag.distNode.map(permuteLabel(_, isDistNode = true))))
  }
}

trait UnaryDiagramOperatorComputationRule extends ComputationRule with DiagramOperator {

  val unaryConstant: UnaryConstantScala

  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    tm match {
      case unaryConstant(argumentTerm) => Common.asAnonymousDiagram(solver, argumentTerm) match {
        case Some(inputDiagram) => transformDiagram(inputDiagram) match {
          case TransformedResult(outputDiagram) => {
            Simplify(outputDiagram.toTerm)
          }
          case _ => Recurse
        }
        case _ => RecurseOnly(List(1))
      }
      case _ => Recurse
    }
  }
}

abstract class FunctorialDiagramOperatorComputationRule[T](unaryConstant: UnaryConstantScala)
  extends ComputationRule(unaryConstant.path) with UnaryDiagramOperatorComputationRule with FunctorialLinearDiagramOperatorMixin[T] {
}