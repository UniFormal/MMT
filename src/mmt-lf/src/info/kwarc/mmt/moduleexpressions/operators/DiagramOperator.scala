/**
  * An abstract class hierarchy for (functorial) ((sub)linear) (unary) {theory|morphism|diagram} operators abstracting
  * over many common things.
  *
  * E.g. if your envisioned theory operator only operates declaration-by-declaration, subclass from
  * [[info.kwarc.mmt.moduleexpressions.operators.SublinearTheoryOperator]]. If it outputs exactly one declaration
  * for every declaration, subclass from [[info.kwarc.mmt.moduleexpressions.operators.LinearTheoryOperator]].
  * And if that extends in a natural way to morphisms, you can use the [[info.kwarc.mmt.moduleexpressions.operators.MorphismOperatorFromLinearTheoryOperatorMixin]].
  */

package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.{ComplexStep, LocalName}

import scala.collection.mutable

sealed abstract class OperatorResult[T]

case class TransformedResult[T](result: T) extends OperatorResult[T]

case class NotApplicable[T]() extends OperatorResult[T]

// Diagram operators
// ==========================
abstract class DiagramOperator(val unaryConstant: UnaryConstantScala) extends ComputationRule(unaryConstant.path) {
  def transformDiagram(diag: AnonymousDiagram): OperatorResult[AnonymousDiagram]

  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    tm match {
      case unaryConstant(argumentTerm) => Common.asAnonymousDiagram(solver, argumentTerm) match {
        case Some(inputDiagram) => transformDiagram(inputDiagram) match {
          case TransformedResult(outputDiagram) =>
            Simplify(outputDiagram.toTerm)
          case _ => Recurse
        }
        case _ => RecurseOnly(List(1))
      }
      case _ => Recurse
    }
  }
}

// Theory operators
// ==========================

trait UnaryTheoryOperator {
  def transformTheory(thy: AnonymousTheory): OperatorResult[AnonymousTheory]
}

class UnaryTheoryOperatorContext {
  // TODO Florian said: Perhaps easier as List[OMV] until OMV and OML are unified?
  protected var context: Context = Context.empty

  def getContext: Context = {
    this.context
  }

  def setContext(newContext: Context): Unit = {
    this.context = newContext
  }
}

trait SublinearTheoryOperator[HelperContextType <: UnaryTheoryOperatorContext] extends UnaryTheoryOperator {
  protected def initialTheoryHelperContext: HelperContextType

  override final def transformTheory(thy: AnonymousTheory): OperatorResult[AnonymousTheory] = {
    // TODO This really looks like map on an [[Option]] type
    //      Rewrite everything to use [[Option]] instead of [[OperatorResult]]?
    transformTheoryAndGetContext(thy) match {
      case TransformedResult((newThy, _)) => TransformedResult(newThy)
      case _ => NotApplicable()
    }
  }

  final def transformTheoryAndGetContext(thy: AnonymousTheory): OperatorResult[(AnonymousTheory, HelperContextType)] = {
    if (!applicableOnTheory(thy)) {
      return NotApplicable()
    }

    // I planned to use reduceLeft in this method, but due to very strange typechecking issues
    // I could not resolve within an hour (e.g. see https://imgur.com/a/velJzt6), I just switched
    // to an ordinary for loop.

    val declarations = thy.getDeclarations
    val transformedDeclarations = mutable.ListBuffer[OML]()
    var helperContext = initialTheoryHelperContext

    for (currentDecl <- declarations) {
      transformDeclaration(currentDecl, helperContext) match {
        case TransformedResult((newDecls, newHelperContext)) =>
          transformedDeclarations ++= newDecls
          helperContext = newHelperContext
          helperContext.setContext(newHelperContext.getContext ++ currentDecl.vd)

        case _ => /* do nothing, just skip declaration */
      }
    }

    TransformedResult((AnonymousTheory(thy.mt, transformedDeclarations.toList), helperContext))
  }

  def applicableOnTheory(thy: AnonymousTheory): Boolean = {
    true
  }

  def transformDeclaration(decl: OML, ctx: HelperContextType): OperatorResult[(List[OML], HelperContextType)]
}

class LinearTheoryOperatorContext extends UnaryTheoryOperatorContext {
  // TODO Florian said: Put this into more general context class above as Map[LN, List[LN]]
  protected var mapping: Map[LocalName, LocalName] = Map.empty

  def getTranslationMapping: Map[LocalName, LocalName] = {
    this.mapping
  }

  def setTranslationMapping(newMapping: Map[LocalName, LocalName]): Unit = {
    this.mapping = newMapping
  }

  final def addTranslationMapping(oldDeclName: LocalName, newDeclName: LocalName): Unit = {
    setTranslationMapping(mapping + (oldDeclName -> newDeclName))
  }
}

trait LinearTheoryOperator[HelperContextType <: LinearTheoryOperatorContext] extends SublinearTheoryOperator[HelperContextType] {

  override final def transformDeclaration(decl: OML, ctx: HelperContextType): OperatorResult[(List[OML], HelperContextType)] = {
    transformSingleDeclaration(decl, ctx) match {
      case TransformedResult((newDecl, newCtx)) =>
        newCtx.addTranslationMapping(decl.name, newDecl.name)
        TransformedResult(
          (
            List(newDecl),
            newCtx
          )
        )
      case NotApplicable() => NotApplicable()
    }
  }

  def transformSingleDeclaration(decl: OML, ctx: HelperContextType): OperatorResult[(OML, HelperContextType)]
}

// Morphism operators
// ==========================
// TODO: Florian said: for domain pass also its original version and transformed version
trait UnaryMorphismOperator[HelperContextType <: UnaryTheoryOperatorContext] {
  def transformMorphism(
                         morphism: AnonymousMorphism,
                         transformedDomain: AnonymousTheory,
                         domainContext: HelperContextType,
                         transformedCodomain: AnonymousTheory,
                         codomainContext: HelperContextType
                       ): OperatorResult[AnonymousMorphism]
}

trait MorphismOperatorFromLinearTheoryOperatorMixin[HelperContextType <: LinearTheoryOperatorContext] extends UnaryMorphismOperator[HelperContextType] with LinearTheoryOperator[HelperContextType] {

  override def transformMorphism(morphism: AnonymousMorphism, transformedDomain: AnonymousTheory, domainContext: HelperContextType, transformedCodomain: AnonymousTheory, codomainContext: HelperContextType): OperatorResult[AnonymousMorphism] = {
    if (!applicableOnMorphism(morphism)) {
      return NotApplicable()
    }

    // I planned to use reduceLeft in this method, but due to very strange typechecking issues
    // I could not resolve within an hour (e.g. see https://imgur.com/a/velJzt6), I just switched
    // to an ordinary for loop.

    val assignments = morphism.getDeclarations
    val transformedAssignments = mutable.ListBuffer[OML]()

    for (currentAssignment <- assignments) {
      // It might well be the case that the [[LinearTheoryOperator]] just skipped
      // translating a declaration and hence we must skip it below as well
      val newLHSOfAssignment: Option[LocalName] = codomainContext.getTranslationMapping.get(currentAssignment.name)

      // Make lazy such that it only gets evaluated if we have a new LHS as well below
      lazy val newRHSOfAssignment: Option[Term] = currentAssignment.df.flatMap {
        // Old RHS is just a constant declaration of the codomain theory
        case codomainDeclaration: OML =>
          codomainContext.getTranslationMapping.get(codomainDeclaration.name).map(OML(_))

        // Some complex definiens expression
        case _ => transformSingleDeclaration(currentAssignment, codomainContext) match {
          case TransformedResult((OML(_, _, Some(definiens), _, _), _)) =>
            Some(definiens)
          case _ => None
        }
      }

      (newLHSOfAssignment, newRHSOfAssignment) match {
        case (Some(newLHS), Some(newRHS)) =>
          transformedAssignments += currentAssignment.copy(
            name = newLHS,
            df = Some(newRHS),
            tp = None // Reset type because type may have changed due to application of unary theory operator
          )

        case _ => // Skip assignment if we couldn't translate LHS (i.e. theory operator skipped it previously in
        // domain theory) or definiens
      }
    }

    TransformedResult(AnonymousMorphism(transformedAssignments.toList))
  }

  def applicableOnMorphism(morphism: AnonymousMorphism): Boolean = true
}

abstract class FunctorialLinearDiagramOperator[HelperContextType <: LinearTheoryOperatorContext](unaryConstant: UnaryConstantScala) extends DiagramOperator(unaryConstant) with MorphismOperatorFromLinearTheoryOperatorMixin[HelperContextType] {

  override def transformDiagram(diag: AnonymousDiagram): OperatorResult[AnonymousDiagram] = {
    def permuteLabel(label: LocalName): LocalName = label match {
      case _ if diag.distNode.contains(label) => LocalName("pres")
      case LocalName(List(ComplexStep(mPath))) => LocalName(mPath.toString)
      case _ => label
    }

    assert(!diag.nodes.exists(_.label == LocalName("pres")))

    val (newNodes, theoryContexts) = diag.nodes.map(node => transformTheoryAndGetContext(node.theory) match {
      case TransformedResult((newTheory, ctx)) =>
        (DiagramNode(permuteLabel(node.label), newTheory), (node.label, ctx))
      case _ => ???
    }).unzip match {
      case (newNodes, theoryContextsAsListsOfPairs) => (newNodes, theoryContextsAsListsOfPairs.toMap)
    }

    val newArrows = diag.arrows.map(arrow => {
      val transformedDomain = newNodes.find(_.label == permuteLabel(arrow.from)).get.theory
      val transformedCodomain = newNodes.find(_.label == permuteLabel(arrow.to)).get.theory

      transformMorphism(
        arrow.morphism,
        transformedDomain, theoryContexts(arrow.from),
        transformedCodomain, theoryContexts(arrow.to)
      ) match {
        case TransformedResult(newMorphism) => arrow.copy(
          label = permuteLabel(arrow.label),
          morphism = newMorphism,
          from = permuteLabel(arrow.from),
          to = permuteLabel(arrow.to)
        )
        case _ => ???
      }
    })

    TransformedResult(AnonymousDiagram(newNodes, newArrows, diag.distNode.map(permuteLabel)))
  }
}
