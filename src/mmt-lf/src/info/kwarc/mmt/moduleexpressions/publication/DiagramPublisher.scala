package info.kwarc.mmt.moduleexpressions.publication

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom.SimplificationUnit

import scala.collection.mutable

object DiagramPublisher {
  /**
    * @todo Eventually rename the structural feature for diagrams to "publish diagram"
    */
  val feature = "diagram"
}

/**
  * Module-level structural feature for publishing diagrams into the current document namespace.
  *
  * @example
  * '''
  * theory Empty = ❚
  * diagram Collection := ?Empty EXTEND { coll : type ⟶ type } ❚
  * '''
  *
  * It expects an [[AnonymousDiagramCombinator]] as its (normalized) definiens and then
  * publishes that diagram in the document namespace in which the structural feature itself is used.n
  */
class DiagramPublisher extends ModuleLevelFeature(DiagramPublisher.feature) {
  override def getHeaderNotation: List[Marker] = Nil

  /** */
  def check(dm: DerivedModule)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  override def modules(dm: DerivedModule): List[Module] = dm.dfC.normalized match {
    case None =>
      throw LocalError("no definiens found for " + dm.path)
    case Some(df) =>
      val simplificationUnit = SimplificationUnit(dm.getInnerContext, expandDefinitions = true, fullRecursion = true, solverO = None)

      controller.simplifier(df, simplificationUnit) match {
        case AnonymousDiagramCombinator(anonDiag) =>
          getModulesForAnonymousDiagram(dm, dm.parent, anonDiag)

        case anyOtherSimplifiedDf =>
          // TODO should use proper error handler
          log("The derived module had meta theory: " + dm.meta)
          val rules = RuleSet.collectRules(controller, Context(dm.meta.get)).get(classOf[ComputationRule]).mkString(", ")
          log("The used rules were " + rules)
          throw LocalError("definiens did not normalize into a flat diagram: " + controller.presenter.asString(anyOtherSimplifiedDf))
      }
  }

  private def getModulesForAnonymousDiagram(dm: DerivedModule, outerDocumentPath: DPath, anonDiag: AnonymousDiagram): List[Module] = {
    // Export the diagram elements to document namespace surrounding the derived declaration.
    val newNames: mutable.Map[LocalName, MPath] = mutable.HashMap()

    def labeller(diagElementName: LocalName): MPath = newNames.getOrElseUpdate(diagElementName, {
      val supposedlyNewName = diagElementName.last match {
        case SimpleStep(name) => outerDocumentPath ? name
        case ComplexStep(path) => labeller(path.name)
      }

      controller.getO(supposedlyNewName) match {
        case None =>
          supposedlyNewName
        case Some(_) =>
          throw LocalError(s"DiagramDefinition structural feature: cannot export diagram element with name ${diagElementName} to outer namespace due to name clash with pre-existing module therein")
      }
    })

    val modules = anonDiag.toModules(labeller)
    // TODO: Ask Florian why this is necessary
    dm.dfC.normalized = Some(anonDiag.relabel(labeller(_).name).toTerm)
    //  A semantically equivalent line was previously in his source code here before
    //  I refactored.

    modules
  }
}