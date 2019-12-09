package info.kwarc.mmt.moduleexpressions.diagdefinition

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.moduleexpressions.operators.Common

object DiagramDefinition {
  val feature = "diagram"
}

/**
  * Structural Feature for Diagram Operators
  *
  * @example
  * '''
  * theory Empty = ❚
  * diagram Collection := ?Empty EXTEND { coll : type ⟶ type } ❚
  * '''
  *
  * It expects an [[AnonymousDiagramCombinator]] as its (normalized) definiens and then
  * lifts the produced [[AnonymousDiagram]] to the usual named space of MMT things.
  * For each node and arrow in the [[AnonymousDiagram]], it is checked whether the [[LocalName]]
  * of that diagram element is an [[Common.ExistingName]]. If not, it is added to the "outer namespace"
  * (i.e. the DPath of the document containing that `diagram` declaration) with the same local name.
  */
class DiagramDefinition extends ModuleLevelFeature(DiagramDefinition.feature) {
  override def getHeaderNotation: List[Marker] = Nil

  /** */
  def check(dm: DerivedModule)(implicit env: ExtendedCheckingEnvironment) {}

  override def modules(dm: DerivedModule): List[Module] = {
    val diag: Term = dm.dfC.normalized.getOrElse {
      throw LocalError("no definiens found for " + dm.path)
    }
    val ad = diag match {
      case AnonymousDiagramCombinator(ad) => ad
      case df =>

        // TODO should use proper error handler
        log("The derived module had meta theory: " + dm.meta)
        val rules = RuleSet.collectRules(controller, Context(dm.meta.get)).get(classOf[ComputationRule]).mkString(", ")
        log("The used rules were " + rules)
        throw LocalError("definiens did not normalize into a flat diagram: " + controller.presenter.asString(df))
    }

    /* defines the name of the generated modules */
    def labelToName(l: LocalName) = LocalName(dm.name.toPath + "_" + l.toPath)

    def labelToPath(l: LocalName) = l match {
      case Common.ExistingName(p) => p
      case _ => dm.parent ? labelToName(l)
    }

    var oldNew: List[(LocalName, LocalName)] = Nil
    val modules = ad.getElements.mapOrSkip { e =>
      e.label match {
        case Common.ExistingName(_) => throw SkipThis
        case _ =>
      }
      val path = labelToPath(e.label)
      val name = path.name
      oldNew ::= (e.label, LocalName(path))
      log("diagram definition generates " + e.toStr(true))
      e match {
        case node: DiagramNode =>
          val anonThy = node.theory
          val df = TermContainer(anonThy.toTerm)
          val thy = Theory(dm.parent, name, anonThy.mt, df = df)
          thy
        case arrow: DiagramArrow =>
          val anonMorph = arrow.morphism
          val df = anonMorph.toTerm
          val vw = View(dm.parent, name, OMMOD(labelToPath(arrow.from)), OMMOD(labelToPath(arrow.to)), Some(df), arrow.isImplicit)
          vw
      }
    }
    val adP = ad.relabel(l => utils.listmap(oldNew, l).getOrElse(l))
    dm.dfC.normalized = Some(adP.toTerm)
    modules
  }
}