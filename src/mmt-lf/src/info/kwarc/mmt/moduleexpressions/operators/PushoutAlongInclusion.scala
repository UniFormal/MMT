package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.{BinaryConstantScala, RecurseOnly, Simplifiability, Simplify}

object PushoutAlongInclusion extends BinaryConstantScala(Combinators._path, "pushout_inclusion")

object ComputePushoutAlongInclusion extends ComputationRule(PushoutAlongInclusion.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val PushoutAlongInclusion(extensionTheoryTerm, morphismTerm) = tm
    val extensionTheoryDiagram = Common.asAnonymousDiagram(solver, extensionTheoryTerm).getOrElse(return RecurseOnly(List(1)))
    val morphismDiagram = Common.asAnonymousDiagram(solver, morphismTerm).getOrElse(return RecurseOnly(List(2)))

    val extensionNode = extensionTheoryDiagram.getDistNode.get
    val morphismArrow = morphismDiagram.arrows.head
    val morphismCodomainNode = morphismDiagram.getNode(morphismDiagram.arrows.head.to).get

    val (morphismIntoPushout, pushoutTheory) = NewPushoutUtils.computeAnonymousPushoutAlongInclusion(
      thyB = morphismCodomainNode.theory,
      thyAToThyB = morphismArrow.morphism,
      thyC = extensionNode.theory
    )

    val pushoutNodeLabel = LocalName("pres")
    val pushoutArrowLabel = LocalName("pres_mor")

    Simplify(AnonymousDiagram(
      nodes = List(
        extensionNode,
        DiagramNode(pushoutNodeLabel, pushoutTheory)
      ),
      arrows = List(
        DiagramArrow(pushoutArrowLabel, extensionNode.label, pushoutNodeLabel, morphismIntoPushout, isImplicit = true)
      ),
      distNode = Some(pushoutNodeLabel)
    ).toTerm)
  }
}