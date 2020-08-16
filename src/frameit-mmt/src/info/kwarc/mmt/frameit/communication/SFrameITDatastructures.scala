package info.kwarc.mmt.frameit.communication

import info.kwarc.mmt.api
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.symbols.{Declaration, FinalConstant, TermContainer, Visibility}
import info.kwarc.mmt.api.{LocalName, MPath, SimpleStep}
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.utils.mmt
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.MitM
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.FactReference
import info.kwarc.mmt.lf.ApplySpine

/**
  * Facts received by the game engine
  */
sealed abstract class SIncomingFact(val label: String) {
  protected def getMMTTypeComponent: Option[Term]
  protected def getMMTDefComponent: Option[Term]
  def toFinalConstant(home: api.objects.Term): FinalConstant = {
    val factConstant = new FinalConstant(
      home = home,
      name = LocalName(SimpleStep(label)),
      alias = Nil,
      tpC = TermContainer.asParsed(getMMTTypeComponent),
      dfC = TermContainer.asParsed(getMMTDefComponent),
      rl = None,
      notC = new NotationContainer,
      vs = Visibility.public
    )

    factConstant.metadata.add(MetaDatum(FrameWorld.MetaKeys.factLabel, MitM.Foundation.StringLiterals(label)))

    factConstant
  }
}

sealed case class SIncomingGeneralFact(override val label: String, tp: Term, df: Option[Term]) extends SIncomingFact(label) {
  override protected def getMMTTypeComponent: Option[Term] = Some(tp)
  override protected def getMMTDefComponent: Option[Term] = df
}

sealed case class SIncomingEqFact(override val label: String, lhs: Term, rhs: Term) extends SIncomingFact(label) {
  override protected def getMMTTypeComponent: Option[Term] = None // should be inferred

  override protected def getMMTDefComponent: Option[Term] = {
    // the [[Term]] "|- lhs ≐ rhs"
    val equalityProposition: Term = ApplySpine(
      OMS(MitM.Foundation.ded),
      ApplySpine(
        OMS(MitM.Foundation.eq),
        // type
        ???,
        lhs,
        rhs
      )
    )

    Some(ApplySpine(
      OMS(MitM.Foundation.sketchOperator),
      equalityProposition,
      StringLiterals("as sent by Unity")
    ))
  }
}

sealed case class SScrollReference(problemTheory: MPath, solutionTheory: MPath)

/**
  * Tentative scroll applications communicated from the game engine to MMT
  */
sealed case class SScrollApplication(scroll: SScrollReference, assignments: List[(FactReference, Term)])
