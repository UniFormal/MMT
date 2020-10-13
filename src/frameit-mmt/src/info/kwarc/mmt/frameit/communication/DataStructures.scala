package info.kwarc.mmt.frameit.communication

import info.kwarc.mmt.api
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.symbols.{Declaration, FinalConstant, TermContainer, Visibility}
import info.kwarc.mmt.api.{LocalName, MPath, SimpleStep}
import info.kwarc.mmt.api.objects.{OMID, OMS, OMV, Term}
import info.kwarc.mmt.api.utils.mmt
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.{MMT, MitM}
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.FactReference
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.{Sigma, Tuple}

object DataStructures {

  // IMPORTANT: do NOT run IntelliJ's automatic "import clean-up" utility. It will remove necessary imports in this file.
  import io.circe.generic.extras._
  import TermCodecs._
  import PathCodecs._
  // end IMPORTANT

  implicit val factJsonConfig: Configuration = Configuration.default
    .withDiscriminator("kind")
    .copy(transformConstructorNames = oldCtorName => {
      val rewriteMap = Map(
        classOf[SIncomingGeneralFact] -> "general",
        classOf[SIncomingValueEqFact] -> "veq"
      ).map { case (key, value) => (key.getSimpleName, value) }

      rewriteMap.getOrElse(oldCtorName, oldCtorName)
    })


  /**
    * Facts received by the game engine
    */
  @ConfiguredJsonCodec
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

  @ConfiguredJsonCodec
  sealed case class SIncomingGeneralFact(override val label: String, tp: Term, df: Option[Term]) extends SIncomingFact(label) {
    override protected def getMMTTypeComponent: Option[Term] = Some(tp)

    override protected def getMMTDefComponent: Option[Term] = df
  }

  @ConfiguredJsonCodec
  sealed case class SIncomingValueEqFact(override val label: String, lhs: Term, rhs: Term) extends SIncomingFact(label) {
    private def getSigmaVariableType: Option[Term] = rhs match {
      case MitM.Foundation.RealLiterals(_) => Some(OMID(MitM.Foundation.Math.real))
      case _ => return None
    }

    override protected def getMMTTypeComponent: Option[Term] = getSigmaVariableType.map(tp => {
      val sigmaVariableName = LocalName("x")

      Sigma(
        sigmaVariableName,
        tp,
        body = ApplySpine(
          OMID(MitM.Foundation.ded),
          ApplySpine(
            OMS(MitM.Foundation.eq),
            tp,
            lhs,
            OMV(sigmaVariableName)
          )
        )
      )
    })

    override protected def getMMTDefComponent: Option[Term] = getSigmaVariableType.map(tp =>
      Tuple(
        rhs,
        ApplySpine(
          OMS(MitM.Foundation.sketchOperator),
          ApplySpine(OMS(MitM.Foundation.eq), tp, lhs, rhs),
          StringLiterals("as sent by Unity")
        )
      )
    )
  }

  sealed case class SScrollReference(problemTheory: MPath, solutionTheory: MPath)

  /**
    * Tentative scroll applications communicated from the game engine to MMT
    */
  sealed case class SScrollApplication(scroll: SScrollReference, assignments: List[(FactReference, Term)])

}