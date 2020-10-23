package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{ComplexStep, GlobalName, LocalName, MPath, SimpleStep}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.MitM
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.datastructures.{FactReference, ScrollReference}
import info.kwarc.mmt.frameit.business.InvalidFactConstant
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.{Sigma, Tuple}
import io.circe.generic.extras.ConfiguredJsonCodec

object DataStructures {
  // vvvvvvv DO NOT REMOVE IMPORTS (even if IntelliJ marks it as unused)
  import Codecs.PathCodecs._
  import Codecs.SOMDocCodecs._
  import Codecs.DataStructureCodecs.FactCodecs.config._
  // ^^^^^^^ END: DO NOT REMOVE

  /**
    * Facts as sent to and received from the game engine.
    *
    * See subclasses for which fact types exist.
    */
  /*@ConfiguredJsonCodec*/
  sealed abstract class SFact(val ref: Option[FactReference], val label: String) {

    /**
      * The type component that is used by [[toFinalConstant()]] to create the constant.
      */
    protected def getMMTTypeComponent: Term

    /**
      * The definiens component that is used by [[toFinalConstant()]] to create the constant -- if
      * some is given here (i.e. not None).
      */
    protected def getMMTDefComponent: Option[Term]

    /**
      * Transform the fact into an MMT representation, namely a [[FinalConstant]].
      * @param home The home theory (as a term) for the final constant.
      */
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

      factConstant.metadata.add(MetaDatum(FrameWorld.MetaKeys.label, MitM.Foundation.StringLiterals(label)))

      factConstant
    }
  }

  /**
    * Mixin for known facts
    *
    * e.g. all facts returned by the server have type ''[[SFact]] with [[SKnownFact]]'', while the facts
    * received by the game engine generally only have type [[SFact]].
    */
  trait SKnownFact {
    def ref: FactReference
  }

  /**
    * Represents facts of the form ''fact: tp ❘ = df'' where df is optional.
    * That is, it represents the most general form of facts.
    *
    * Overall, facts sent by the game engine or parsed from existing MMT formalizations
    * should only become [[SGeneralFact]]s if other fact types don't match (most
    * importantly [[SValueEqFact]]).
    */
  /*@ConfiguredJsonCodec*/
  sealed case class SGeneralFact(
                                  override val ref: Option[FactReference],
                                  override val label: String,
                                  tp: Term,
                                  df: Option[Term]
                                ) extends SFact(ref, label) {
    override protected def getMMTTypeComponent: Term = tp

    override protected def getMMTDefComponent: Option[Term] = df
  }

  /**
    * Represents facts of the form
    *
    * - ''fact: Σ x: valueTp. ⊦ lhs ≐ x'' and
    * - ''fact: Σ x: valueTp. ⊦ lhs ≐ x❘ = ⟨value, proof⟩''.
    *
    * If no valueTp is given, it is tried to infer it from value -- if that is given.
    * If inference fails (so far only works for real literals as values) or no value is given,
    * an exception upon construction of the case class object is immediately raised.
    *
    * If no value is given, the definiens is left out.
    * If a value is given, but not a proof, then a ''sketch "as sent by game engine"'' proof is implicitly
    * used as the proof.
    * If no value is given, but a proof is, an exception is raised immediately upon construction of the case
    * class object.
    */
  /*@ConfiguredJsonCodec*/
  sealed case class SValueEqFact(
                                  override val ref: Option[FactReference],
                                  override val label: String,
                                  lhs: Term,
                                  valueTp: Option[Term],
                                  value: Option[Term],
                                  proof: Option[Term]
                                ) extends SFact(ref, label) {

    if (value.isEmpty && proof.nonEmpty) {
      throw InvalidFactConstant("SvalueEqFacts cannot have a proof, but no value. That doesn't make sense.")
    }

    private val inferredValueType = valueTp.getOrElse(value match {
      case Some(MitM.Foundation.RealLiterals(_)) => OMID(MitM.Foundation.Math.real)
      case Some(v) =>
        throw InvalidFactConstant(s"SValueEqFact with value type that is not inferrable from value `${v}`")
      case None =>
        throw InvalidFactConstant("SValueEqFact without value and without value type: hence, value type cannot be inferred, but is required.")
    })

    override protected def getMMTTypeComponent: Term = {
      val sigmaVariableName = LocalName("x")

      Sigma(
        sigmaVariableName,
        inferredValueType,
        body = ApplySpine(
          OMID(MitM.Foundation.ded),
          ApplySpine(
            OMS(MitM.Foundation.eq),
            inferredValueType,
            lhs,
            OMV(sigmaVariableName)
          )
        )
      )
    }

    override protected def getMMTDefComponent: Option[Term] = value.map(v =>
      // we only have a definiens if we have a value
      Tuple(v, ApplySpine(
        OMS(MitM.Foundation.sketchOperator),
        ApplySpine(OMS(MitM.Foundation.eq), inferredValueType, lhs, v),
        StringLiterals("as sent by game engine")
      ))
    )
  }

  /* Scrolls */
  sealed case class SScroll(
                            ref: ScrollReference,
                            label: String,
                            description: String,
                            requiredFacts: List[SFact]
                          )

  /**
    * Tentative scroll applications communicated from the game engine to MMT
    */
  sealed case class SScrollApplication(scroll: ScrollReference, assignments: List[(FactReference, Term)]) {
    def toView(target: MPath, codomain: Term)(implicit ctrl: Controller): View = {
      val domain = scroll.problemTheory

      val view = new View(
        doc = target.doc,
        name = target.name,
        fromC = TermContainer.asParsed(OMMOD(domain)),
        toC = TermContainer.asParsed(codomain),
        dfC = TermContainer.empty(),
        isImplicit = false
      )

      ctrl.add(view)

      // collect all assignments such that if typechecking later fails, we can conveniently output
      // debug information
      val scrollViewAssignments = assignments.map {
        case (factRef, assignedTerm) =>
          // create new assignment
          new FinalConstant(
            home = view.toTerm,
            name = LocalName(ComplexStep(factRef.uri.module) :: factRef.uri.name),
            alias = Nil,
            tpC = TermContainer.empty(),
            dfC = TermContainer.asParsed(assignedTerm),
            rl = None,
            notC = NotationContainer.empty(),
            vs = Visibility.public,
          )
      }
      scrollViewAssignments.foreach(ctrl.add(_))

      view
    }
  }
}
