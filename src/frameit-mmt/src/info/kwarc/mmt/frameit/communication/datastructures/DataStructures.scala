package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.api
import info.kwarc.mmt.api.LocalName.toList
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{Module, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{ComplexStep, GlobalName, LocalName, MPath, SimpleStep}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations.MetaKeys
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.StringLiterals
import info.kwarc.mmt.frameit.archives.MitM
import info.kwarc.mmt.frameit.business.datastructures.{FactReference, Scroll, ScrollReference}
import info.kwarc.mmt.frameit.business.{InvalidFactConstant, Utils}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.{Sigma, Tuple}

object DataStructures {
  // vvvvvvv DO NOT REMOVE IMPORTS (even if IntelliJ marks it as unused)
  import Codecs.PathCodecs._
  import Codecs.SOMDocCodecs._
  import Codecs.DataStructureCodecs.FactCodecs.config._

  import io.circe.generic.extras.ConfiguredJsonCodec
  // ^^^^^^^ END: DO NOT REMOVE

  /**
    * Facts as sent to and received from the game engine.
    *
    * See subclasses for which fact types exist.
    */
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
      * @param target The full path to give to the constant.
      *               It'll live in the module [[target.module]] with name [[target.name]].
      */
    def toFinalConstant(target: GlobalName): FinalConstant = {
      val factConstant = new FinalConstant(
        home = OMMOD(target.module),
        name = target.name,
        alias = Nil,
        tpC = TermContainer.asAnalyzed(getMMTTypeComponent),
        dfC = TermContainer.asAnalyzed(getMMTDefComponent),
        rl = None,
        notC = new NotationContainer,
        vs = Visibility.public
      )

      factConstant.metadata.add(MetaDatum(MetaKeys.label, StringLiterals(label)))

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
    * should only become [[SGeneralFact]]s if other fact types are not suitable (most
    * importantly [[SValueEqFact]]).
    */
  sealed case class SGeneralFact(
                                  override val ref: Option[FactReference],
                                  override val label: String,
                                  tp: Term,
                                  df: Option[Term]
                                ) extends SFact(ref, label) {
    override protected def getMMTTypeComponent: Term = tp

    override protected def getMMTDefComponent: Option[Term] = df
  }

  sealed case class SEquationSystemFact(
                                  override val ref: Option[FactReference],
                                  override val label: String,
                                  tp: Term,
                                  df: Option[Term],
                                  equations: List[(Term, Term)]
                                ) extends SFact(ref, label) {
    override protected def getMMTTypeComponent: Term = tp

    override protected def getMMTDefComponent: Option[Term] = df
  }

  /**
    * Represents facts of the form
    *
    *  - ''fact: Σ x: valueTp. ⊦ lhs ≐ x'' and
    *  - ''fact: Σ x: valueTp. ⊦ lhs ≐ x❘ = ⟨value, proof⟩''.
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
      case Some(FrameWorld.RealLiterals(_)) => OMS(FrameWorld.real)
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
          OMID(FrameWorld.ded),
          ApplySpine(
            OMS(FrameWorld.eq),
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
        OMS(FrameWorld.sketchOperator),
        ApplySpine(OMS(FrameWorld.eq), inferredValueType, lhs, v),
        StringLiterals("as sent by game engine")
      ))
    )
  }

  /* Scrolls */
  sealed case class SScroll(
                            ref: ScrollReference,
                            label: String,
                            description: String,
                            requiredFacts: List[SFact],
                            acquiredFacts: List[SFact]
                          )

  sealed case class SScrollAssignments(assignments: List[(FactReference, Term)]) {
    def toMMTList: List[(GlobalName, Term)] = assignments.map(asgn => (asgn._1.uri, asgn._2))
    def toMMTMap: Map[GlobalName, Term] = assignments.map(asgn => (asgn._1.uri, asgn._2)).toMap
  }

  object SScrollAssignments {
    def fromMMTList(mmtList: List[(GlobalName, Term)]): SScrollAssignments =
      SScrollAssignments(mmtList.map(asgn => (FactReference(asgn._1), asgn._2)))
  }

  /**
    * Tentative scroll applications communicated from the game engine to MMT
    */
  sealed case class SScrollApplication(scroll: ScrollReference, assignments: SScrollAssignments) {
    def toView(target: MPath, codomain: Term)(implicit ctrl: Controller): View = {
      val fullScrollRef = Scroll.fromReference(scroll).get
      val domain = fullScrollRef.ref.problemTheory

      val view = new View(
        doc = target.doc,
        name = target.name,
        fromC = TermContainer.asParsed(OMMOD(domain)),
        toC = TermContainer.asParsed(codomain),
        dfC = TermContainer.empty(),
        isImplicit = false
      )

      Utils.addModule(view)

      // collect all assignments such that if typechecking later fails, we can conveniently output
      // debug information
      val scrollViewAssignments = assignments.assignments.map {
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

      Utils.endAddModule(view)

      view
    }
  }

  sealed abstract class SCheckingError(val msg: String)
  sealed case class SInvalidScrollAssignment(override val msg: String, fact: FactReference) extends SCheckingError(msg)
  sealed case class SNonTotalScrollApplication(override val msg : String = "Scroll application not total") extends SCheckingError(msg)
  sealed case class SMiscellaneousError(override val msg: String) extends SCheckingError(msg)

  sealed case class SScrollApplicationResult(
                                          valid: Boolean,
                                          errors: List[SCheckingError],
                                          acquiredFacts: List[SFact]
                                          )
  object SScrollApplicationResult {
    def success(acquiredFacts: List[SFact]): SScrollApplicationResult =
      SScrollApplicationResult(valid = true, Nil, acquiredFacts)

    def failure(errors: List[SCheckingError]): SScrollApplicationResult =
      SScrollApplicationResult(valid = false, errors, Nil)
  }

  sealed case class SDynamicScrollInfo(
                                        original: SScroll,
                                        rendered: SScroll,

                                        valid: Boolean,
                                        errors: List[SCheckingError],

                                        completions: List[SScrollAssignments]
                                      )
}
