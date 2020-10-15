package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{GeneralError, GlobalName, LocalName, MPath, SimpleStep}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaKeys
import info.kwarc.mmt.frameit.archives.MitM
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.{InvalidFactConstant, InvalidMetaData}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.{Sigma, Tuple}
import io.circe.generic.extras.ConfiguredJsonCodec

object DataStructures {
  // vvvvvvv DO NOT REMOVE IMPORTS (even if IntelliJ marks it as unused)
  import Codecs.PathCodecs._
  import Codecs.TermCodecs._
  import Codecs.FactCodecs._
  // ^^^^^^^ END: DO NOT REMOVE

  /**
    * Facts as sent to and received from the game engine.
    *
    * See subclasses for which fact types exist.
    */
  @ConfiguredJsonCodec
  sealed abstract class SFact(val label: String) {

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
    * A reference to an already registered fact only -- without accompanying data.
    * @param uri The URI for the fact.
    */
  sealed case class FactReference(uri: GlobalName)

  /**
    * Mixin for known facts
    *
    * e.g. all facts returned by the server have type ''[[SFact]] with [[KnownFact]]'', while the facts
    * received by the game engine generally only have type [[SFact]].
    */
  trait KnownFact {
    def ref: FactReference
  }

  /**
    * Some helper methods to be used by [[SFact]] (subclasses) only.
    */
  private object SFactHelpers {
    final def parseLabelFromConstant(c: Constant): String = {
      c.metadata.get(MetaKeys.label) match {
        // fall back to declaration name as label
        case Nil => c.name.toString
        case MetaDatum(_, StringLiterals(label)) :: Nil => label
        case _ => throw InvalidFactConstant("could not create fact from constant", InvalidMetaData(s"Fact declaration contained an invalid label annotation or multiple label annotations, declaration path was: ${c.path}"))
      }
    }

    final def getSimplifiedTypeAndDefFromConstant(c: Constant)(implicit ctrl: Controller): (Term, Option[Term]) = {
      def simplify(obj: Obj): obj.ThisType = {
        val ctx = Context(c.path.module)
        val simplicationUnit = SimplificationUnit(ctx, expandDefinitions = true, fullRecursion = true)

        try {
          ctrl.simplifier.apply(obj, simplicationUnit)
        } catch {
          case e: GeneralError =>
            e.printStackTrace(System.err)
            e.getAllCausedBy.take(3).foreach(_.printStackTrace(System.err))

            obj // just return unsimplified
        }
      }

      // todo: currently, only the simplified things are returned
      //       do we also need the non-simplified ones?
      val simplifiedTp: Term = c.tp.map(simplify(_)).getOrElse(
        throw InvalidFactConstant(s"failed parsing fact of ${c.path}: has no type component")
      )

      val simplifiedDf: Option[Term] = c.df.map(simplify(_))

      (simplifiedTp, simplifiedDf)
    }
  }

  object SFact {
    /**
      * Parses a [[Constant]] into an [[SValueEqFact]] (preferred) or [[SGeneralFact]] (otherwise).
      *
      * Performs simplification first.
      */
    final def fromConstant(c: Constant)(implicit ctrl: Controller): SFact with KnownFact = {
      val label = SFactHelpers.parseLabelFromConstant(c)
      val (tp, df) = SFactHelpers.getSimplifiedTypeAndDefFromConstant(c)

      val valueEqFact = SValueEqFact.tryParseFrom(c.path, label, tp, df)
      valueEqFact.getOrElse(SGeneralFact.fromConstant(c.path, label, tp, df))
    }

    /**
      * Collects all [[SFact facts]] from a given [[Theory theory]].
      */
    def collectFromTheory(theory: Theory, recurseOnInclusions: Boolean)(implicit ctrl: Controller): List[SFact with KnownFact] = {
      theory.getDeclarations.collect {

        // todo: use a better way to get all constants (transitively), the method below produces duplicates in the result list for diamong inclusions
        case c: Constant => List(fromConstant(c))
        case PlainInclude(from, to) if recurseOnInclusions && to == theory.path => collectFromTheory(ctrl.getTheory(from), recurseOnInclusions)
      }.flatten
    }
  }

  /**
    * Represents facts of the form ''fact: tp ❘ = df'' where df is optional.
    * That is, it represents the most general form of facts.
    *
    * Overall, facts sent by the game engine or parsed from existing MMT formalizations
    * should only become [[GeneralFact]]s if other fact types don't match (most
    * importantly [[SValueEqFact]]).
    */
  @ConfiguredJsonCodec
  sealed case class SGeneralFact(override val label: String, tp: Term, df: Option[Term]) extends SFact(label) {
    override protected def getMMTTypeComponent: Term = tp

    override protected def getMMTDefComponent: Option[Term] = df
  }

  private object SGeneralFact {
    def fromConstant(path: GlobalName, label: String, tp: Term, df: Option[Term]): SGeneralFact with KnownFact = {
      new SGeneralFact(label, tp, df) with KnownFact {
        override def ref: FactReference = FactReference(path)
      }
    }
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
  @ConfiguredJsonCodec
  sealed case class SValueEqFact(
                                  override val label: String,
                                  lhs: Term,
                                  valueTp: Option[Term],
                                  value: Option[Term],
                                  proof: Option[Term]
                                ) extends SFact(label) {

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

  private object SValueEqFact {
    def tryParseFrom(path: GlobalName, label: String, simpleTp: Term, simpleDf: Option[Term]): Option[SValueEqFact with KnownFact] = {
      simpleTp match {
        case Sigma(
        x1,
        tp1,
        ApplySpine(OMID(MitM.Foundation.ded), List(ApplySpine(OMID(MitM.Foundation.eq), List(tp2, lhs, OMV(x2)))))
        ) if x1 == x2 && tp1 == tp2 =>

          val (value, proof) = simpleDf match {
            case Some(Tuple(v, pf)) => (Some(v), Some(pf))
            case Some(_) =>
              throw InvalidFactConstant("cannot read value and proof of definiens to parse into SValueEqFact")
            case _ => (None, None)
          }

          Some(new SValueEqFact(label, lhs, valueTp = Some(tp1), value, proof) with KnownFact {
            override def ref: FactReference = FactReference(path)
          })

        case _ => None
      }
    }
  }

  /**
    * A reference to a scroll -- without accompanying information.
    */
  sealed case class SScrollReference(problemTheory: MPath, solutionTheory: MPath)

  /**
    * Tentative scroll applications communicated from the game engine to MMT
    */
  sealed case class SScrollApplication(scroll: SScrollReference, assignments: List[(FactReference, Term)])
}
