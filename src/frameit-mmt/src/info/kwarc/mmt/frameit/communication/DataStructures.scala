package info.kwarc.mmt.frameit.communication

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, PlainInclude, TermContainer, Visibility}
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, SimpleStep}
import info.kwarc.mmt.api.objects.{Context, OMID, OMS, OMV, Obj, Term}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaKeys
import info.kwarc.mmt.frameit.archives.{MMT, MitM}
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.{InvalidFactConstant, InvalidMetaData}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.{Sigma, Tuple}
import io.circe.{Decoder, Encoder, HCursor, Json}

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
        classOf[SGeneralFact] -> "general",
        classOf[SValueEqFact] -> "veq"
      ).map { case (key, value) => (key.getSimpleName, value) }

      rewriteMap.getOrElse(oldCtorName, oldCtorName)
    })


  /**
    * Facts as sent to and received from the game engine
    */
  @ConfiguredJsonCodec
  sealed abstract class SFact(val label: String) {
    protected def getMMTTypeComponent: Term
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

  object Codecs {
    // vvvvvvvv do not remove imports!
    import TermCodecs._
    import PathCodecs._
    import SOMDoc.STermCodecs._

    implicit val factEncoder: Encoder[SFact] = io.circe.generic.semiauto.deriveEncoder[SFact]

    implicit val knownFactEncoder: Encoder[SFact with KnownFact] = (knownFact: SFact with KnownFact) => {
      // just add `uri: ...` field to encoded fact
      Json.fromJsonObject(
        // assumption: facts are encoded as objects
        factEncoder(knownFact).asObject.getOrElse(???).add("uri", globalNameEncoder(knownFact.ref.uri))
      )
    }

    // No knownFactDecoder (not needed yet)
  }

  private object SFactHelpers {
    final def parseLabelFromConstant(c: Constant): String = {
      c.metadata.get(MetaKeys.factLabel) match {
        // fall back to declaration name as label
        case Nil => c.name.toString
        case MetaDatum(_, StringLiterals(label)) :: Nil => label
        case _ => throw InvalidFactConstant("could not create fact from constant", InvalidMetaData(s"Fact declaration contained an invalid label annotation or multiple label annotations, declaration path was: ${c.path}"))
      }
    }

    final def getSimplifiedTypeAndDefFromConstant(c: Constant)(implicit ctrl: Controller): (Term, Option[Term]) = {
      def simplify(obj: Obj): obj.ThisType = {
        val ctx = Context(c.path.module)
        val simplicationUnit = SimplificationUnit(ctx, expandDefinitions = false, fullRecursion = false)

        ctrl.simplifier.apply(obj, simplicationUnit)
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
    final def fromConstant(c: Constant)(implicit ctrl: Controller): SFact with KnownFact = {
      c match {
        // todo: a bit inefficient because the unapplys in both cases do simplification and metadata reads of the constant
        case SValueEqFact(fact) => fact
        case SGeneralFact(fact) => fact // should match almost any constant
      }
    }

    def collectFromTheory(theory: Theory, recurseOnInclusions: Boolean)(implicit ctrl: Controller): List[SFact with KnownFact] = theory.getDeclarations.collect {
      case c: Constant => List(fromConstant(c))
      case PlainInclude(from, to) if recurseOnInclusions && to == theory.path => collectFromTheory(ctrl.getTheory(from), recurseOnInclusions)
    }.flatten
  }

  @ConfiguredJsonCodec
  sealed case class SGeneralFact(override val label: String, tp: Term, df: Option[Term]) extends SFact(label) {
    override protected def getMMTTypeComponent: Term = tp
    override protected def getMMTDefComponent: Option[Term] = df
  }

  object SGeneralFact {
    def unapply(c: Constant)(implicit ctrl: Controller): Option[SGeneralFact with KnownFact] = {
      val label = SFactHelpers.parseLabelFromConstant(c)

      val (tp, df) = SFactHelpers.getSimplifiedTypeAndDefFromConstant(c)

      Some(new SGeneralFact(label, tp, df) with KnownFact {
        override def ref: FactReference = FactReference(c.path)
      })
    }
  }

  @ConfiguredJsonCodec
  sealed case class SValueEqFact(override val label: String, lhs: Term, valueTp: Term, value: Option[Term]) extends SFact(label) {
    override protected def getMMTTypeComponent: Term = {
      val sigmaVariableName = LocalName("x")

      Sigma(
        sigmaVariableName,
        valueTp,
        body = ApplySpine(
          OMID(MitM.Foundation.ded),
          ApplySpine(
            OMS(MitM.Foundation.eq),
            valueTp,
            lhs,
            OMV(sigmaVariableName)
          )
        )
      )
    }

    override protected def getMMTDefComponent: Option[Term] = value.map(v =>
      // we only have a definiens if we have a value
      Tuple(
        v,
        ApplySpine(
          OMS(MitM.Foundation.sketchOperator),
          ApplySpine(OMS(MitM.Foundation.eq), valueTp, lhs, v),
          StringLiterals("as sent by Unity")
        )
      )
    )
  }

  object SValueEqFact {
    def unapply(c: Constant)(implicit ctrl: Controller): Option[SValueEqFact with KnownFact] = {
      val label = SFactHelpers.parseLabelFromConstant(c)
      val (tp, df) = SFactHelpers.getSimplifiedTypeAndDefFromConstant(c)

      tp match {
        case Sigma(
          x1,
          tp1,
          ApplySpine(OMID(MitM.Foundation.ded), List(ApplySpine(OMID(MitM.Foundation.eq), List(tp2, lhs, OMV(x2)))))
        ) if x1 == x2 && tp1 == tp2 =>

          val suppliedValue = df match {
            case Some(Tuple(v, _)) => Some(v)
            case _ => None
          }

          Some(new SValueEqFact(label, lhs, tp1, suppliedValue) with KnownFact {
            override def ref: FactReference = FactReference(c.path)
          })

        case Sigma(_, _, _) =>
            throw InvalidFactConstant(s"failed parsing fact of ${c.path}: type has too complex sigma type")

        case _ => None
      }
    }
  }

  sealed case class SScrollReference(problemTheory: MPath, solutionTheory: MPath)

  /**
    * Tentative scroll applications communicated from the game engine to MMT
    */
  sealed case class SScrollApplication(scroll: SScrollReference, assignments: List[(FactReference, Term)])
}