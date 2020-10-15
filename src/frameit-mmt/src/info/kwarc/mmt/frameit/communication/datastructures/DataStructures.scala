package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, SimpleStep}
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

      factConstant.metadata.add(MetaDatum(FrameWorld.MetaKeys.label, MitM.Foundation.StringLiterals(label)))

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

    def collectFromTheory(theory: Theory, recurseOnInclusions: Boolean, simplify: Boolean = false)(implicit ctrl: Controller): List[SFact with KnownFact] = theory.getDeclarations.collect {
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
    def fromConstant(path: GlobalName, label: String, tp: Term, df: Option[Term]): SGeneralFact with KnownFact = {
      new SGeneralFact(label, tp, df) with KnownFact {
        override def ref: FactReference = FactReference(path)
      }
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
    def tryParseFrom(path: GlobalName, label: String, simpleTp: Term, simpleDf: Option[Term]): Option[SValueEqFact with KnownFact] = {
      simpleTp match {
        case Sigma(
        x1,
        tp1,
        ApplySpine(OMID(MitM.Foundation.ded), List(ApplySpine(OMID(MitM.Foundation.eq), List(tp2, lhs, OMV(x2)))))
        ) if x1 == x2 && tp1 == tp2 =>

          val suppliedValue = simpleDf match {
            case Some(Tuple(v, _)) => Some(v)
            case _ => None
          }

          Some(new SValueEqFact(label, lhs, tp1, suppliedValue) with KnownFact {
            override def ref: FactReference = FactReference(path)
          })

        case Sigma(_, _, _) =>
          throw InvalidFactConstant(s"failed parsing fact of ${path}: type has too complex sigma type")

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
