package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import info.kwarc.mmt.api.uom.{Recurse, Simplifiability, SimplificationRule, SimplificationUnit, Simplify}
import info.kwarc.mmt.api.{GeneralError, GlobalName, NamespaceMap, Path, RuleSet}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations
import info.kwarc.mmt.frameit.archives.MitM
import info.kwarc.mmt.frameit.business.InvalidFactConstant
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{SFact, SGeneralFact, SValueEqFact}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.{Sigma, Tuple}

/**
  * A reference to an already registered fact only -- without accompanying data.
  * @param uri The URI for the fact.
  */
sealed case class FactReference(uri: GlobalName)

sealed case class Fact(
                        ref: FactReference,
                        meta: UserMetadata,
                        tp: Term,
                        df: Option[Term]
                      ) {

  def toSimple(implicit ctrl: Controller): SFact = {
    val simplify: Term => Term = {
      val simplificationRules: RuleSet = {
        val rules = RuleSet.collectRules(ctrl, Context(ref.uri.module))

        rules.add({
          val realTimes = Path.parseS("http://mathhub.info/MitM/Foundation?RealLiterals?times_real_lit", NamespaceMap.empty)

          new SimplificationRule(realTimes) {
            override def apply(context: Context, t: Term): Simplifiability = t match {
              case ApplySpine(`realTimes`, List(FrameWorld.RealLiterals(x), FrameWorld.RealLiterals(y))) =>
                Simplify(FrameWorld.RealLiterals(x * y))
              case _ =>
                Recurse
            }
          }
        })
        rules
      }

      val ctx = Context(ref.uri.module)
      val simplicationUnit = SimplificationUnit(ctx, expandDefinitions = true, fullRecursion = true)

      ctrl.simplifier(_, simplicationUnit, simplificationRules)
    }

    lazy val simpleTp = simplify(tp)
    lazy val simpleDf = df.map(simplify)

    val label = meta.label.toStr(true) // replace with real rendering

    Fact.tryRenderSValueEqFact(ref, label, tp = tp, simpleTp = simpleTp, simpleDf = simpleDf) match {
      case Some(valueEqFact) => valueEqFact
      case _ => SGeneralFact(Some(ref), label, tp, df)
    }
  }
}

object Fact {
  def fromConstant(c: Constant)(implicit ctrl: Controller): Fact = Fact(
    FactReference(c.path),
    UserMetadata.parse(c),
    c.tp.getOrElse(throw InvalidFactConstant(s"tried parsing fact from constant ${c.path}, but it has no type")),
    c.df
  )

  // in narrative order
  private def collectConstantsFromTheory(theory: Theory, recurseOnInclusions: Boolean)(implicit ctrl: Controller): List[Constant] = theory.getDeclarations.collect {
    case c: Constant => List(c)
    case PlainInclude(from, to) if recurseOnInclusions && to == theory.path =>
      collectConstantsFromTheory(ctrl.getTheory(from), recurseOnInclusions)
  }.flatten.distinct

  /**
    * Collects all [[SFact facts]] from a given [[Theory theory]].
    */
  def findAllIn(theory: Theory, recurseOnInclusions: Boolean)(implicit ctrl: Controller): List[Fact] = collectConstantsFromTheory(theory, recurseOnInclusions).map(fromConstant)

  // todo: document this function: used to spare duplicating match expressions when
  //   first matching against unsimplified term and then (upon failure) against simplified term
  private def m[S, T](s1: S, s2: S, f: S => Option[T]): Option[T] = {
    f(s1) match {
      case Some(t) => Some(t)
      case None => f(s2)
    }
  }

  private def tryRenderSValueEqFact(ref: FactReference, label: String, tp: Term, simpleTp: Term, simpleDf: Option[Term]): Option[SValueEqFact] = {
    m[Term, SValueEqFact](tp, simpleTp, {
      case Sigma(
      x1,
      tp1,
      ApplySpine(OMS(FrameWorld.ded), List(ApplySpine(OMS(FrameWorld.eq), List(tp2, lhs, OMV(x2)))))
      ) if x1 == x2 && tp1 == tp2 =>

        val (value, proof) = simpleDf match {
          case Some(Tuple(v, pf)) => (Some(v), Some(pf))
          case Some(_) =>
            throw InvalidFactConstant("cannot read value and proof of definiens to parse into SValueEqFact")
          case _ => (None, None)
        }

        Some(SValueEqFact(Some(ref), label, lhs, valueTp = Some(tp1), value, proof))

      case _ => None
    })
  }
}