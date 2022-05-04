package info.kwarc.mmt.frameit.business.datastructures

import java.util.concurrent.ConcurrentHashMap
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.{GlobalName, RuleSet}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.archives.LabelVerbalizationRule
import info.kwarc.mmt.frameit.business.InvalidFactConstant
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{SEquationSystemFact, SFact, SGeneralFact, SValueEqFact}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.{LFList, ListType, Sigma, Tuple}


/**
  * A reference to an already registered fact only -- without accompanying data.
  * @param uri The URI for the fact.
  */
sealed case class FactReference(uri: GlobalName)

/**
  * A general immutable representation of FrameIT facts known to MMT -- from the POV of MMT.
  *
  * A fact is more or less an MMT [[Constant]] with a type [[tp]], an optional definiens [[df]],
  * and some meta data terms encapsulated in [[meta]].
  *
  * By contrast, [[SFact]] is a "dumbed down" representation tailored for passing on to and receiving from the game
  * engine.
  *
  * [[Fact]] objects usually stem from being mentioned in a scroll (in an existing formalization), from
  * the situation theory, or from being received from the game engine.
  *
  *  - If inside a scroll, fact objects are automatically created and stored in a [[Scroll]] object when one of the many
  * helper methods, such as [[Scroll.fromReference()]] and [[Scroll.findAll()]], is called.
  * Internally, these methods call ''Fact.fromConstant()''.
  *  - If inside the situation theory, [[Fact]] objects are collected via ''Fact.findAllIn''.
  *  - If sent from the game engine, the receiving endpoint in [[info.kwarc.mmt.frameit.communication.server]] parses
  *    the sent fact first as an [[SFact]], then persists it via [[SFact.toFinalConstant()]] in the situation theory,
  *    after which the previous item applies.
  *
  * @param ref The reference to the fact
  * @param meta Meta data such as label and description associated to this fact in forms of [[Term]]s.
  *             These only get verbalized upon calling [[toSimple]].
  */
sealed case class Fact(
                        ref: FactReference,
                        meta: UserMetadata,
                        tp: Term,
                        df: Option[Term]
                      ) {

  /**
    * Renders to an [[SFact]] for passing on to the game engine.
    *
    * The result is cached in a field on the [[Fact]] companion object.
    *
    * @param ctrl A controller to perform simplifications.
    */
  def toSimple(implicit ctrl: Controller): SFact = {
    Fact.sfactCache.computeIfAbsent(this, (_: Fact) => _toSimple)
  }

  /**
    * Renders to an [[SFact]] for passing on to the game engine -- without cache.
    *
    * Should only be used [[toSimple]] upon a cache miss.
    */
  private def _toSimple(implicit ctrl: Controller): SFact = {
    val simplify: Term => Term = {
      val simplificationRules: RuleSet = {
        val rules = RuleSet.collectRules(ctrl, Context(ref.uri.module))
        rules.add(new LabelVerbalizationRule()(ctrl.globalLookup))

        rules
      }

      val ctx = Context(ref.uri.module)
      val simplicationUnit = SimplificationUnit(ctx, expandDefinitions = true, fullRecursion = true)

      ctrl.simplifier(_, simplicationUnit, simplificationRules)
    }

    val simpleTp = simplify(tp)
    val simpleDf = df.map(simplify)

    val label = simplify(meta.label).toStr(shortURIs = true)

    Fact.tryRenderSValueEqFact(ref, label, tp = tp, simpleTp = simpleTp, simpleDf = simpleDf) match {
      case Some(valueEqFact) => valueEqFact
      case _ => Fact.tryRenderSEquationSystemFact(ref, label, tp = tp, simpleTp = simpleTp, df = df, simpleDf = simpleDf) match {
          case Some(equationSystemFact) => equationSystemFact
          case _ => SGeneralFact(Some(ref), label, tp, df)
      }
    }
  }
}

object Fact {
  /**
    * A cache to speed up [[Fact.toSimple]].
    */
  private val sfactCache: ConcurrentHashMap[Fact, SFact] = new ConcurrentHashMap

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

  private def tryRenderSEquationSystemFact(ref: FactReference, label: String, tp: Term, simpleTp: Term, df: Option[Term], simpleDf: Option[Term]): Option[SEquationSystemFact] = {
    m[Term, SEquationSystemFact](tp, simpleTp, {
      case ListType(
      tp1
      ) if tp1 == OMS(FrameWorld.prop) =>

        var equations = List[(Term,Term)]()
        simpleDf match {
          case Some(LFList(eqs)) => eqs match {
            case eqs:List[Term] => eqs.foreach{
                case ApplySpine(OMS(FrameWorld.eq), List(_, lhs, rhs)) => equations = (lhs,rhs) :: equations
                case _ => None
            }
            case _ => None
          }
          case _ => None
        }

        Some(SEquationSystemFact(Some(ref), label, tp, df, equations = List.from(equations)))

      case _ => None
    })
  }
}