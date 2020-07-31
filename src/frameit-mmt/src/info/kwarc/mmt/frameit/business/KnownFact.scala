package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.objects.{Context, Obj, Term}
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant, PlainInclude}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaKeys
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals

// TODO choose better name
sealed case class TermPair(original: Term, simplified: Term)

sealed case class FactReference(uri: GlobalName)

// TODO choose better name: known in the sense of known to MMT, e.g. problem theories of scrolls also consist of known facts (that are unknown in the sense of game play, of course)
sealed case class KnownFact(uri: FactReference, label: String, tp: TermPair, df: Option[TermPair])

object KnownFact {
  def apply(uri: GlobalName, label: String, tp: TermPair, df: Option[TermPair]): KnownFact =
    KnownFact(FactReference(uri), label, tp, df)

  def fromConstant(c: Constant)(implicit ctrl: Controller): KnownFact = {
    val label = c.metadata.get(MetaKeys.factLabel) match {
      // fall back to declaration name as label
      case Nil => c.name.toString
      case MetaDatum(_, StringLiterals(label)) :: Nil => label
      case _ => throw InvalidFactConstant("could not create fact from constant", InvalidMetaData(s"Fact declaration contained an invalid label annotation or multiple label annotations, declaration path was: ${c.path}"))
    }

    def simplify(obj: Obj): obj.ThisType = {
      val ctx = Context(c.path.module)
      val simplicationUnit = SimplificationUnit(ctx, expandDefinitions = false, fullRecursion = false)

      ctrl.simplifier.apply(obj, simplicationUnit)
    }

    val tp = c.tp
      .map(tp => TermPair(tp, simplify(tp)))
      .getOrElse(throw InvalidFactConstant("could not create fact from constant as constant has no type component"))
    val df = c.df.map(df => TermPair(df, simplify(df)))

    KnownFact(c.path, label, tp, df)
  }

  def collectFromTheory(theory: Theory, recurseOnInclusions: Boolean)(implicit ctrl: Controller): List[KnownFact] = theory.getDeclarations.collect {
        case c: Constant => List(fromConstant(c))
        case PlainInclude(from, to) if recurseOnInclusions && to == theory.path => collectFromTheory(ctrl.getTheory(from), recurseOnInclusions)
  }.flatten
}
