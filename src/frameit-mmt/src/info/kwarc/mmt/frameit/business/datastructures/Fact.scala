package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{GeneralError, GlobalName}
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

  def renderStatic()(implicit ctrl: Controller): SFact = {
    val (tp, df) = Fact.getSimplifiedTypeAndDefFromConstant(ctrl.getConstant(ref.uri))

    val label = meta.label.toStr(true) // replace with real rendering

    Fact.tryRenderSValueEqFact(ref, label, tp, df) match {
      case Some(valueEqFact) => valueEqFact
      case _ => SGeneralFact(Some(ref), label, tp, df)
    }
  }

  private[datastructures] def renderDynamicFact(viewRenderer: ScrollViewRenderer): Fact = {
    this.copy(
      meta = meta.render(viewRenderer),
      tp = viewRenderer(tp),
      df = df.map(viewRenderer.apply)
    )
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

  private final def getSimplifiedTypeAndDefFromConstant(c: Constant)(implicit ctrl: Controller): (Term, Option[Term]) = {
    def simplify(obj: Obj): obj.ThisType = {
      val ctx = Context(c.path.module)
      val simplicationUnit = SimplificationUnit(ctx, expandDefinitions = true, fullRecursion = true)

      try {
        ctrl.simplifier.apply(obj, simplicationUnit)
      } catch {
        case e: GeneralError =>
          System.err.println("error while simplifying, possibly known MMT bug (UniFormal/MMT#546)")

          // reenable these outputs vvv if the bug above is solved
          /*e.printStackTrace(System.err)
          e.getAllCausedBy.take(3).foreach(_.printStackTrace(System.err))*/

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

  private def tryRenderSValueEqFact(ref: FactReference, label: String, simpleTp: Term, simpleDf: Option[Term]): Option[SValueEqFact] = {
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

        // todo: replace toStr
        Some(SValueEqFact(Some(ref), label, lhs, valueTp = Some(tp1), value, proof))

      case _ => None
    }
  }
}