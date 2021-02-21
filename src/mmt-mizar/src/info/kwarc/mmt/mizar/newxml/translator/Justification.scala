package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt._
import api._
import info.kwarc.mmt.mizar.newxml.mmtwrapper.Mizar
import info.kwarc.mmt.mizar.newxml.translator.TranslatorUtils.mMLIdtoGlobalName
import info.kwarc.mmt.mizar.newxml.translator.claimTranslator.translate_Claim
import objects.{Context, OMA, OMATTR, OMBINDC, OMFOREIGN, OMID, OML, OMLIT, OMLITTrait, OMS, OMSemiFormal, OMV, Term, UnknownOMLIT}
import mizar.newxml.syntax._

object justificationTranslator {
  def translate_Justification(just:Justification, claim: Term)(implicit defContext: DefinitionContext): Option[objects.Term] = just match {
    case Straightforward_Justification(pos, _refs) => None
    case Block(kind, pos, _items) =>
      val currentThesis = defContext.thesis
      defContext.setThesis(claim)
      val usedFacts: List[Term] = usedInJustification(just)
      defContext.setThesis(currentThesis)
      //TODO: actually translate the proofs, may need additional arguments from the context, for instance the claim to be proven
      Some(Mizar.uses(claim, usedFacts))
    case Scheme_Justification(pos, nr, idnr, schnr, spelling, _refs) => ???
  }
  def translate_Iterative_Equality_Proof(it: Iterative_Equality)(implicit defContext: DefinitionContext): objects.Term = {
    val claim = translate_Claim(it)
    val currentThesis = defContext.thesis
    defContext.setThesis(claim)
    val usedFacts: List[Term] = it._just::it._iterSteps.map(_._just) flatMap(usedInJustification)
    defContext.setThesis(currentThesis)
    //TODO: actually translate the proofs, may need additional arguments from the context, for instance the claim to be proven
    Mizar.uses(claim, usedFacts)
  }
  def translate_Proved_Claim(provedClaim: ProvedClaim)(implicit defContext: DefinitionContext = DefinitionContext.empty()) = {
    val claim = provedClaim._claim match {
      case Diffuse_Statement(spelling, serialnr, labelnr, _label) => provedClaim._just.get match {
        case Block(kind, pos, _items) =>
          val claims = _items.map(_._subitem match { case c: Claim => (true, Some(c)) case _ => (false, None) }).filter(_._1).map(_._2.get)
          Mizar.And(claims.map(translate_Claim(_)))
        case _ => Mizar.trueCon
      }
      case _ => translate_Claim(provedClaim._claim)
    }
    val prf = (provedClaim._claim, provedClaim._just) match {
      case (_, Some(just)) => translate_Justification(just, claim)
      case (it: Iterative_Equality, None) => Some(translate_Iterative_Equality_Proof(it))
      case (_, None) => throw ProvedClaimTranslationError("No proof given for claim, which is not an iterative-equality (proving itself). ", provedClaim)
    }
    (claim, prf)
  }
  def usedInJustification(just: Justification): List[Term] = just match {
    case Straightforward_Justification(pos, _refs) => Nil
    case Block(kind, pos, _items) =>_items.map(_._subitem) flatMap {
      case st: Statement =>
        val j = st.prfClaim._just
        st.prfClaim._claim match {
          case ds: Diffuse_Statement => j map usedInJustification getOrElse Nil
          case Proposition(_, _, Thesis(_, _)) => j map usedInJustification getOrElse Nil
          case it: Iterative_Equality if (j == None) =>
            val Mizar.And(clms) = translate_Claim(it)
            val justs = it._just :: it._iterSteps .map (_._just)
            clms:::justs .flatMap(usedInJustification)
          case claim =>
            translate_Claim(claim):: (j map usedInJustification getOrElse Nil)
        }
      case ex: Exemplification => Nil //TODO: translate
      case Assumption(ass) => Nil //Since they already need to be known
      case red: Reduction => Nil//TODO: translate this to something
      case id : Identify => Nil
      case gen: Generalization => Nil
      case _ => Nil
    }
    case Scheme_Justification(position, nr, idnr, schnr, spelling, _refs) => _refs.map(r => OMS(r.referencedLabel()))
  }
}