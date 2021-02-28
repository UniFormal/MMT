package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt._
import api._
import info.kwarc.mmt.api.symbols.{Constant, OMSReplacer}
import info.kwarc.mmt.mizar.newxml.mmtwrapper.MizarPrimitiveConcepts._
import info.kwarc.mmt.mizar.newxml.translator.TranslatorUtils.mMLIdtoGlobalName
import info.kwarc.mmt.mizar.newxml.translator.claimTranslator.translate_Claim
import info.kwarc.mmt.mizar.newxml.translator.definitionTranslator.translate_Definition
import objects.{Context, OMA, OMATTR, OMBINDC, OMFOREIGN, OMID, OML, OMLIT, OMLITTrait, OMS, OMSemiFormal, OMV, Term, UnknownOMLIT}
import mizar.newxml.syntax._

object justificationTranslator {
  def translate_Justification(just:Justification, claim: Term)(implicit defContext: DefinitionContext): Option[objects.Term] = just match {
    case Straightforward_Justification(pos, _refs) => None
    case Block(kind, pos, _items) =>
      val currentThesis = if (defContext.withinProof) Some(defContext.getThesis) else None
      defContext.setThesis(claim)
      val usedFacts: List[Term] = usedInJustification(just)
      currentThesis foreach { defContext.setThesis(_) }
      //TODO: actually translate the proofs, may need additional arguments from the context, for instance the claim to be proven
      Some(uses(claim, usedFacts))
    case Scheme_Justification(pos, nr, idnr, schnr, spelling, _refs) => ???
  }
  def translate_Iterative_Equality_Proof(it: Iterative_Equality)(implicit defContext: DefinitionContext): objects.Term = {
    val claim = translate_Claim(it)
    val currentThesis = if (defContext.withinProof) Some(defContext.getThesis) else None
    defContext.setThesis(claim)
    val usedFacts: List[Term] = it._just::it._iterSteps.map(_._just) flatMap(usedInJustification)
    currentThesis foreach { defContext.setThesis(_) }
    //TODO: actually translate the proofs, may need additional arguments from the context, for instance the claim to be proven
    uses(claim, usedFacts)
  }
  def translate_Proved_Claim(provedClaim: ProvedClaim)(implicit defContext: => DefinitionContext): (Term, Option[Term]) = {
    val claim = provedClaim._claim match {
      case Diffuse_Statement(spelling, serialnr, labelnr, _label) => provedClaim._just.get match {
        case Block(kind, pos, _items) =>
          val claims = _items.map(_._subitem match { case c: Claim => (true, Some(c)) case _ => (false, None) }).filter(_._1).map(_._2.get)
          And(claims.map(translate_Claim(_)(defContext)))
        case _ => trueCon
      }
      case _ => translate_Claim(provedClaim._claim)(defContext)
    }
    val prf = (provedClaim._claim, provedClaim._just) match {
      case (_, Some(just)) => translate_Justification(just, claim)(defContext)
      case (it: Iterative_Equality, None) => Some(translate_Iterative_Equality_Proof(it)(defContext))
      case (_, None) => throw ProvedClaimTranslationError("No proof given for claim, which is not an iterative-equality (proving itself). ", provedClaim)
    }
    (claim, prf)
  }
  def usedInJustification(just: Justification)(implicit defContext: DefinitionContext): List[Term] = just match {
    case Straightforward_Justification(pos, _refs) => Nil
    case Block(kind, pos, _items) =>
      def translateSubitems(subs: List[Subitem]): List[Term] = subs match {
        case Nil => Nil
        case it :: tail => it match {
          case st: Statement =>
            val j = st.prfClaim._just
            val trIt = st.prfClaim._claim match {
              case ds: Diffuse_Statement => j map usedInJustification getOrElse Nil
              case Proposition(_, _, Thesis(_, _)) => j map usedInJustification getOrElse Nil
              case it: Iterative_Equality if (j == None) =>
                val And(clms) = translate_Claim(it)
                val justs = it._just :: it._iterSteps.map(_._just)
                clms ::: justs.flatMap(usedInJustification)
              case claim =>
                translate_Claim(claim) :: (j map usedInJustification getOrElse Nil)
            }
            trIt ::: translateSubitems(tail)
          case ex: Exemplification => translateSubitems(tail) //TODO: translate
          case Assumption(ass) => translateSubitems(tail) //Since they already need to be known
          case red: Reduction => translateSubitems(tail) //TODO: translate this to something
          case id: Identify => translateSubitems(tail)
          case gen: Generalization => translateSubitems(tail)
          case prDef: PrivateDefinition =>
            val List(trDef: Constant) = translate_Definition(prDef)
            val tr = OMSReplacer({ case gn if (gn == trDef.path) => trDef.df case _ => None })
            translateSubitems(tail) map (tr.toTranslator()(Context.empty, _))
          case _ => Nil
        }
      }
      translateSubitems(_items map (_._subitem))
    case Scheme_Justification(position, nr, idnr, schnr, spelling, _refs) => _refs.map(r => OMS(r.referencedLabel()))
  }
}