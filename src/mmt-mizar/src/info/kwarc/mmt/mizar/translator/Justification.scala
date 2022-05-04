package info.kwarc.mmt.mizar.translator

import info.kwarc.mmt._
import api._
import info.kwarc.mmt.api.symbols.{Declaration, HasDefiniens, HasNotation, HasType}
import info.kwarc.mmt.mizar.mmtwrapper.MizarPrimitiveConcepts._
import info.kwarc.mmt.mizar.mmtwrapper.PatternUtils.{PiOrEmpty, lambdaBindArgs}
import info.kwarc.mmt.mizar.translator.JustificationTranslator.lambdaBindDefCtxArgs
import info.kwarc.mmt.mizar.translator.claimTranslator._
import info.kwarc.mmt.mizar.translator.definitionTranslator.translate_Definition
import info.kwarc.mmt.mizar.translator.statementTranslator.translate_Choice_Statement
import info.kwarc.mmt.mizar.translator.termTranslator.translate_Term
import objects._
import mizar.syntax._

object JustificationTranslator {
  /** whether to also translate proof steps or only references to used statements */
  val proofSteps = false
  def translate_Proved_Claim(provedClaim: ProvedClaim)(implicit defContext: => DefinitionContext): (Term, Term) = {
    val claim = provedClaim._claim match {
      case Diffuse_Statement(_) => trueCon
      case _ => translate_Claim(provedClaim._claim)(defContext)
    }
    val prf = (provedClaim._claim, provedClaim._just) match {
      case (_, Some(just)) => translate_Justification(just, claim)(defContext)
      case (it: Iterative_Equality, None) => translate_Iterative_Equality_Proof(it)(defContext)
      case (_, None) => throw ProvedClaimTranslationError("No proof given for claim, which is not an iterative-equality (proving itself). ", provedClaim)
    }
    (proof(claim), prf)
  }
  def translate_Justification(just:Justification, claim: Term)(implicit defContext: DefinitionContext, bindArgs: Boolean = true): objects.Term = just match {
    case sj: Scheme_Justification if proofSteps => translate_Scheme_Justification(sj)
    case _: Justification =>
      if (proofSteps) defContext.enterProof
      val usedFacts: List[Term] = usedInJustification(just)
      if (proofSteps) defContext.exitProof
      lambdaBindDefCtxArgs(uses(claim, usedFacts))
  }
  def globalReferences(refs: List[Reference]): List[Term] = refs.filter(_.isInstanceOf[Theorem_Reference]).map(_.asInstanceOf[GlobalReference].referencedItem)
  private def translate_Scheme_Justification(sj:Scheme_Justification) = lf.ApplyGeneral(OMS(sj.referencedScheme),
    sj._refs.map(_.referencedLabel.asInstanceOf[GlobalName]).map(OMS(_)))
  private def lambdaBindDefCtxArgs(tm: Term)(implicit defContext: DefinitionContext, bindArgs: Boolean = true): Term = {
    if (defContext.args.nonEmpty && bindArgs) lambdaBindArgs(tm)(defContext.args.map(_.toTerm)) else tm
  }
  private def translate_Iterative_Equality_Proof(it: Iterative_Equality)(implicit defContext: DefinitionContext): objects.Term = {
    val claim = translate_Claim(it)
    val usedFacts: List[Term] = it._just::it._iterSteps._iterSteps.map(_._just) flatMap usedInJustification
    lambdaBindDefCtxArgs(uses(claim, usedFacts))
  }
  private def translate_Exemplification(exemplification: Exemplification)(implicit defContext: => DefinitionContext): List[objects.Term] = {
    exemplification._exams map { exam =>
      val exemTm: Term = translate_Term(exam._tm)(defContext)
      val exemTp: Term = TranslationController.inferType(exemTm)(defContext)
      ProofByExample(exemTp, exemTm)
    }
  }
  private def translate_Diffuse_Statement_Claim(ds: Diffuse_Statement, _just: Option[Justification])(implicit defContext: DefinitionContext): Term = trueCon
  def usedInJustification(just: Justification)(implicit defContext: => DefinitionContext): List[Term] = just match {
    case Straightforward_Justification(_refs) => globalReferences(_refs)
    case Block(pos, _, _items) =>
      if (proofSteps) defContext.enterProof
      def translateSubitems(subs: List[Subitem]): List[Term] = subs.flatMap {
        case st: Statement =>
          val usedInJust = (st.prfClaim._claim, st.prfClaim._just) match {
            case (it:Iterative_Equality, None) => it._iterSteps._iterSteps.map(_._just) flatMap(usedInJustification(_)(defContext))
            case (_: Claim, jO) => usedInJustification(jO.get)(defContext)
          }
          (if (proofSteps) {
            List(st match {
              case cs: Choice_Statement =>
                val (addArgs, (claim, _), _) = translate_Choice_Statement(cs)(defContext)
                defContext.addArguments(addArgs)
                claim
              case ds: Diffuse_Statement => translate_Diffuse_Statement_Claim(ds, st.prfClaim._just)(defContext)
              case _ => translate_Claim(st.prfClaim._claim)(defContext)
            })
          } else Nil) ::: usedInJust
        case Per_Cases(_just) => usedInJustification(_just)(defContext)
        case ex: Exemplification if (proofSteps) => translate_Exemplification(ex)(defContext)
        case Existential_Assumption(_qual, _) if (proofSteps) =>
          _qual._children.flatMap(contextTranslator.translate_Context(_)(defContext)) foreach defContext.addLocalBindingVar
          Nil
        case Default_Generalization(_qual, _conds) if (proofSteps) =>
          _qual._children.flatMap(contextTranslator.translate_Context(_)(defContext)) foreach defContext.addLocalBindingVar
          _conds foreach (cl => defContext.addAssumption(translate_Claim(cl)(defContext)))
          Nil
        case Generalization(_qual, _conds) if (proofSteps) =>
          _qual._children.flatMap(contextTranslator.translate_Context(_)(defContext)) foreach defContext.addLocalBindingVar
          _conds foreach (cl => defContext.addAssumption(translate_Claim(cl)(defContext)))
          Nil
        case prDef: PrivateDefinition if (proofSteps) =>
          // This will add the definition to the list of local definitions inside the definition context
          translate_Definition(prDef, None)(defContext)
          Nil
        case _ => Nil
      }
      val subitems = translateSubitems(_items map (_._subitem))
      if (proofSteps) defContext.exitProof
      subitems
    case sj: Scheme_Justification => if (proofSteps) List(translate_Scheme_Justification(sj)) else OMS(sj.referencedScheme)::globalReferences(sj._refs)
  }
}