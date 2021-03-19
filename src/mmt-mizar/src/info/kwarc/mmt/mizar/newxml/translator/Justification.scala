package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt._
import api._
import info.kwarc.mmt.mizar.newxml.mmtwrapper.MizarPrimitiveConcepts._
import info.kwarc.mmt.mizar.newxml.mmtwrapper.PatternUtils.{PiOrEmpty, lambdaBindArgs}
import info.kwarc.mmt.mizar.newxml.translator.claimTranslator._
import info.kwarc.mmt.mizar.newxml.translator.definitionTranslator.translate_Definition
import info.kwarc.mmt.mizar.newxml.translator.statementTranslator.translate_Choice_Statement
import info.kwarc.mmt.mizar.newxml.translator.termTranslator.translate_Term
import objects._
import mizar.newxml.syntax._

object JustificationTranslator {
  def translate_Justification(just:Justification, claim: Term)(implicit defContext: DefinitionContext): objects.Term = just match {
    case Straightforward_Justification(_refs) => lambdaBindDefCtxArgs(uses(claim, globalReferences(_refs)))
    case _: Block =>
      defContext.enterProof
      val usedFacts: List[Term] = usedInJustification(just)
      defContext.exitProof
      //TODO: actually translate the proofs, may need additional arguments from the context, for instance the claim to be proven
      lambdaBindArgs(uses(claim, usedFacts))(defContext.args.map(_.toTerm))
    case sj: Scheme_Justification => lambdaBindDefCtxArgs(uses(claim, globalReferences(sj._refs)))//translate_Scheme_Justification(sj)
  }
  def globalReferences(refs: List[Reference]): List[Term] = refs flatMap {
    case ref: Theorem_Reference => Some(ref.referencedLabel())
    case ref: Definition_Reference => Some(ref.referencedLabel())
    case _ => None
  }
  private def translate_Iterative_Equality_Proof(it: Iterative_Equality)(implicit defContext: DefinitionContext): objects.Term = {
    val claim = translate_Claim(it)
    defContext.enterProof
    val usedFacts: List[Term] = it._just::it._iterSteps.map(_._just) flatMap usedInJustification
    defContext.exitProof
    //TODO: actually translate the proofs, may need additional arguments from the context, for instance the claim to be proven
    lambdaBindDefCtxArgs(uses(claim, usedFacts))
  }
  private def translate_Exemplification(exemplification: Exemplification)(implicit defContext: => DefinitionContext): List[objects.Term] = {
    exemplification._exams map { exam =>
      val exemTm: Term = translate_Term(exam._tm)(defContext)
      val exemTp: Term = TranslationController.inferType(exemTm)(defContext)
      ProofByExample(exemTp, exemTm)
    }
  }
  private def lambdaBindDefCtxArgs(tm: Term)(implicit defContext: DefinitionContext): Term = {
    if (defContext.args.nonEmpty) lambdaBindArgs(tm)(defContext.args.map(_.toTerm)) else tm
  }
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
  private def translate_Diffuse_Statement(ds: Diffuse_Statement, _just: Option[Justification])(implicit defContext: DefinitionContext): Term = trueCon
  private def translate_Scheme_Justification(sj:Scheme_Justification) = lf.ApplyGeneral(OMS(sj.referencedScheme), sj._refs.map(_.referencedLabel) map (OMS(_)))
  def usedInJustification(just: Justification)(implicit defContext: => DefinitionContext): List[Term] = just match {
    case Straightforward_Justification(_refs) => globalReferences(_refs)
    case Block(_, _items) =>
      def translateSubitems(subs: List[Subitem]): List[Term] = subs match {
        case Nil => Nil
        case it :: tail => it match {
          case st: Statement =>
            val ProvedClaim(clm, j) = st.prfClaim
            val claim = clm match {
              case ds: Diffuse_Statement => translate_Diffuse_Statement(ds, j)(defContext)
              case _ => translate_Claim(clm)(defContext)
            }
            st match {
              case cs: Choice_Statement =>
                val (addArgs, _) = translate_Choice_Statement(cs)(defContext)
                defContext.addArguments(addArgs)
              case _ =>
            }
            defContext.enterProof
            val trIt = st.prfClaim._claim match {
              case _: Diffuse_Statement => j map (usedInJustification(_)(defContext)) getOrElse Nil
              case Proposition(_, Thesis()) => j map (usedInJustification(_)(defContext)) getOrElse Nil
              case it: Iterative_Equality if j.isEmpty =>
                val And(clms) = translate_Claim(it)(defContext)
                val justs = it._just :: it._iterSteps.map(_._just)
                clms.map(PiOrEmpty(defContext.getLocalBindingVars, _)) ::: justs.flatMap(usedInJustification(_)(defContext))
              case claim =>
                PiOrEmpty(defContext.getLocalBindingVars, translate_Claim(claim)(defContext)) :: (j map (usedInJustification(_)(defContext)) getOrElse Nil)
            }
            defContext.exitProof
            trIt ::: translateSubitems(tail)
          case ex: Exemplification => translate_Exemplification(ex)(defContext) ::: translateSubitems(tail)
          case Per_Cases(_just) =>
            val (caseBlocks, remainingTail) = tail.span { case _: Case_Block => true case _ => false }
            val usedInCases = caseBlocks flatMap ( j => translateSubitems(List(j)) )
            usedInJustification(_just)(defContext):::usedInCases:::translateSubitems(remainingTail)
          case Assumption(ass) => translateSubitems(tail)
          case Existential_Assumption(_qual, _) =>
            _qual._children.flatMap(contextTranslator.translate_Context(_)(defContext)) foreach defContext.addLocalBindingVar
            translateSubitems(tail)
          case _: Reduction => translateSubitems(tail) //TODO: translate this to something
          case _: Identify => translateSubitems(tail)
          case Default_Generalization(_qual, _conds) =>
            _qual._children.flatMap(contextTranslator.translate_Context(_)(defContext)) foreach defContext.addLocalBindingVar
            _conds foreach (cl => defContext.addAssumption(translate_Claim(cl)(defContext)))
            translateSubitems(tail)
          case Generalization(_qual, _conds) =>
            _qual._children.flatMap(contextTranslator.translate_Context(_)(defContext)) foreach defContext.addLocalBindingVar
            _conds foreach (cl => defContext.addAssumption(translate_Claim(cl)(defContext)))
            translateSubitems(tail)
          case prDef: PrivateDefinition =>
            // This will add the definition as to the list of local definitions inside the definition context
            translate_Definition(prDef)(defContext)
            translateSubitems(tail)
          case _ => Nil
        }
      }
      Nil //translateSubitems(_items map (_._subitem))
    case sj:Scheme_Justification => globalReferences(sj._refs)
  }
}