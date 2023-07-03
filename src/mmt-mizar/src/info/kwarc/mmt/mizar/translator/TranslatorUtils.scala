package info.kwarc.mmt.mizar.translator

import info.kwarc.mmt.api.symbols.{Declaration, HasDefiniens, HasNotation, HasType}
import info.kwarc.mmt.api.{objects, _}
import objects._
import info.kwarc.mmt.mizar._
import mmtwrapper.MizarPrimitiveConcepts._
import mmtwrapper.MizSeq._
import mmtwrapper.MizarPrimitiveConcepts
import syntax._
import termTranslator._

sealed class TranslatingError(str: String) extends Exception(str)
class DeclarationLevelTranslationError(str: String, decl: Subitem) extends TranslatingError(str)
case class DeclarationTranslationError(str: String, decl: Subitem) extends DeclarationLevelTranslationError(str, decl) {
  def apply(str: String, decl: Subitem) = {
    new DeclarationLevelTranslationError(str+
      "\nDeclarationTranslationError while translating the "+decl.shortKind+": "+decl.toString, decl)
  }
}
class ObjectLevelTranslationError(str: String, tm: ObjectLevel) extends TranslatingError(str)
case class ProvedClaimTranslationError(str: String, prfedClaim: ProvedClaim) extends TranslatingError(str+
  "\nProvedClaimTranslationError while translating the (proved) "+prfedClaim._claim.getClass.getName+": "+prfedClaim.toString)
case class PatternTranslationError(str: String, pat: Patterns) extends ObjectLevelTranslationError(str+
  "\nPatternClaimTranslationError while translating the pattern with spelling "+pat.patternAttrs.spelling+": "+pat.toString, pat)
case class ExpressionTranslationError(str: String, expr: Expression) extends ObjectLevelTranslationError(str+
  "\nExpressionTranslationError while translating the expression "+expr.ThisType()+": "+expr.toString, expr)
object ExpectedTheoryAt {
  def apply(mpath: MPath) = new TranslatingError("Error looking up the theory at: "+mpath.toString)
}

object TranslatorUtils {
  def makeGlobalName(aid: String, kind: String, nr: Int) : info.kwarc.mmt.api.GlobalName = Utils.makeGlobalName(aid, kind, nr.toString)
  def makeGlobalPatConstrName(patAid: String, constrAid: String, kind: String, patNr: Int, constrNr: Int) : info.kwarc.mmt.api.GlobalName = {
    val patGN = makeGlobalName(patAid, kind, patNr)
    val constrGN = makeGlobalName(constrAid, kind, constrNr)
    constrGN.copy(name = LocalName(patGN.name.toString + constrGN.name.toString))
  }

  def computeGlobalName(pat: GloballyReferencingObjAttrs, orgVersion: Boolean = false) = pat match {
    case p: RedefinablePatterns => if (orgVersion) p.globalOrgPatConstrName else p.globalPatConstrName
    case p: ConstrPattern => p.globalPatConstrName
    case objAttrs: GloballyReferencingReDefAttrs => objAttrs.globalPatConstrName
    case objAttrs: GloballyReferencingDefAttrs => objAttrs.globalPatConstrName
    case objAttrs => objAttrs.globalPatternName
  }
  def negatedFormula(form:Claim) = Negated_Formula(form)
  def emptyCondition() = negatedFormula(Contradiction())

  private def namedDefArgsSubstition(args: Context, varName: LocalName = LocalName(mmtwrapper.PatternUtils.argsVarName)): objects.Substitution = {
    val (argNum, argTps) = (args.length, args map (_.toTerm))
    objects.Substitution(argTps.zipWithIndex map {
      case (vd, i) => vd / Index(OMV(varName),  OMI(i))
    }:_*)
  }
  /**
   * Compute a substitution substituting the arguments from the implicitely sublied definition context by terms of the form Index(OMV(<varName>), OMI(i)),
   * where <varName> is the name of the argument sequence from the corresponding pattern
   *
   * This is make the arguments used the names actually used in the binder of the pattern,
   * not the names given to the arguments in Mizar
   * @param varName (default PatternUtils.argsVarName) The name of the argument sequence in the corresponding pattern
   * @param defContext (implicit) the arguments to build the substitution for
   * @return A substitution replacing the arguments by the corresponding index terms
   */
  def implicitNamedDefArgsSubstition(varName: LocalName = LocalName(mmtwrapper.PatternUtils.argsVarName))(implicit defContext: DefinitionContext): objects.Substitution = namedDefArgsSubstition(defContext.args, varName)
  /**
   * Compute a translating function any references to constants in hidden to the corresponding ones in the Mizar base theories
   * @return A translation function on declarations making the substitution
   */
  def hiddenRefTranslator(d: symbols.Declaration with HasType with HasDefiniens with HasNotation) = {
    d.translate(OMSReplacer(gn => resolveHiddenReferences(gn)).toTranslator()).asInstanceOf[Declaration with HasType with HasDefiniens with HasNotation]
  }
  def translateArguments(arguments: Arguments)(implicit defContext: DefinitionContext, selectors: List[(Int, VarDecl)] = Nil) : List[Term] = { arguments._children map translate_Term }
  val hiddenArt = TranslationController.getTheoryPath("hidden")

  /**
   * Since the theories hidden, tarski, tarksi_a are not translated but used as dependencies
   * and their content is instead defined in the mizar base theories in latin2
   * use this to translate those basic notions
   * @param name
   * @return
   */
  def resolveHiddenReferences(gn: GlobalName) = {
    def modeKind(p: Int) = Utils.shortKind(Utils.ModeKind()).toString+p.toString
    def predKind(p: Int, c: Int) = Utils.shortKind(Utils.PredicateKind()).toString+p.toString+Utils.shortKind(Utils.PredicateKind())+c.toString
    gn match {
      case Utils.SimpleGlobalName(aid, name) if (gn.module == hiddenArt) => name match {
        case str if str == modeKind(1) => Some(any)
        case str if str == modeKind(2) => Some(set)
        case str if str == predKind(1, 1) => Some(MizarPrimitiveConcepts.equal.term)
        case str if str == predKind(2, 1) => Some(neq.term)
        case str if str == predKind(3, 2) => Some(in)
        case _ => throw new ImplementationError("Failure to translate the reference to a declaration in hidden of name "+name+". ")
      }
      case _ => None
    }
  }
}