package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.symbols.OMSReplacer
import info.kwarc.mmt.api.{objects, _}
import info.kwarc.mmt.lf.elpi.ELPI.Lambda
import info.kwarc.mmt.lf.{Pi, Univ}
import notations.NotationContainer
import objects._
import info.kwarc.mmt.mizar.newxml._
import mmtwrapper.MizarPrimitiveConcepts._
import mmtwrapper.MMTUtils.Lam
import mmtwrapper.MizSeq._
import mmtwrapper.MizarPrimitiveConcepts
import info.kwarc.mmt.mizar.newxml.mmtwrapper.PatternUtils._
import syntax._
import info.kwarc.mmt.mizar.newxml.translator.contextTranslator.translate_Variable
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

object TranslatorUtils {
  def makeGlobalName(aid: String, kind: String, nr: Int) : info.kwarc.mmt.api.GlobalName = {
    val ln = LocalName(kind+nr)
    TranslationController.getTheoryPath(aid) ? ln
  }
  def makeGlobalPatConstrName(patAid: String, constrAid: String, kind: String, patNr: Int, constrNr: Int) : info.kwarc.mmt.api.GlobalName = {
    val patGN = makeGlobalName(patAid, kind, patNr)
    val constrGN = makeGlobalName(constrAid, kind, constrNr)
    constrGN.copy(name = LocalName(patGN.name.toString + constrGN.name.toString))
  }
  def makeNewGlobalName(kind: String, nr: Int) = makeGlobalName(TranslationController.currentAid, kind, nr)

  def computeGlobalName(pat: GloballyReferencingObjAttrs, orgVersion: Boolean = false) = pat match {
    case p: RedefinablePatterns => if (orgVersion) p.globalOrgPatConstrName else p.globalPatConstrName
    case p: ConstrPattern => p.globalPatConstrName
    case objAttrs: GloballyReferencingReDefAttrs => objAttrs.globalPatConstrName
    case objAttrs: GloballyReferencingDefAttrs => objAttrs.globalPatConstrName
    case objAttrs => objAttrs.globalPatternName
  }
  def addConstant(gn:info.kwarc.mmt.api.GlobalName, notC:NotationContainer, df: Option[Term], tp:Option[Term] = None) = {
    val hm : Term= OMMOD(gn.module).asInstanceOf[Term]
    val const = info.kwarc.mmt.api.symbols.Constant(OMMOD(gn.module), gn.name, Nil, tp, df, None, notC)
    TranslationController.add(const)
  }
  def emptyPosition() = syntax.Position("translation internal")
  def negatedFormula(form:Claim) = Negated_Formula(emptyPosition(), form)
  def emptyCondition() = negatedFormula(Contradiction(emptyPosition()))

  def getVariables(varSegms: Variable_Segments) : List[Variable] = varSegms._vars.flatMap {
    case segm: VariableSegments => segm._vars()
  }

  private def namedDefArgsSubstition(args: Context, varName: LocalName): objects.Substitution = {
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
  def namedDefArgsSubstition(varName: LocalName = LocalName(mmtwrapper.PatternUtils.argsVarName))(implicit defContext: DefinitionContext): objects.Substitution = namedDefArgsSubstition(defContext.args, varName)
  /**
   * Compute a translating function any references to constants in hidden to the corresponding ones in the Mizar base theories
   * @return A translation function on declarations making the substitution
   */
  def hiddenRefTranslator(d: symbols.Declaration): symbols.Declaration = {
    d.translate(OMSReplacer(gn => resolveHiddenReferences(gn)).toTranslator(), Context.empty)
  }
  def translateArguments(arguments: Arguments)(implicit defContext: DefinitionContext, selectors: List[(Int, VarDecl)] = Nil) : List[Term] = { arguments._children map translate_Term }
  val hiddenArt = TranslationController.getTheoryPath("hidden")
  val hiddenArts = List("hidden", "tarski", "tarski_a") map TranslationController.getTheoryPath

  /**
   * Since the theories hidden, tarski, tarksi_a are not translated but used as dependencies
   * and their content is instead defined in the mizar base theories in latin2
   * use this to translate those basic notions
   * @param name
   * @return
   */
  def resolveHiddenReferences(gn: GlobalName) = {
    //This mess is necessary because eq and neq have same constrnr and new patternnrs can be defined for either in certain redefinitions
    val neqPats = List("R2")
    val eqPats = List("R1", "R4")
    gn match {
      case GlobalName(module, name) if (module == hiddenArt) => name.toString match {
        //TODO: Also translate content of tarski, tarksi_a?
        case str if (str.endsWith("M1")) => Some(any)
        case str if (str.endsWith("M2")) => Some(set)
        case str if (str.endsWith("R1") && neqPats.exists(str.endsWith(_))) => Some(neq.term)
        case str if (str.endsWith("R1") && eqPats.exists(str.endsWith(_))) => Some(MizarPrimitiveConcepts.eq.term)
        case str if (str.endsWith("R2") || str.endsWith("R3")) => Some(in)
        case _ =>
          throw new ImplementationError("Failure to translate the reference to a declaration in hidden of name "+name.toString)
      }
      case _ => None
    }
  }
}
