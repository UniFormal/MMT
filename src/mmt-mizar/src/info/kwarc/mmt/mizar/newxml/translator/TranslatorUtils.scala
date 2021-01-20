package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import notations.NotationContainer
import objects._
import info.kwarc.mmt.mizar.newxml._
import syntax.Utils.MizarGlobalName
import syntax.{Arguments, Claim, Contradiction, DeclarationLevel, Expression, Negated_Formula, ObjectLevel, Position, Property, ProvedClaim, RedObjSubAttrs, RedObjectSubAttrs, Standard_Type, Subitem, Type, Variable, VariableSegments, Variable_Segments, globallyReferencingDefAttrs, globallyReferencingObjAttrs, globallyReferencingReDefAttrs}
import info.kwarc.mmt.mizar.newxml.translator.contextTranslator.translate_Variable
import termTranslator._

sealed abstract class TranslatingError(str: String) extends Exception(str)
class DeclarationLevelTranslationError(str: String, decl: DeclarationLevel) extends TranslatingError(str)
case class DeclarationTranslationError(str: String, decl: Subitem) extends DeclarationLevelTranslationError(str, decl) {
  def apply(str: String, decl: Subitem) = {
    new DeclarationLevelTranslationError(str+
      "\nDeclarationTranslationError while translating the "+decl.shortKind+": "+decl.toString, decl)
  }
}
class ObjectLevelTranslationError(str: String, tm: ObjectLevel) extends TranslatingError(str)
case class ProvedClaimTranslationError(str: String, prfedClaim: ProvedClaim) extends ObjectLevelTranslationError(str+
  "\nProvedClaimTranslationError while translating the (proved) "+prfedClaim._claim.getClass.getName+": "+prfedClaim.toString, prfedClaim)
case class ExpressionTranslationError(str: String, expr: Expression) extends ObjectLevelTranslationError(str+
  "\nExpressionTranslationError while translating the expression "+expr.ThisType()+": "+expr.toString, expr)

object TranslatorUtils {
  def makeGlobalName(aid: String, kind: String, nr: Int) : info.kwarc.mmt.api.GlobalName = {
    val ln = LocalName(kind+":"+nr)
    TranslationController.getTheoryPath(aid) ? ln
  }
  def MMLIdtoGlobalName(mizarGlobalName: MizarGlobalName): info.kwarc.mmt.api.GlobalName = {
    makeGlobalName(mizarGlobalName.aid, mizarGlobalName.kind, mizarGlobalName.nr)
  }
  def computeGlobalPatternName(tpAttrs: globallyReferencingObjAttrs) = {MMLIdtoGlobalName(tpAttrs.globalPatternName())}
  def computeGlobalConstrName(tpAttrs: globallyReferencingDefAttrs) = {MMLIdtoGlobalName(tpAttrs.globalConstrName())}
  def computeGlobalOrgPatternName(tpAttrs: globallyReferencingReDefAttrs) = {MMLIdtoGlobalName(tpAttrs.globalOrgPatternName())}
  def computeGlobalOrgConstrName(tpAttrs: globallyReferencingReDefAttrs) = {MMLIdtoGlobalName(tpAttrs.globalOrgConstrName())}
  def addConstant(gn:info.kwarc.mmt.api.GlobalName, notC:NotationContainer, df: Option[Term], tp:Option[Term] = None) = {
    val hm : Term= OMMOD(gn.module).asInstanceOf[Term]
    val const = info.kwarc.mmt.api.symbols.Constant(OMMOD(gn.module), gn.name, Nil, tp, df, None, notC)
    TranslationController.add(const)
  }
  def emptyPosition() = Position("translation internal")
  def negatedFormula(form:Claim) = Negated_Formula(RedObjSubAttrs(emptyPosition(),"Negated-Formula"),form)
  def emptyCondition() = negatedFormula(Contradiction(RedObjSubAttrs(emptyPosition(),"Contradiction")))

  /**
   * Retrieves the type argument <T> of a type <tp> of the form Element of <T>
   * @param tp the type <tp>
   * @return
   */
  def getUniverse(tp:Type)(implicit args: Context= Context.empty, assumptions: List[Term] = Nil, corr_conds: List[JustifiedCorrectnessConditions] = Nil, props: List[Property] = Nil) : Term = tp match {
    case Standard_Type(tpAttrs, _, _, Arguments(List(u))) => translate_Term(u)
    case Standard_Type(tpAttrs, _, _, elementArgs) => throw ExpressionTranslationError("Expected a type of form\"<element of> <tp>\" for some type tp. "+
      "In particular expected exactly 1 argument, but found "+elementArgs._children.length+". ", tp)
    case _ => throw ExpressionTranslationError("Expected a type of form\"<element of> <tp>\" for some type tp. ", tp)
  }
  def getVariables(varSegms: Variable_Segments) : List[Variable] = varSegms._vars.flatMap {
    case segm: VariableSegments => segm._vars()
  }
  def translateVariables(varSegms: VariableSegments) : List[OMV] = {varSegms._vars().map(translate_Variable)}
  def translateVariables(varSegms: Variable_Segments) : List[OMV] = {getVariables(varSegms).map(translate_Variable)}

  /**
   * Compute a substitution substituting the implicitely sublied argument by terms of the form OMV(<varName> / i),
   * where <varName> is the name of the argument sequence from the corresponding pattern
   *
   * This is make the arguments used the names actually used in the binder of the pattern,
   * not the names given to the arguments in Mizar
   * @param varName (default "x") The name of the argument sequence in the corresponding pattern
   * @param args (implicit) the arguments to build the substitution for
   * @return
   */
  def namedDefArgsSubstition(args: Context, varName: String = "x") = {
    val (argNum, argTps) = (args.length, args map (_.toTerm))
    objects.Substitution(argTps.zipWithIndex map {
      case (vd, i) => vd / objects.OMV(LocalName(varName) / i.toString)//Index(OMV(varName), OMI(i))
    }:_*)
  }
  /**
   * Compute a translator substituting the implicitely sublied argument within type and definition of a declaration by terms of the form OMV(<varName> / i),
   * where <varName> is the name of the argument sequence from the corresponding pattern
   *
   * This is make the arguments use the names actually used in the binder of the pattern,
   * not the names given to the arguments in Mizar
   * @param varName (default "x") The name of the argument sequence in the corresponding pattern
   * @param args (implicit) the arguments to build the substitution for
   * @return A translation function on declarations making the substitution
   */
  private def namedDefArgsTranslator(varName: String, args: Context) : symbols.Declaration => symbols.Declaration = {
    {d: symbols.Declaration =>
      val tl = symbols.ApplySubs(namedDefArgsSubstition(args, varName))
     d.translate(tl, Context.empty)}
  }
  def namedDefArgsTranslator(varName: String = "x")(implicit defContext: DefinitionContext) : symbols.Declaration => symbols.Declaration = namedDefArgsTranslator(varName, defContext.args)
  def translateArguments(arguments: Arguments)(implicit args: Context = Context.empty, assumptions: List[Term] = Nil, corr_conds: List[JustifiedCorrectnessConditions] = Nil, props: List[Property] = Nil, selectors: List[(Int, VarDecl)] = Nil) : List[Term] = {arguments._children map translate_Term }
  def translateObjRef(refObjAttrs:globallyReferencingObjAttrs)  = OMS(computeGlobalPatternName(refObjAttrs))
}