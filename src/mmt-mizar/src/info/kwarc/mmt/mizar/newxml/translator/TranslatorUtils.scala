package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.{LocalName, objects}
import info.kwarc.mmt.api.objects.{OMMOD, OMV}
import info.kwarc.mmt.mizar.newxml.syntax.Utils.MizarGlobalName
import info.kwarc.mmt.mizar.newxml.syntax.{Arguments, Claim, ConstrExtObjAttrs, Contradiction, DeclarationLevel, Expression, ExtObjAttrs, Negated_Formula, ObjectLevel, Position, RedObjSubAttrs, Sort, Spelling, Standard_Type, Subitem, Term, Type, Variable, VariableSegments, Variable_Segments, referencingConstrObjAttrs, referencingObjAttrs}
import info.kwarc.mmt.mizar.newxml.translator.{TranslationController, termTranslator, variableTranslator}

sealed abstract class TranslatingError(str: String) extends Exception(str)
class DeclarationLevelTranslationError(str: String, decl: DeclarationLevel) extends TranslatingError(str)
case class DeclarationTranslationError(str: String, decl: Subitem) extends DeclarationLevelTranslationError(str, decl) {
  def apply(str: String, decl: Subitem) = {
    new DeclarationLevelTranslationError(str+
      "\nDeclarationTranslationError while translating the "+decl.shortKind+": "+decl.toString, decl)
  }
}
class ObjectLevelTranslationError(str: String, tm: ObjectLevel) extends TranslatingError(str)
case class ExpressionTranslationError(str: String, tm: Expression) extends ObjectLevelTranslationError(str, tm) {
  def apply(str: String, expr: Expression) = {
    new ObjectLevelTranslationError(str+
      "\nObjectTranslationError while translating the expression "+expr.ThisType()+": "+expr.toString, expr)
  }
}
object TranslatorUtils {
  def makeGlobalName(aid: String, kind: String, nr: Int) : info.kwarc.mmt.api.GlobalName = {
    val ln = LocalName(kind+":"+nr)
    TranslationController.getTheoryPath(aid) ? ln
  }
  def makeNewGlobalName(kind: String, nr: Int) = {
    val ln = LocalName(kind+":"+nr)
    TranslationController.currentTheoryPath ? ln
  }
  def MMLIdtoGlobalName(mizarGlobalName: MizarGlobalName): info.kwarc.mmt.api.GlobalName = {
    makeGlobalName(mizarGlobalName.aid, mizarGlobalName.kind, mizarGlobalName.nr)
  }
  // TODO: replace by global versions of it, once the test files contain the corresponding global attributes
  def computeGlobalName(tpAttrs: referencingObjAttrs) = {MMLIdtoGlobalName(tpAttrs.globalName(
    TranslationController.currentAid))}
  def computeStrGlobalName(tpAttrs: referencingConstrObjAttrs) = {
    makeGlobalName(TranslationController.currentAid, "Struct-Type", tpAttrs.patternNr.patternnr)
  }
  def addConstant(gn:info.kwarc.mmt.api.GlobalName, notC:NotationContainer, df: Option[objects.Term], tp:Option[objects.Term] = None) = {
    val hm : Term= OMMOD(gn.module).asInstanceOf[Term]
    val const = info.kwarc.mmt.api.symbols.Constant(OMMOD(gn.module), gn.name, Nil, tp, df, None, notC)
    TranslationController.add(const)
  }
  def conforms(A:Type, B:Type) : Boolean = {
    val List(a,b) = List(A,B).map(typeTranslator.translate_Type(_))
    val List(as, bs) = List(a,b) map TranslationController.simplifyTerm
    as == bs
  }
  def negatedFormula(form:Claim) = Negated_Formula(RedObjSubAttrs(emptyPosition(),Sort("Negated-Formula")),form)
  def emptyCondition() = negatedFormula(Contradiction(RedObjSubAttrs(emptyPosition(),Sort("Contradiction"))))
  def emptyPosition() = Position("translation internal")
  def getUniverse(tp:Type) : Term = tp match {
    case Standard_Type(ExtObjAttrs(_, _, _, Spelling("Element"), Sort("Mode")), _, _, args) =>
      args match { case List(Arguments(List(u))) => u }
  }
  def getVariables(varSegms: Variable_Segments) : List[Variable] = varSegms._vars.flatMap {
    case segm: VariableSegments => segm._vars()
  }
  def translateVariables(varSegms: VariableSegments) : List[OMV] = {varSegms._vars().map(variableTranslator.translate_Variable)}
  def translateVariables(varSegms: Variable_Segments) : List[OMV] = {getVariables(varSegms).map(variableTranslator.translate_Variable)}
  def firstVariableUniverse(varSegms: VariableSegments) : Type = {
    assert(! varSegms._vars().isEmpty)
    varSegms._tp()
  }
  def firstVariableUniverse(varSegm: Variable_Segments) : Type = {
    varSegm._vars.head._tp()
  }
  def translateArguments(args: Arguments)(implicit selectors: List[(Int, objects.VarDecl)] = Nil) : List[objects.Term] = {args._children map termTranslator.translate_Term }
  def translateObjRef(refObjAttrs:referencingObjAttrs)  = objects.OMS(computeGlobalName(refObjAttrs))
}