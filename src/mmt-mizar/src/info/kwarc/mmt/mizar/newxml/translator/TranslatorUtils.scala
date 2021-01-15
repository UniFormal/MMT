package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import notations.NotationContainer
import objects.{OMMOD, OMV}
import info.kwarc.mmt.mizar.newxml.syntax._
import Utils.MizarGlobalName
import info.kwarc.mmt.mizar.newxml.translator._
import termTranslator._
import typeTranslator._
import variableTranslator._

sealed abstract class TranslatingError(str: String) extends Exception(str)
class DeclarationLevelTranslationError(str: String, decl: DeclarationLevel) extends TranslatingError(str)
case class DeclarationTranslationError(str: String, decl: Subitem) extends DeclarationLevelTranslationError(str, decl) {
  def apply(str: String, decl: Subitem) = {
    new DeclarationLevelTranslationError(str+
      "\nDeclarationTranslationError while translating the "+decl.shortKind+": "+decl.toString, decl)
  }
}
class ObjectLevelTranslationError(str: String, tm: ObjectLevel) extends TranslatingError(str)
case class ExpressionTranslationError(str: String, expr: Expression) extends ObjectLevelTranslationError(str+
  "\nObjectTranslationError while translating the expression "+expr.ThisType()+": "+expr.toString, expr)
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
  def computeGlobalPatternName(tpAttrs: globallyReferencingObjAttrs) = {MMLIdtoGlobalName(tpAttrs.globalPatternName)}
  def computeGlobalOrgPatternName(tpAttrs: globallyReferencingReDefObjAttrs) = {MMLIdtoGlobalName(tpAttrs.globalPatternName)}
  def addConstant(gn:info.kwarc.mmt.api.GlobalName, notC:NotationContainer, df: Option[objects.Term], tp:Option[objects.Term] = None) = {
    val hm : Term= OMMOD(gn.module).asInstanceOf[Term]
    val const = info.kwarc.mmt.api.symbols.Constant(OMMOD(gn.module), gn.name, Nil, tp, df, None, notC)
    TranslationController.add(const)
  }
  def conforms(A:Type, B:Type) : Boolean = {
    val List(a,b) = List(A,B).map(translate_Type(_))
    val List(as, bs) = List(a,b) map TranslationController.simplifyTerm
    as == bs
  }
  def negatedFormula(form:Claim) = Negated_Formula(RedObjSubAttrs(emptyPosition(),"Negated-Formula"),form)
  def emptyCondition() = negatedFormula(Contradiction(RedObjSubAttrs(emptyPosition(),"Contradiction")))
  def emptyPosition() = Position("translation internal")
  def getUniverse(tp:Type) : Term = tp match {
    case Standard_Type(tpAttrs, _, _, elementArgs) if (elementArgs._children.length == 1) =>
      elementArgs match { case Arguments(List(u)) => u }
    case Standard_Type(tpAttrs, noocc, origNr, elementArgs) =>throw ExpressionTranslationError("Expected a type of form\"<element of> <tp>\" for some type tp. "+
      "In particular expected exactly 1 argument, but found "+elementArgs._children.length+". ", tp)
    case _ => throw ExpressionTranslationError("Expected a type of form\"<element of> <tp>\" for some type tp. ", tp)
  }
  def getVariables(varSegms: Variable_Segments) : List[Variable] = varSegms._vars.flatMap {
    case segm: VariableSegments => segm._vars()
  }
  def translateVariables(varSegms: VariableSegments) : List[OMV] = {varSegms._vars().map(translate_Variable)}
  def translateVariables(varSegms: Variable_Segments) : List[OMV] = {getVariables(varSegms).map(translate_Variable)}
  def firstVariableUniverse(varSegms: VariableSegments) : Type = {
    assert(! varSegms._vars().isEmpty)
    varSegms._tp()
  }
  def firstVariableUniverse(varSegm: Variable_Segments) : Type = {
    varSegm._vars.head._tp()
  }
  def translateArguments(args: Arguments)(implicit selectors: List[(Int, objects.VarDecl)] = Nil) : List[objects.Term] = {args._children map translate_Term }
  def translateObjRef(refObjAttrs:globallyReferencingObjAttrs)  = objects.OMS(computeGlobalPatternName(refObjAttrs))
}