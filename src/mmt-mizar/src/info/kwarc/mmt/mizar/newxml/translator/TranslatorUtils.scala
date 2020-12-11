package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.{LocalName, objects}
import info.kwarc.mmt.api.objects.{OMMOD, OMV}
import info.kwarc.mmt.mizar.newxml.syntax.Utils.MizarGlobalName
import info.kwarc.mmt.mizar.newxml.syntax.{Arguments, Claim, Contradiction, ExtObjAttrs, Negated_Formula, Position, RedObjSubAttrs, Sort, Spelling, Standard_Type, Term, Type, Variable, VariableSegments, Variable_Segments, referencingObjAttrs}
import info.kwarc.mmt.mizar.newxml.translator.{TranslationController, termTranslator, variableTranslator}

object Utils {
  def MMLIdtoGlobalName(mizarGlobalName: MizarGlobalName): info.kwarc.mmt.api.GlobalName = {
    val theoryName = LocalName(mizarGlobalName.aid)
    val ln = LocalName(mizarGlobalName.kind+":"+mizarGlobalName.nr)
    TranslationController.currentThyBase ? theoryName ? ln
  }
  def addConstant(gn:info.kwarc.mmt.api.GlobalName, notC:NotationContainer, df: Option[objects.Term], tp:Option[objects.Term] = None) = {
    val hm : Term= OMMOD(gn.module).asInstanceOf[Term]
    val const = info.kwarc.mmt.api.symbols.Constant(OMMOD(gn.module), gn.name, Nil, tp, df, None, notC)
    TranslationController.add(const)
  }
  def conforms(A:Type, B:Type) : Boolean = {
    val List(a,b) = List(A,B) map typeTranslator.translate_Type
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
  def translateArguments(args: Arguments) : List[objects.Term] = {args._children map termTranslator.translate_Term }
  def translateObjRef(refObjAttrs:referencingObjAttrs)  = objects.OMS(MMLIdtoGlobalName(refObjAttrs.globalName()))
}