package info.kwarc.mmt.mizar.newxml.mmtwrapper

import info.kwarc.mmt.api._
import symbols.{DerivedDeclaration, ParametricTheoryLike}
import objects._
import patterns.PatternFeature
import uom.FlexaryConstantScala
import info.kwarc.mmt.lf._
import structuralfeatures.StructuralFeatureUtils
import MizSeq.{Ellipsis, OMI, Rep, Sequence, nTerms}
import info.kwarc.mmt.mizar.newxml._
import MMTUtils._
import info.kwarc.mmt.api.notations.NotationContainer
import translator.{TranslationController, TranslatorUtils}
import info.kwarc.mmt.sequences.NatRules

object PatternUtils {
  def structureStrictDeclName(implicit parentTerm: GlobalName) = parentTerm.name / LocalName("strictDef")
  def structureStrictPropName(implicit parentTerm: GlobalName) = parentTerm.name / LocalName("strictProp")
  def structureDefRestrName(substrName:String)(implicit parentTerm: GlobalName) = parentTerm.name / LocalName("restr") / substrName
  def structureDefSubstrSelPropName(restrName:LocalName, sel: LocalName)(implicit parentTerm: GlobalName) = parentTerm.name / LocalName(restrName) / "selProp" / sel
  def referenceExtDecl(substrPath:GlobalName, nm: String) = OMS(StructuralFeatureUtils.externalName(substrPath,LocalName(nm)))
  def referenceIntSel(strName: String, nm: String) = {
    val strPath = TranslationController.currentTheoryPath ? strName
    referenceExtDecl(strPath, nm)
  }
}

import PatternUtils._

object StructureInstance {
  /**
   * Constructs a Mizar structure instance
   * @param name
   * @param l The number of arguments to the structure instance
   * @param argTps The types of the arguments to the structure instance
   * @param n The number of structure instances it extends
   * @param substr A list of OMMOD(p), where p is the mpath of a derived declaration dd
   *               of another Mizar structure instance
   * @param m the number of field declarations (selectors in Mizar)
   * @param fieldDecls The field declarations (selectors) of the structure,
   *                   inherited selectors must be repeated here
   */
  def apply(declarationPath:GlobalName, l:Int, argNameTps:Context, n:Int, substr:List[Term], m:Int, fieldDecls:List[VarDecl]): List[symbols.Declaration] = {
    MizarStructure.elaborateAsMizarStructure(argNameTps,fieldDecls,substr,TranslationController.controller)(declarationPath)
  }
  def withUnnamedArgs(declarationPath:GlobalName, l:Int, argTps:List[Term], n:Int, substr:List[Term], m:Int, fieldDecls:List[VarDecl]): List[symbols.Declaration] = {
    val argNameTps = argTps.zipWithIndex.map {case (tp, ind) => OMV("x"+ind) % tp}
    StructureInstance(declarationPath:GlobalName, l, argNameTps, n, substr, m, fieldDecls)
  }
}

object MizarPatternInstance {
  def apply(name: LocalName, pat: String, args: List[Term], notC: NotationContainer = NotationContainer.empty()) : DerivedDeclaration = {
    val home : Term = OMMOD(TranslationController.currentTheoryPath)
    val pattern = Mizar.MizarPatternsTh ? LocalName(pat)
    MizInstance.apply(home, name, pattern, args, notC)
  }
  def unapply(mizInstance: DerivedDeclaration): Option[(LocalName, String, List[Term])] = mizInstance match {
    case MizInstance(home, name, pattern, args, notC) if pattern.module == Mizar.MizarPatternsTh =>
      Some(name, pattern.name.toString, args)
    case _ => None
  }
}

trait functorDefInstance {
  def unapply(dd: DerivedDeclaration): Option[Product]
}
object directPartialFunctorDefinition extends functorDefInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directPartFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "directPartFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes)) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}
object indirectPartialFunctorDefinition extends functorDefInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectPartFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "indirectPartFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes)) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}
object directCompleteFunctorDefinition extends functorDefInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directComplFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes)))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term])] = dd match {
    case MizarPatternInstance(name, "directComplFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes))) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes))
    case _ => None
  }
}
object indirectCompleteFunctorDefinition extends functorDefInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectComplFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes)))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term])] = dd match {
    case MizarPatternInstance(name, "indirectComplFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes))) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes))
    case _ => None
  }
}

trait PredicateDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[Product]
}
object directPartialPredicateDef extends PredicateDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directPartPredDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "directPartPredDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}
object directCompletePredicateDef extends PredicateDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directComplPredDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes)))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term])] = dd match {
    case MizarPatternInstance(name, "directComplPredDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes))) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes))
    case _ => None
  }
}

trait AttributeDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[Product]
}
object directCompleteAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directComplAttrDef", List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes)))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term])] = dd match {
    case MizarPatternInstance(name, feature, args) if ("directComplAttrDef" == feature) =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes)) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes))
    case _ => None
    }
}
object directPartialAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directPartAttrDef", List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, feature, args) if ("directPartAttrDef" == feature) =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}
object indirectCompleteAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectComplAttrDef", List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes)))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term])] = dd match {
    case MizarPatternInstance(name, feature, args) if ("indirectComplAttrDef" == feature) =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes)) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes))
    case _ => None
  }
}
object indirectPartialAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectPartAttrDef", List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, feature, args) if ("indirectPartAttrDef" == feature) =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}

trait ModeDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[Product]

}
object directPartialModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directPartModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "directPartModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}
object directCompleteModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "directComplModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes)))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term])] = dd match {
    case MizarPatternInstance(name, "directComplModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes))) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes))
    case _ => None
  }
}
object indirectPartialModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectPartModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "indirectPartModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), defRes)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}
object indirectCompleteModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(cases.length == caseNum && caseRes.length == caseNum)
    MizarPatternInstance(name, "indirectComplModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes)))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term])] = dd match {
    case MizarPatternInstance(name, "indirectComplModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes))) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes))
    case _ => None
  }
}

object schemeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], assNum:Int, assumptions:List[Term], p:Term, notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    assert(assumptions.length == assNum)
    MizarPatternInstance(name, "schemeDef", List(OMI(argNum), Sequence(argTypes), OMI(assNum), Sequence(assumptions), p))
  }
}

trait RegistrationInstance
object existentialRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, atrs:List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    val argNum = argTypes.length
    val atrNum = atrs.length
    MizarPatternInstance(name, "existRegistration", List(OMI(argNum),Sequence(argTypes),OMI(atrNum),tp,Sequence(atrs)))
  }
}
object conditionalRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, atrs:List[Term], ats:List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    val argNum = argTypes.length
    val atrNum = atrs.length
    val atNum = ats.length
    MizarPatternInstance(name, "condRegistration", List(OMI(argNum),Sequence(argTypes),OMI(atrNum),tp,Sequence(atrs),Sequence(ats)))
  }
}
object unqualifiedFunctorRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, tm:Term, atrs:List[Term]) = {
    val argNum = argTypes.length
    val atrNum = atrs.length
    MizarPatternInstance(name, "unqualFuncRegistration", List(OMI(argNum),Sequence(argTypes),OMI(atrNum),tp,tm, Sequence(atrs)))
  }
}
object qualifiedFunctorRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, tm:Term, atrs:List[Term], notC: NotationContainer = NotationContainer.empty()) = {
    val argNum = argTypes.length
    val atrNum = atrs.length
    MizarPatternInstance(name, "qualFuncRegistration", List(OMI(argNum),Sequence(argTypes),OMI(atrNum),tp,tm, Sequence(atrs)))
  }
}

trait NotationInstance
class NymicNotation(key:String) extends NotationInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], v: Term, notC: NotationContainer = NotationContainer.empty()) = {
    assert(argTypes.length == argNum)
    MizarPatternInstance(name, key, List(OMI(argNum),Sequence(argTypes),v))
  }
}
object synonymicNotation extends NymicNotation("synonymicNotation")
object antonymicNotation extends NymicNotation("antonymicNotation")