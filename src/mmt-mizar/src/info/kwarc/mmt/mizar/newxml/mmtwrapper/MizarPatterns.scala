package info.kwarc.mmt.mizar.newxml.mmtwrapper

import info.kwarc.mmt.api._
import symbols.{DerivedDeclaration, ParametricTheoryLike}
import objects._
import info.kwarc.mmt.lf._
import structuralfeatures.StructuralFeatureUtils
import MizSeq.{Ellipsis, Index, OMI, Rep, Sequence, nTerms, nTypes}
import info.kwarc.mmt.mizar.newxml._
import MMTUtils._
import info.kwarc.mmt.api.notations.NotationContainer
import translator.{TranslationController, TranslatorUtils}

object PatternUtils {
  def pseudoSlash(a: LocalName, b: LocalName) : LocalName = LocalName(a.toString+"_"+b.toString)
  def pseudoSlash(a: LocalName, b: String) : LocalName = pseudoSlash(a, LocalName(b))
  def structureStrictDeclName(implicit parentTerm: GlobalName) = pseudoSlash(parentTerm.name, LocalName("strictDef"))
  def structureStrictPropName(implicit parentTerm: GlobalName) = pseudoSlash(parentTerm.name, LocalName("strictProp"))
  def structureDefRestrName(substrName:String)(implicit parentTerm: GlobalName) = pseudoSlash(LocalName("restr"), substrName)
  def structureDefSubstrSelPropName(restrName:LocalName, sel: LocalName)(implicit parentTerm: GlobalName) = pseudoSlash(pseudoSlash(LocalName(restrName), "selProp"), sel)
  def referenceExtDecl(substrPath:GlobalName, nm: String) = OMS(substrPath.module ? pseudoSlash(substrPath.name, LocalName(nm)))
  def referenceIntSel(strName: String, nm: String) = {
    val strPath = TranslationController.currentTheoryPath ? strName
    referenceExtDecl(strPath, nm)
  }

  //effectively copied from InternalDeclarationUtil
  //TODO: Make it accessible and usable at one central location
  object PiOrEmpty {
    def apply(ctx: Context, body: Term) = if (ctx.isEmpty) body else Pi(ctx, body)
    def unapply(tm: Term) : Option[(Context, Term)] = Some(tm match {
      case Pi(n, tp, x) =>
        val PiOrEmpty(ctx, body) = x
        (OMV(n) % tp::ctx, body)
      case t => (Context.empty, t)
    })
  }
  object LambdaOrEmpty {
    def apply(ctx: Context, body: Term) = if (ctx.isEmpty) body else Lambda(ctx, body)
    def unapply(tm: Term) : Option[(Context, Term)] = Some(tm match {
      case Lambda(n, tp, x) =>
        val LambdaOrEmpty(ctx, body) = x
        (OMV(n) % tp::ctx, body)
      case t => (Context.empty, t)
    })
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
  def apply(declarationPath:GlobalName, l:Int, argNameTps:Context, n:Int, substr:List[Term], m:Int, fieldDecls:List[VarDecl], notationC: NotationContainer = NotationContainer.empty()): List[symbols.Declaration] = {
    MizarStructure.elaborateAsMizarStructure(argNameTps,fieldDecls,substr,TranslationController.controller, notationC, Some(pseudoSlash(_, _)))(declarationPath)
  }
  def withUnnamedArgs(declarationPath:GlobalName, l:Int, argTps:List[Term], n:Int, substr:List[Term], m:Int, fieldDecls:List[VarDecl]): List[symbols.Declaration] = {
    val argNameTps = argTps.zipWithIndex.map {case (tp, ind) => OMV("x"+ind) % tp}
    StructureInstance(declarationPath:GlobalName, l, argNameTps, n, substr, m, fieldDecls)
  }
}

object MizarPatternInstance {
  def apply(name: LocalName, pat: String, args: List[Term])(implicit notC: NotationContainer) : DerivedDeclaration = {
    val home : Term = OMMOD(TranslationController.currentTheoryPath)
    val pattern = Mizar.MizarPatternsTh ? LocalName(pat)
    MizInstance.apply(home, name, pattern, args, notC)
  }
  private def lambdaBindArgs(tm: Term)(implicit args: List[Term]): Term = {
    val LambdaOrEmpty(ctx, body) = tm
    if (args.length > 0) tm else Lam("x", nTypes(args.length), tm)
  }
  private def apply(name: LocalName, pat: String, argNumI: Int, argumentsUnbound: List[Term], furtherParams: List[Term])(implicit notC: NotationContainer) : DerivedDeclaration = {
    val argNum = OMI(argNumI)
    implicit val args = argumentsUnbound.map(lambdaBindArgs(_)(argumentsUnbound))
    assert(args.length == argNumI)
    val parameters: List[Term] = argNum::Sequence(args)::furtherParams
    apply(name, pat, parameters)
  }
  /**
   * Lambda-binds the passed parameters of all kinds over the argument
   * @param name
   * @param pat
   * @param args
   * @param notC
   * @return
   */
  def apply(name: LocalName, pat: String, argNumI: Int, arguments: List[Term], retO: Option[Term], motherTpUnbound: Option[Term], caseNumI: Int, casesUnbound: List[Term], caseResUnbound: List[Term], consistencyProofUnbound: Option[Term], defResUnbound: Option[Term])(implicit notC: NotationContainer) : DerivedDeclaration = {
    assert(casesUnbound.length == caseNumI && caseResUnbound.length == caseNumI)
    val caseNum = OMI(caseNumI)
    implicit val args = arguments
    val ret = retO map(List(_)) getOrElse Nil
    val motherType = motherTpUnbound map(tm => List(lambdaBindArgs(tm))) getOrElse Nil
    val cases = Sequence(casesUnbound map lambdaBindArgs)
    val caseRes = Sequence(caseResUnbound map lambdaBindArgs)
    val x = OMV("x")
    val consistencyProofU = (caseNumI, consistencyProofUnbound) match {
      case (0, _) => Mizar.zeroAryAndPropCon
      case (_, Some(pf)) => pf
      case (n, _) => Mizar.uses(Mizar.And((1 to n).toList map (i => Mizar.implies(
        Apply(MizSeq.Index(OMV("cases"), OMI(i)), x), Mizar.And((i to n).toList map(j => Mizar.implies(
          Apply(MizSeq.Index(OMV("cases"), OMI(j)), x), Mizar.eq(
            Apply(MizSeq.Index(OMV("caseRes"), OMI(i)), x),
            Apply(MizSeq.Index(OMV("caseRes"), OMI(j)), x)))))))), Nil)
      case _ => throw ImplementationError("consistency correctness condition expected, but none given for "+pat+". ")
    }
    val consistencyProof = Pi(x.name, Rep(Mizar.any), Lam("argsWellTyped", Sequence(args map {arg =>
      Mizar.proof(Mizar.is(MizSeq.Index(x, OMV("i")), Apply(MizSeq.Index(Sequence(args), OMV("i")), x)))}), consistencyProofU))
    val defRes = defResUnbound map(tm => List(lambdaBindArgs(tm))) getOrElse Nil
    val furtherParameters: List[Term] = ret ++ motherType ++ (caseNum::cases::caseRes::consistencyProof::defRes)
    apply(name, pat, argNumI, arguments, furtherParameters)
  }
  /**
   * For schemeDef and nymicNotations
   * @param name
   * @param pat
   * @param argNumI
   * @param arguments
   * @param notC
   * @return
   */
  def apply(name: LocalName, pat: String, argNumI: Int, arguments: List[Term], assNumI: Option[Int], ass: List[Term], pred: Term, proofU: Option[Term] = None)(implicit notC: NotationContainer) : DerivedDeclaration = {
    if (assNumI.isDefined) {assert(assNumI.get == ass.length)}
    implicit val args = arguments
    val assumptions = assNumI map(tm => List(OMI(tm), Sequence(ass map lambdaBindArgs))) getOrElse Nil
    val v = lambdaBindArgs(pred)
    val proof = proofU map(tm => List(lambdaBindArgs(tm))) getOrElse Nil
    val furtherParameters: List[Term] = assumptions:::v::proof
    apply(name, pat, argNumI, arguments, furtherParameters)
  }
  /**
   * For registrations of all kind
   * @param name
   * @param pat
   * @param argNumI
   * @param arguments
   * @param attrAssNumI
   * @param tpU
   * @param tmO
   * @param attrAssU
   * @param attrConclU
   * @param notC
   * @return
   */
  def apply(name: LocalName, pat: String, arguments: List[Term], tpU: Term, tmO: Option[Term], attrAssU: List[Term], attrConclU: List[Term])(implicit notC: NotationContainer) : DerivedDeclaration = {
    implicit val args = arguments
    val argNumI = args.length
    val attrAssNum = OMI(attrAssU.length)
    val tp = lambdaBindArgs(tpU)
    val tm = tmO map(tm => List(lambdaBindArgs(tm))) getOrElse Nil
    val attrAss = Sequence(attrAssU map lambdaBindArgs)
    val attrConcl = if (attrConclU.isEmpty) Nil else {
      List(OMI(attrConclU.length), Sequence(attrConclU map lambdaBindArgs))}

    val furtherParameters: List[Term] = attrAssNum::tp::tm:::attrAss::attrConcl
    apply(name, pat, argNumI, arguments, furtherParameters)
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
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "directPartFuncDef", argNum, argTypes, Some(ret), None, caseNum, cases, caseRes, consistencyProof, Some(defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term, Term)] = dd match {
    case MizarPatternInstance(name, "directPartFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes)) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes, consistencyProof, defRes))
    case _ => None
  }
}
object indirectPartialFunctorDefinition extends functorDefInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "directPartFuncDef", argNum, argTypes, Some(ret), None, caseNum, cases, caseRes, consistencyProof, Some(defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term, Term)] = dd match {
    case MizarPatternInstance(name, "indirectPartFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes)) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes, consistencyProof, defRes))
    case _ => None
  }
}
object directCompleteFunctorDefinition extends functorDefInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "directComplFuncDef", argNum, argTypes, Some(ret), None, caseNum, cases, caseRes, consistencyProof, None)
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "directComplFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof)) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes, consistencyProof))
    case _ => None
  }
}
object indirectCompleteFunctorDefinition extends functorDefInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "indirectComplFuncDef", argNum, argTypes, Some(ret), None, caseNum, cases, caseRes, consistencyProof, None)
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "indirectComplFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof)) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes, consistencyProof))
    case _ => None
  }
}

trait PredicateDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[Product]
}
object directPartialPredicateDef extends PredicateDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "directPartPredDef", argNum, argTypes, None, None, caseNum, cases, caseRes, consistencyProof, Some(defRes))
    }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term, Term)] = dd match {
    case MizarPatternInstance(name, "directPartPredDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, consistencyProof, defRes))
    case _ => None
  }
}
object directCompletePredicateDef extends PredicateDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "directPartPredDef", argNum, argTypes, None, None, caseNum, cases, caseRes, consistencyProof, None)
    }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "directComplPredDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, consistencyProof))
    case _ => None
  }
}

trait AttributeDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[Product]
}
object directCompleteAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "directComplAttrDef", argNum, argTypes, None, Some(motherTp), caseNum, cases, caseRes, consistencyProof, None)
    }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, feature, args) if ("directComplAttrDef" == feature) =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes, consistencyProof))
    case _ => None
    }
}
object directPartialAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "directPartAttrDef", argNum, argTypes, None, Some(motherTp), caseNum, cases, caseRes, consistencyProof, Some(defRes))
    }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term, Term)] = dd match {
    case MizarPatternInstance(name, feature, args) if ("directPartAttrDef" == feature) =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes, consistencyProof, defRes))
    case _ => None
  }
}
object indirectCompleteAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "indirectComplAttrDef", argNum, argTypes, None, Some(motherTp), caseNum, cases, caseRes, consistencyProof, None)
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, feature, args) if ("indirectComplAttrDef" == feature) =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes, consistencyProof))
    case _ => None
  }
}
object indirectPartialAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "indirectPartAttrDef", argNum, argTypes, None, Some(motherTp), caseNum, cases, caseRes, consistencyProof, Some(defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term, Term)] = dd match {
    case MizarPatternInstance(name, feature, args) if ("indirectPartAttrDef" == feature) =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes, consistencyProof, defRes))
    case _ => None
  }
}

trait ModeDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[Product]

}
object directPartialModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "directPartModeDef", argNum, argTypes, None, None, caseNum, cases, caseRes, consistencyProof, Some(defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term, Term)] = dd match {
    case MizarPatternInstance(name, "directPartModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, consistencyProof, defRes))
    case _ => None
  }
}
object directCompleteModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "directComplModeDef", argNum, argTypes, None, None, caseNum, cases, caseRes, consistencyProof, None)
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "directComplModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases), Sequence(caseRes), consistencyProof)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, consistencyProof))
    case _ => None
  }
}
object indirectPartialModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "indirectPartModeDef", argNum, argTypes, None, None, caseNum, cases, caseRes, consistencyProof, Some(defRes))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term, Term)] = dd match {
    case MizarPatternInstance(name, "indirectPartModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, consistencyProof, defRes))
    case _ => None
  }
}
object indirectCompleteModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], consistencyProof: Option[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "indirectComplModeDef", argNum, argTypes, None, None, caseNum, cases, caseRes, consistencyProof, None)
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "indirectComplModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, consistencyProof))
    case _ => None
  }
}

object schemeDefinitionInstance {
  def apply(name: LocalName, argTypes: List[Term], assumptions:List[Term], p:Term, prf: Option[Term] = None)(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "schemeDef", argTypes.length, argTypes, Some(assumptions.length), assumptions, p, prf)
  }
}

trait RegistrationInstance
object existentialRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, atrs:List[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "existRegistration", argTypes, tp, None, atrs, Nil)
  }
}
object conditionalRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, atrs:List[Term], ats:List[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "condRegistration", argTypes, tp, None, atrs, ats)
  }
}
object unqualifiedFunctorRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, tm:Term, atrs:List[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "unqualFuncRegistration", argTypes, tp, Some(tm), atrs, Nil)
  }
}
object qualifiedFunctorRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, tm:Term, atrs:List[Term])(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "qualFuncRegistration", argTypes, tp, Some(tm), atrs, Nil)
  }
}

trait NotationInstance
class NymicNotation(key:String) extends NotationInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], v: Term)(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, key, argNum, argTypes, None, Nil, v, None)
  }
}
object synonymicNotation extends NymicNotation("synonymicNotation")
object antonymicNotation extends NymicNotation("antonymicNotation")