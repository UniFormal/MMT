package info.kwarc.mmt.mizar.newxml.mmtwrapper

import info.kwarc.mmt.api._
import symbols.DerivedDeclaration
import objects.{Term, _}
import info.kwarc.mmt.lf._
import MizSeq.{Index, OMI, Rep, Sequence, nTerms}
import info.kwarc.mmt.mizar.newxml._
import MizarPrimitiveConcepts._
import MMTUtils._
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.lf.structuralfeatures.RecordUtil.{makeName, recName, recTypeName}
import translator.{DefinitionContext, TranslationController}
object PatternUtils {
  def argsVarName: String = "x"//"argumentSequence"
  def argsWellTypedName: String = "p"
  def pseudoSlash(a: LocalName, b: LocalName) : LocalName = a / b //LocalName(a.toString+"_"+b.toString)
  private def referenceExtDecl(strPath:GlobalName, nm: LocalName): GlobalName = strPath.module ? pseudoSlash(strPath.name, LocalName(nm))
  def structureStrictDeclPath(implicit parentTerm: GlobalName): GlobalName = referenceExtDecl(parentTerm, LocalName("strictDef"))
  def structureForgetfulFunctorPath(implicit parentTerm: GlobalName): GlobalName = referenceExtDecl(parentTerm, LocalName("forgetfulFunctor"))
  def structureTypePath(implicit parentTerm: GlobalName): GlobalName = referenceExtDecl(parentTerm, LocalName(recTypeName))
  def structureMakePath(implicit parentTerm: GlobalName): GlobalName = referenceExtDecl(parentTerm, LocalName(makeName))
  def structureSelectorPath(selName: LocalName)(implicit parentTerm: GlobalName): GlobalName = referenceExtDecl(parentTerm, selName)
  def structureAncestorSubtypingPath(ancestorName: LocalName)(implicit parentTerm: GlobalName): GlobalName = referenceExtDecl(parentTerm, ancestorName)

  //effectively copied from InternalDeclarationUtil
  //TODO: Make it accessible and usable at one central location
  object PiOrEmpty {
    def apply(ctx: Context, body: Term): Term = if (ctx.isEmpty) body else Pi(ctx, body)
    def unapply(tm: Term) : Some[(Context, Term)] = Some(tm match {
      case Pi(n, tp, x) =>
        val PiOrEmpty(ctx, body) = x
        (OMV(n) % tp::ctx, body)
      case t => (Context.empty, t)
    })
  }
  object LambdaOrEmpty {
    def apply(ctx: Context, body: Term): Term = if (ctx.isEmpty) body else Lambda(ctx, body)
    def unapply(tm: Term) : Option[(Context, Term)] = Some(tm match {
      case Lambda(n, tp, x) =>
        val LambdaOrEmpty(ctx, body) = x
        (OMV(n) % tp::ctx, body)
      case t => (Context.empty, t)
    })
  }
  def lambdaBindArgs(tm: Term, bindEmpty: Boolean = false)(implicit args: List[Term]): Term = {
    if (args.nonEmpty) Lam(argsVarName, nTerms(args.length), tm) else tm
  }
  def lambdaBindContext(tm: Term, bindEmpty: Boolean = false)(implicit defContext: DefinitionContext): Term = {
    lambdaBindArgs(tm)(defContext.args.map(_.toTerm))
  }
  def piBindArgs(tm: Term, bindEmpty: Boolean = false)(implicit args: List[Term]): Term = {
    if (args.nonEmpty) Pi(LocalName(argsVarName), nTerms(args.length), tm) else tm
  }
  def piBindContext(tm: Term, bindEmpty: Boolean = false)(implicit defContext: DefinitionContext): Term = {
    piBindArgs(tm)(defContext.args.map(_.toTerm))
  }
}

import PatternUtils._

object StructureInstance {
  /**
   * Constructs a Mizar structure instance
   * @param declarationPath the path of the structure
   * @param l The number of arguments to the structure instance
   * @param argNameTps The names and types of the arguments to the structure instance
   * @param n The number of structure instances it extends
   * @param substr A list of ApplyGeneral(OMS(p), args), where p is the path of the struct type
   *               of another Mizar structure instance and args are values for its arguments
   * @param m the number of field declarations (selectors in Mizar)
   * @param fieldDecls The field declarations (selectors) of the structure,
   *                   inherited selectors must be repeated here
   */
  def apply(l:Int, argNameTps:Context, n:Int, substr:List[Term], m:Int, fieldDecls:List[VarDecl], notCons: List[NotationContainer])(implicit declarationPath: GlobalName): List[symbols.Constant] = {
    MizarStructure.elaborateAsMizarStructure(argNameTps, fieldDecls, substr, TranslationController.controller, notCons, Some(pseudoSlash(_, _)))
  }
  def withUnnamedArgs(l:Int, argTps:List[Term], n:Int, substr:List[Term], m:Int, fieldDecls:List[VarDecl])(implicit declarationPath: GlobalName): List[symbols.Declaration] = {
    val argNameTps = argTps.zipWithIndex.map {case (tp, ind) => OMV("arg"+ind) % tp}
    StructureInstance(l, argNameTps, n, substr, m, fieldDecls, NotationContainer.empty()::Nil)
  }
}

object MizarPatternInstance {
  def apply(name: LocalName, pat: String, args: List[Term])(implicit notC: NotationContainer): DerivedDeclaration = {
    val home: Term = OMMOD(TranslationController.currentTheoryPath)
    val pattern = MizarPatternsTh ? LocalName(pat)
    MizInstance.apply(home, name, pattern, args, notC)
  }

  private[mmtwrapper] def apply(name: LocalName, pat: String, argNumI: Int, argumentsUnbound: List[Term], furtherParams: List[Term])(implicit notC: NotationContainer): DerivedDeclaration = {
    val argNum = OMI(argNumI)
    implicit val args: List[Term] = argumentsUnbound.map(lambdaBindArgs(_)(argumentsUnbound))
    assert(args.length == argNumI)
    val parameters: List[Term] = argNum :: Sequence(args) :: furtherParams
    apply(name, pat, parameters)
  }

  def argsWellTyped(body: Term)(implicit args: List[Term]): Term = {
    val argNumI = args.length
    val x = OMV(argsVarName)
    Lambda(x.name, nTerms(argNumI), Lambda(LocalName(argsWellTypedName), Sequence((0 until argNumI).toList.map({ ind: Int =>
      proof(is(Index(x, OMI(ind)), Apply(Index(Sequence(args map (lambdaBindArgs(_)(args))), OMI(ind)), x)))
    })), body))
  }
  /**
   * Lambda-binds the passed parameters of all kinds over the argument
   * @param name the name of the instance
   * @param pat its pattern
   * @param argNumI the number of its arguments
   * @param arguments the types of its arguments
   * @param retO (optional) the return type of the pattern
   * @param motherTpUnbound (optional) the mother type of the pattern
   * @param caseNumI the number of cases of its definien
   * @param casesUnbound the cases of its definien
   * @param caseResUnbound the definition in each case of its definien
   * @param consistencyProofUnbound (optional) the consistency proof
   * @param defResUnbound (optional) the default definien
   * @param furtherProofs proofs for further correctness conditions
   * @param notC (implicit) the notation of the instance and its main declaration
   * @return
   */
  def apply(name: LocalName, pat: String, argNumI: Int, arguments: List[Term], retO: Option[Term], motherTpUnbound: Option[Term], caseNumI: Int, casesUnbound: List[Term], caseResUnbound: List[Term], consistencyProofUnbound: Option[Term], defResUnbound: Term, furtherProofs: List[Term] = Nil)(implicit notC: NotationContainer) : DerivedDeclaration = {
    assert(casesUnbound.length == caseNumI && caseResUnbound.length == caseNumI && argNumI == arguments.length)
    val caseNum = OMI(caseNumI)
    implicit val args = arguments
    val ret = retO map(List(_)) getOrElse Nil
    val motherType = motherTpUnbound map(tm => List(lambdaBindArgs(tm))) getOrElse Nil
    val cases = Sequence(casesUnbound map (lambdaBindArgs(_, true)))
    val caseRes = Sequence(caseResUnbound map (lambdaBindArgs(_, true)))
    val x = OMV(argsVarName)
    val consistencyProofU = (caseNumI, consistencyProofUnbound) match {
      case (0, _) => zeroAryAndPropCon
      case (_, Some(pf)) => pf
      case (m, _) => uses(And((0 until m).toList map (i => implies(
        Index(Apply(OMV("cases"), OMI(i)), x), And((i until m).toList map(j => implies(
          Index(Apply(OMV("cases"), OMI(j)), x), MizarPrimitiveConcepts.equal(
            Apply(Index(OMV("caseRes"), OMI(i)), x),
            Apply(Index(OMV("caseRes"), OMI(j)), x)))))))), Nil)
      case _ => throw ImplementationError("consistency correctness condition expected, but none given for "+pat+". ")
    }
    def caseResSingleTp(n: Int) = pat match {
      case s if s.contains("Func") && s.contains ("dir") => Arrow(nTerms(n), any)
      case s if s.contains("Func") && s.contains ("indir") => Arrow(nTerms(n+1), any)
      case s if s.contains("Pred") && s.contains ("dir") => Arrow(nTerms(n), prop)
      case s if s.contains("Pred") && s.contains ("indir") => Arrow(nTerms(n+1), prop)
      case s if s.contains("Attr") && s.contains ("dir") => Arrow(nTerms(n), prop)
      case s if s.contains("Attr") && s.contains ("indir") => Arrow(nTerms(n+1), prop)
      case s if s.contains("Mode") && s.contains ("dir") => Arrow(nTerms(n), Arrow(any, prop))
      case s if s.contains("Mode") && s.contains ("indir") => Arrow(nTerms(n), Arrow(Arrow(any, prop), prop))
    }
    val casesTp = Rep(Arrow(nTerms(argNumI), prop), caseNum)
    val caseResTp = Rep(caseResSingleTp(caseNumI), caseNum)

    val consistencyProof = argsWellTyped(consistencyProofU)
    val defRes = lambdaBindArgs(defResUnbound)
    val furtherParameters: List[Term] = ret ::: motherType ::: caseNum::cases::caseRes::consistencyProof::defRes::(furtherProofs map argsWellTyped)
    apply(name, pat, argNumI, arguments, furtherParameters)
  }
  /**
   * For schemeDef and nymicNotations
   */
  def apply(name: LocalName, pat: String, argNumI: Int, arguments: List[Term], assNumI: Option[Int], ass: List[Term], pred: Term, proofU: Option[Term])(implicit notC: NotationContainer) : DerivedDeclaration = {
    if (assNumI.isDefined) {assert(assNumI.get == ass.length)}
    implicit val args = arguments
    val assumptions = assNumI map(tm => List(OMI(tm), Sequence(ass map(lambdaBindArgs(_, true))))) getOrElse Nil
    val v = lambdaBindArgs(pred)
    val proof = proofU map(tm => List(lambdaBindArgs(tm))) getOrElse Nil
    val furtherParameters: List[Term] = assumptions:::v::proof
    apply(name, pat, argNumI, arguments, furtherParameters)
  }
  /**
   * For registrations of all kind
   * @param name
   * @param pat
   * @param arguments
   * @param tpU
   * @param tmO
   * @param attrAssU
   * @param attrConclU
   * @param notC
   * @return
   */
  def apply(name: LocalName, pat: String, arguments: List[Term], tpU: Term, tmO: Option[Term], attrAssU: List[Term], attrConclU: List[Term], corCondProof: Term)(implicit notC: NotationContainer) : DerivedDeclaration = {
    implicit val args: List[Term] = arguments
    val argNumI = args.length
    val attrAssNum = OMI(attrAssU.length)
    val tp = lambdaBindArgs(tpU)
    val tm = tmO map(tm => List(lambdaBindArgs(tm))) getOrElse Nil
    val attrAss = Sequence(attrAssU map (lambdaBindArgs(_, true)))
    val attrConcl = if (attrConclU.isEmpty) Nil else {
      List(OMI(attrConclU.length), Sequence(attrConclU map (lambdaBindArgs(_, true))))}

    val furtherParameters: List[Term] = attrAssNum::tp::tm:::attrAss::attrConcl.:+(corCondProof)
    apply(name, pat, argNumI, arguments, furtherParameters)
  }
  def unapply(mizInstance: DerivedDeclaration): Option[(LocalName, String, List[Term])] = mizInstance match {
    case MizInstance(home, name, pattern, args, notC) if pattern.module == MizarPatternsTh =>
      Some(name, pattern.name.toString, args)
    case _ => None
  }
  def mainDeclO(dd: DerivedDeclaration): Option[GlobalName] = dd match {
    case MizarPatternInstance(ln, pat, _) =>
      Some(TranslationController.currentTheoryPath ? ln / LocalName(pat match {
      case s if s.toLowerCase contains "func" => "func"
      case s if s.toLowerCase contains "pred" => "pred"
      case s if s.toLowerCase contains "attr" => "attribute"
      case s if s.toLowerCase contains "mode" => "mode"
      case s if s.toLowerCase contains "schemeDef" => "scheme"
      case s if s.toLowerCase contains "notation" => "notation"
      case s if s.toLowerCase contains "registration" => "registration"
      case s if s.toLowerCase contains "identify" => "registration"
    }))
    case _ => None
  }
}

sealed trait FunctorDefinitionInstance
object FunctorDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Option[Term], Term)] =
    DirectFunctorDefinition.unapply(dd) orElse IndirectFunctorDefinition.unapply(dd)
}
object DirectFunctorDefinition extends FunctorDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Term, coherenceProof: Term)(implicit notC: NotationContainer): DerivedDeclaration = {
    MizarPatternInstance(name, "directFuncDef", argNum, argTypes, Some(ret), None, caseNum, cases, caseRes, Some(consistencyProof), defRes, List(coherenceProof))
  }
  def unapply(dd: DerivedDeclaration): Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Some[Term], Term)] = dd match {
    case MizarPatternInstance(name, "directFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases), Sequence(caseRes), consistencyProof, defRes, _)) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes, Some(consistencyProof), defRes))
    case _ => None
  }
}
object IndirectFunctorDefinition extends FunctorDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], ret: Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Term, existenceProof: Term, uniquenessProof: Term)(implicit notC: NotationContainer): DerivedDeclaration = {
    MizarPatternInstance(name, "indirectFuncDef", argNum, argTypes, Some(ret), None, caseNum, cases, caseRes, Some(consistencyProof), defRes, List(existenceProof, uniquenessProof))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Some[Term], Term)] = dd match {
    case MizarPatternInstance(name, "indirectFuncDef", List(OMI(argNum), Sequence(argTypes), ret, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes, _, _)) =>
      Some((name, argNum, argTypes, ret, caseNum, cases, caseRes, Some(consistencyProof), defRes))
    case _ => None
  }
}

trait PredicateDefinitionInstance
object PredicateDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = DirectPredicateDef.unapply(dd)
}
object DirectPredicateDef extends PredicateDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Term)(implicit notC: NotationContainer) = {
    MizarPatternInstance(name, "directPredDef", argNum, argTypes, None, None, caseNum, cases, caseRes, Some(consistencyProof), defRes)
    }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "directPredDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}

trait AttributeDefinitionInstance
object AttributeDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] =
    DirectAttributeDefinition.unapply(dd) orElse IndirectAttributeDefinition.unapply(dd)
}
object DirectAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Term)(implicit notC: NotationContainer) = {
    MizarPatternInstance(name, "directAttrDef", argNum, argTypes, None, Some(motherTp), caseNum, cases, caseRes, Some(consistencyProof), defRes)
    }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, feature, args) if "directAttrDef" == feature =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}
object IndirectAttributeDefinition extends AttributeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], motherTp:Term, caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Term)(implicit notC: NotationContainer) = {
    MizarPatternInstance(name, "indirectAttrDef", argNum, argTypes, None, Some(motherTp), caseNum, cases, caseRes, Some(consistencyProof), defRes)
  }
  def unapply(dd: DerivedDeclaration): Option[(LocalName, Int, List[Term], Term, Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, feature, args) if ("indirectAttrDef" == feature) =>
      val List(OMI(argNum), Sequence(argTypes), motherTp, OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes) = args
      Some((name, argNum, argTypes, motherTp, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}

trait ModeDefinitionInstance
object ModeDefinitionInstance {
  def unapply(dd: DerivedDeclaration): Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] =
    DirectModeDefinition.unapply(dd) orElse IndirectModeDefinition.unapply(dd)
}
object DirectModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Term, existenceProof: Term)(implicit notC: NotationContainer) = {
    MizarPatternInstance(name, "directModeDef", argNum, argTypes, None, None, caseNum, cases, caseRes, Some(consistencyProof), defRes, List(existenceProof))
  }
  def unapply(dd: DerivedDeclaration): Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "directModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes, _)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}
object IndirectModeDefinition extends ModeDefinitionInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], caseNum:Int, cases:List[Term], caseRes: List[Term], defRes:Term, consistencyProof: Term, existenceProof: Term)(implicit notC: NotationContainer) = {
    MizarPatternInstance(name, "indirectModeDef", argNum, argTypes, None, None, caseNum, cases, caseRes, Some(consistencyProof), defRes, List(existenceProof))
  }
  def unapply(dd: DerivedDeclaration) : Option[(LocalName, Int, List[Term], Int, List[Term], List[Term], Term)] = dd match {
    case MizarPatternInstance(name, "indirectModeDef", List(OMI(argNum), Sequence(argTypes), OMI(caseNum), Sequence(cases),Sequence(caseRes), consistencyProof, defRes, _)) =>
      Some((name, argNum, argTypes, caseNum, cases, caseRes, defRes))
    case _ => None
  }
}

object SchemeDefinitionInstance {
  def apply(name: LocalName, argTypes: List[Term], assumptions:List[Term], p:Term, prf: Term)(implicit notC: NotationContainer) = {
    MizarPatternInstance(name, "schemeDef", argTypes.length, argTypes, Some(assumptions.length), assumptions, p, Some(prf))
  }
}

trait RegistrationInstance
object ExistentialRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, atrs:List[Term], existProof: Term)(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "existRegistration", argTypes, tp, None, atrs, Nil, existProof)
  }
}
object ConditionalRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, atrs:List[Term], ats:List[Term], coherenceProof: Term)(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "condRegistration", argTypes, tp, None, atrs, ats, coherenceProof)
  }
}
object UnqualifiedFunctorRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, tm:Term, atrs:List[Term], coherenceProof: Term)(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "unqualFuncRegistration", argTypes, tp, Some(tm), atrs, Nil, coherenceProof)
  }
}
object QualifiedFunctorRegistration extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], tp:Term, tm:Term, atrs:List[Term], coherenceProof: Term)(implicit notC: NotationContainer = NotationContainer.empty()) = {
    MizarPatternInstance(name, "qualFuncRegistration", argTypes, tp, Some(tm), atrs, Nil, coherenceProof)
  }
}
object Identify extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], eqns: List[(Term, Term)], expr1:Term, expr2: Term, compatibility: Term)(implicit notC: NotationContainer = NotationContainer.empty()) = {
    val (asU, bsU) = eqns unzip

    implicit val args: List[Term] = argTypes
    val argNumI = args.length
    val eqnNum = OMI(eqns.length)
    val fstExpr = lambdaBindArgs(expr1)
    val sndExpr = lambdaBindArgs(expr2)
    val as = Sequence(asU map (lambdaBindArgs(_, true)))
    val bs = Sequence(bsU map (lambdaBindArgs(_, true)))

    val furtherParameters: List[Term] = List(eqnNum, as, bs, fstExpr, sndExpr, compatibility)
    MizarPatternInstance(name, "identify", argNumI, argTypes, furtherParameters)
  }
}
object Reduction extends RegistrationInstance {
  def apply(name: LocalName, argTypes: List[Term], predecessor:Term, successor: Term, reducibility: Term)(implicit notC: NotationContainer = NotationContainer.empty()) = {
    implicit val args: List[Term] = argTypes
    val argNumI = args.length
    val a = lambdaBindArgs(predecessor)
    val b = lambdaBindArgs(successor)

    MizarPatternInstance(name, "reduce", argNumI, argTypes, List(a, b, reducibility))
  }
}

trait NotationInstance
class NymicNotation(nymic:String) extends NotationInstance {
  def apply(name: LocalName, argNum: Int, argTypes: List[Term], v: Term)(implicit notC: NotationContainer, kind: String) = {
    val fullPatternName = nymic+(if (kind == "func")  "Functor" else if (kind == "pred") "Predicate" else "")+"Notation"
    MizarPatternInstance(name, fullPatternName, argNum, argTypes, List(v))
  }
}
object NymicNotation {
  def unapply(dd: DerivedDeclaration): Option[(LocalName, String, Int, List[Term], Term)] = dd match {
    case MizarPatternInstance(name, key, List(OMI(n), Sequence(argTypes), v)) => Some((name, key, n, argTypes, v))
    case _ => None
  }
}
object SynonymicNotation extends NymicNotation("synonymic") {
  def unapply(dd: DerivedDeclaration): Option[(LocalName, String, Int, List[Term], Term)] = dd match {
    case NymicNotation(name, "synonymicNotation", n, argTypes, v) => Some((name, "synonymicNotation", n, argTypes, v))
    case _ => None
  }
}
object AntonymicNotation extends NymicNotation("antonymic") {
  def unapply(dd: DerivedDeclaration): Option[(LocalName, String, Int, List[Term], Term)] = dd match {
    case NymicNotation(name, "antonymicNotation", n, argTypes, v) => Some((name, "antonymicNotation", n, argTypes, v))
    case _ => None
  }
}