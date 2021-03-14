package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api._
import notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mizar.newxml._
import mmtwrapper.{FunctorDefInstance, _}
import MMTUtils._
import MizarPrimitiveConcepts._
import syntax._
import Utils._
import expressionTranslator._
import termTranslator._
import typeTranslator._
import contextTranslator._
import formulaTranslator._
import TranslatorUtils._
import claimTranslator._
import definitionTranslator.{translate_Redefine, _}
import blockTranslator._
import info.kwarc.mmt.api.symbols.{Constant, Declaration, HasDefiniens, HasNotation, HasType}
import info.kwarc.mmt.mizar.newxml.mmtwrapper.PatternUtils.{LambdaOrEmpty, PiOrEmpty}
import info.kwarc.mmt.mizar.newxml.syntax.Utils._
import clusterTranslator._
import definiensTranslator._
import info.kwarc.mmt.mizar.newxml.mmtwrapper.MizSeq.OMI
import info.kwarc.mmt.mizar.newxml.translator.JustifiedCorrectnessConditions.translate_consistency
import info.kwarc.mmt.mizar.newxml.translator.TranslationController.{addUnresolvedDependency, controller, currentAid, getAnonymousTheoremCount, getIdentifyCount, getUnresolvedDependencies, incrementAndGetAnonymousTheoremCount, incrementAndGetIdentifyCount, inferType, localPath, makeConstant, makeConstantInContext}
import info.kwarc.mmt.mizar.newxml.translator.statementTranslator.translate_Choice_Statement
import justificationTranslator._
import propertyTranslator._
import nymTranslator._
import patternTranslator._

case class JustifiedCorrectnessConditions(correctness_Condition: CorrectnessConditions, just: Option[Justification])
object JustifiedCorrectnessConditions {
  def translate(corConds: List[CorrectnessConditions], just: Option[Justification])(implicit defContext: DefinitionContext): List[JustifiedCorrectnessConditions] = {
    corConds map (JustifiedCorrectnessConditions(_, just))
  }
  def translate_consistency(just: Option[Justification], argTps: List[Term], cases: List[Term], caseRes: List[Term], dir: Boolean, kind : String)(implicit defContext: DefinitionContext) = {
    val sort = kind match {
        case "func" => "Tmres"
        case "pred" => "Propres"
        case "attr" => "Propres"
        case "mode" => "Moderes"
      }
    val tp = if (!dir && kind == "mode") consistencyTp(argTps, cases, caseRes, dir, sort) else Univ(1)
    just flatMap(translate_Justification(_, tp))
  }
}

case class DefinitionContext(var args: Context = Context.empty, var assumptions: List[Term] = Nil, var usedFacts: List[(Term, Option[Term])] = Nil, corr_conds: List[JustifiedCorrectnessConditions] = Nil, props: List[MizarProperty] = Nil) {
  // nested proof level, 0 if not within a proof
  private var proofLevel = 0
  private var localDefinitions: List[List[(LocalName, Term)]] = Nil
  // binding variables (of new binders) within the current proof (and its parents)
  private var localBindingVars: List[Context] = List(Context.empty)
  def addLocalDefinition(n: LocalName, defn: Term) = {
    assert (withinProof)
    val toAdd = (n, defn)
    localDefinitions = localDefinitions.head.:+(toAdd) :: localDefinitions.tail
  }
  def lookupLocalDefinitionWithinSameProof(n: LocalName): Option[Term] = {
    if (withinProof) localDefinitions.flatMap(_.find(_._1 == n)).headOption.map(_._2) else None
  }
  /**
   * A a binding variable with scope the current proof
   * @param vd
   */
  def addLocalBindingVar(vd: VarDecl): Unit = { localBindingVars = localBindingVars.head ++ vd  :: localBindingVars.tail }
  def getLocalBindingVars: Context = localBindingVars flatMap(_.variables)
  def addArguments(arguments: Context): Unit = { args ++= arguments }
  /**
   * Replaces the argument of same name by this one
   * This is used for changing the argument type when translating type changing statements
   * @param newArg An argument of same as one of the arguments in the context, but of different type
   * @precondition there are no two arguments of same name in the definition context
   */
  def replaceArguments(newArg: VarDecl): Unit = {
    args = args.map {
      case w if (w.name == newArg.name) => newArg
      case w => w
    }
  }
  def addAssumptions(assumptions: List[Term]): Unit = { this.assumptions ++= assumptions }
  def addAssumption(assumption: Term): Unit = { this.assumptions :+= assumption }
  def addUsedFacts(usedFacts: List[(Term, Option[Term])]): Unit = { this.usedFacts ++= usedFacts }
  def addUsedFact(usedFact: (Term, Option[Term])): Unit = { this.usedFacts :+= usedFact }
  def enterProof = {
    proofLevel += 1
    localBindingVars ::= Nil
    localDefinitions ::= Nil
  }
  def exitProof = {
    proofLevel -= 1
    localBindingVars = localBindingVars.tail
    localDefinitions = localDefinitions.tail
  }
  def withinProof = proofLevel > 0
  private def notWithinProofError = new TranslatingError("Trying to access thesis (seemingly) outside of a proof. ")
}
object DefinitionContext {
  def empty() = DefinitionContext()
}

sealed abstract class CaseByCaseDefinien {
  /**
   * Used to do inference on, mainly
   * @return
   */
  def someCase: Term
  def cases: List[Term]
  def caseRes: List[Term]
  def caseNum = cases.length
}
case class DirectPartialCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], defRes: Term) extends CaseByCaseDefinien {
  override def someCase: Term = defRes
}
object DirectPartialCaseByCaseDefinien {
  def apply(tm: Term): DirectPartialCaseByCaseDefinien = DirectPartialCaseByCaseDefinien(Nil, Nil, tm)
}
case class IndirectPartialCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], defRes: Term) extends CaseByCaseDefinien {
  override def someCase: Term = defRes
}
object IndirectPartialCaseByCaseDefinien {
  def apply(tm: Term): IndirectPartialCaseByCaseDefinien = IndirectPartialCaseByCaseDefinien(Nil, Nil, Lam("it", any, tm))
}
case class DirectCompleteCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], completenessProof: Option[Term] = None) extends CaseByCaseDefinien {
  override def someCase: Term = caseRes.head
}
case class IndirectCompleteCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], completenessProof: Option[Term] = None) extends CaseByCaseDefinien {
  override def someCase: Term = caseRes.head
}

object definiensTranslator {
  def translate_Definiens(defs:Definiens, just: Option[Justification] = None)(implicit defContext: DefinitionContext): CaseByCaseDefinien = {
    translate_CaseBasedExpr(defs._expr)
  }
  def translate_CaseBasedExpr(defn:CaseBasedExpr)(implicit defContext: DefinitionContext): CaseByCaseDefinien = {
    defn.check()
    if (defn.isSingleCase()) {
      val defRes = translate_Expression(defn.singleCasedExpr._expr.get)
      if (defRes.freeVars.contains(LocalName("it"))) {
        IndirectPartialCaseByCaseDefinien(defRes)
      } else {
        DirectPartialCaseByCaseDefinien(defRes)
      }
    } else {
      translate_Cased_Expression(defn.partialCasedExpr)
    }
  }
  def translate_Cased_Expression(partDef:PartialDef)(implicit defContext: DefinitionContext): CaseByCaseDefinien = {
    assert(partDef._partDefs.isDefined)
    partDef.check()
    val defRes = partDef._otherwise.get._expr map translate_Expression
    var isIndirect = false
    val complCases = partDef._partDefs.get._partDef map {
      case Partial_Definiens(_expr, _form) =>
        val caseCond = translate_Formula(_form)
        val caseRes = translate_Expression(_expr)
        if (caseRes.freeVars.contains(LocalName("it"))) {isIndirect = true}
        (caseCond, caseRes)
    }
    val (cases, indCaseRes) = complCases unzip
    val caseRes = indCaseRes map(Lam("it", any, _))
    val res : CaseByCaseDefinien = if (isIndirect) {
      if (defRes.isDefined) {
        IndirectPartialCaseByCaseDefinien(cases, caseRes, Lam("it", any, defRes.get))
      } else {
        IndirectCompleteCaseByCaseDefinien(cases, caseRes)
      }
    } else {
      if (defRes.isDefined) {
        DirectPartialCaseByCaseDefinien(cases, caseRes, defRes.get)
      } else {
        DirectCompleteCaseByCaseDefinien(cases, caseRes)
      }
    }
    res
  }
}

object patternTranslator {
  def parseFormatDesc(formatdes: String): (Int, Int, Int) = {
    val interior = formatdes.substring(formatdes.indexOf('[')+1, formatdes.lastIndexOf(']'))
    val List(pre, int, suf) = interior.split(Array('(', ')')).toList map(_.toInt)
    (pre, int, suf)
  }
  private def PrePostFixMarkers(del: String, infixArgNum: Int, suffixArgNum: Int, rightArgsBracketed: Boolean = false) = {
    assert (!del.isEmpty, "Encountered empty delimiter in PrePostFix. ")
    PrePostfix(Delim(del), infixArgNum, infixArgNum+suffixArgNum)
  }
  private def CircumfixMarkers(leftDel: String, rightDel: String, circumfixArgNr: Int) = {
    assert (!leftDel.isEmpty && !rightDel.isEmpty, "Encountered empty delimiter in CircumMarkers.\nThe delimiters are \""+leftDel+"\" and \""+rightDel+"\".")
    Circumfix(Delim(leftDel), Delim(rightDel), circumfixArgNr)
  }
  private def makeNotCont(fixity: Fixity): NotationContainer = {
    NotationContainer(TextNotation(fixity = fixity, precedence = Precedence.integer(ones = 20), meta = None))
  }
  def makeNotationCont(del: String, infixArgNum: Int, suffixArgNum: Int, rightArgsBracketed: Boolean = false) = {
    makeNotCont(PrePostFixMarkers(del, infixArgNum, suffixArgNum, rightArgsBracketed))
  }
  def globalLookup(pat: GloballyReferencingObjAttrs, notMainExtDecl: Boolean = false): GlobalName = {
    val referencedGn = computeGlobalName(pat, true)
    val hidden = referencedGn.toString contains "hidden"
    val gn = if (hidden) {
      resolveHiddenReferences(referencedGn) match {
        case Some(OMS(p)) => p
        case _ => referencedGn
      }
    } else referencedGn
    TranslationController
    if (notMainExtDecl || hidden) gn else pat.globalKind match {
      case c if (c == shortKind(FunctorKind())) => gn / "func"
      case c if (c == shortKind(AttributeKind())) => gn / "attr"
      case c if (c == shortKind(PredicateKind())) => gn / "pred"
      case c if (c == shortKind(ModeKind())) => gn / "mode"
      case _ => gn
    }
  }
  def translate_Pattern(pattern:Patterns, orgVersion: Boolean = false) : (LocalName, GlobalName, NotationContainer) = {
    val gn = if (orgVersion) globalLookup(pattern, true) else computeGlobalName(pattern)
    val name = gn.name

    val (infixArgNr, circumfixArgNr, suffixArgNr) = parseFormatDesc(pattern.patternAttrs.formatdes)
    val fstDel = pattern.patternAttrs.spelling
    assert (! fstDel.isEmpty)
    //val fstDel = if (spl != "") spl else name.toString
    val fixity = pattern match {
      case InfixFunctor_Pattern(rightargsbracketedO, orgExtPatAttr, _loci, _locis) =>
        val rightArgsBracketed = rightargsbracketedO.getOrElse(false)
        PrePostFixMarkers(fstDel, infixArgNr, suffixArgNr, rightArgsBracketed)
      case CircumfixFunctor_Pattern(orgExtPatAttr, _right_Circumflex_Symbol, _loci, _locis) =>
        val sndDel = _right_Circumflex_Symbol.spelling
        assert (! sndDel.isEmpty)
        CircumfixMarkers(fstDel, sndDel, circumfixArgNr)
      case pat: Patterns =>
        PrePostFixMarkers(fstDel, infixArgNr, suffixArgNr)
    }
    (name, gn, makeNotCont(fixity))
  }
  /**
   * Get the name, argument types, reference to the declaration, notation and parameters
   * of the declaration referenced by the pattern
   * @param pat
   * @return
   */
  def translate_Referencing_Pattern(pse: Pattern_Shaped_Expression)(implicit defContext: DefinitionContext): (LocalName, List[Term], Term, NotationContainer, List[Term]) = {
    val pat: Patterns = pse._pat
    val params = pat._locis.flatMap(_._loci map translate_Locus)
    val (name, gn, notC) = translate_Pattern(pat, true)
    val (mainDecl, addArgsTps): (Term, List[Term]) = TranslationController.controller.get(gn) match {
      case c: Constant =>
        val FunType(addArgsTps, prop) = c.tp.getOrElse(
          throw new ObjectLevelTranslationError("Trying to retrieve type of looked up declaration " + c.name + " referenced by pattern: " + pat + ", but declaration has not type defined. ", pat))
        (c.toTerm, addArgsTps.map(_._2))
      case dd: symbols.DerivedDeclaration =>
        val addArgTps = dd match {
          case DirectPartialAttributeDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case IndirectPartialAttributeDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case DirectCompleteAttributeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case IndirectCompleteAttributeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case DirectPartialFunctorDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case IndirectPartialFunctorDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case DirectCompleteFunctorDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case IndirectCompleteFunctorDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case DirectPartialModeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case IndirectPartialModeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case DirectCompleteModeDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
          case IndirectCompleteModeDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
          case DirectPartialPredicateDef(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case DirectCompletePredicateDef(_, _, addArgTps, _, _, _, _) => addArgTps
          case other => throw PatternTranslationError("Expected reference to original declaration of same kind in redefinition, but instead found "+other.feature+" at referenced location "+other.path+"\nReferenced by the pattern: "+pat+". ", pat)
        }
        (OMS(globalLookup(pat)), addArgTps)
    }
    (name, addArgsTps, mainDecl, notC, params)
  }
}

object propertyTranslator {
  def translate_JustifiedProperty(justProp: MizarProperty, definitionRef: Option[GlobalName])(implicit definitionContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = (justProp, definitionRef) match {
    case (Sethood(_just, Some(_tp)), None) =>
      val tp = translate_Type(_tp)
      val claim = constant("sethood")(tp)
      val just = _just map (translate_Justification(_, claim)) getOrElse None
      val name = LocalName("sethood_of_"+tp.toStr(true))
      makeConstantInContext(name, Some(Univ(1)), just)
    case (p: MizarProperty, Some(defRef)) =>
      val claim =constant(p.property)(defRef())
      val just = p.just map (translate_Justification(_, claim)) getOrElse None
      val name = LocalName(p.property+"_of_"+defRef.name.toString)
      makeConstantInContext(name, Some(Univ(1)), just)
    case _ => throw ImplementationError("Invalid parsing assumption: Found property outside of definition block, which isn't sethood. ")
  }
}

object subitemTranslator {
  def notToplevel = new TranslatingError("This kind of declaration should not occur on toplevel. ")
  def translate_Reservation(reservation: Reservation) = { Nil }
  def translate_Definition_Item(definition_Item: Definition_Item): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    definition_Item.check() match {
      case "Definitional-Block" => translate_Definitional_Block(definition_Item._block)
      case "Registration-Block" => translate_Registration_Block(definition_Item._block)
      case "Notation-Block" => translate_Notation_Block(definition_Item._block)
    }
  }
  def translate_Section_Pragma(section_Pragma: Section_Pragma) = { Nil }
  def translate_Pragma(pragma: Pragma) = { Nil }
  def translate_Identify(identify: Identify)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = { clusterTranslator.translate_Identify(identify) }
  def translate_Scheme_Block_Item(scheme_Block_Item: Scheme_Block_Item)(implicit defContext: => DefinitionContext = DefinitionContext.empty()) = scheme_Block_Item match {
    case sbi @ Scheme_Block_Item(_, _block) =>
      val gn = sbi.globalName
      val provenSentence = sbi.provenSentence()
      val Scheme_Head(_sch, _vars, _form, _provForm) = sbi.scheme_head()
      val spelling = _sch.spelling match { case "" => "AnonymousScheme_"+TranslationController.incrementAndGetAnonymousSchemeCount().toString case str => str }
      _vars._segms foreach (segm => translateBindingVariables(segm)(defContext))
      val ass : List[Term] = _provForm.map(_._props map(translate_Claim(_)(defContext))) getOrElse Nil
      implicit val notC = makeNotationCont(spelling, 0, defContext.args.length, true)
      val (p, prf) = translate_Proved_Claim(provenSentence)(defContext)
      TranslationController.articleStatistics.incrementStatisticsCounter("scheme")
      List(SchemeDefinitionInstance(gn.name, defContext.args map (_.tp.get), ass, p, prf))
  }
}

object headTranslator {
  // this may never be called in practise
  def translate_Head(head:Heads) = { Nil }
  def translate_Scheme_Head(reservation: Scheme_Head) = {}
  def translate_Suppose_Head(reservation: Suppose_Head) = {}
  def translate_Case_Head(reservation: Case_Head) = {}
}

object nymTranslator {
  def translate_Nym(nym:Nyms)(implicit defContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    val oldPatRef = nym._patRefOld
    val newPat: Patterns = nym._patNew
    val (name, newName, notC) = translate_Pattern(newPat)
    try {
      val (_, addArgsTps, mainDecl, _, _) = translate_Referencing_Pattern(oldPatRef)
      val allArgs = defContext.args.map(_.tp.get)// ++ addArgsTps
      val res  = SynonymicNotation(newName.name, allArgs.length, allArgs, mainDecl)(notC)
      TranslationController.articleStatistics.incrementStatisticsCounter("nym")
      List(res)
    } catch {
      case e: ObjectLevelTranslationError => Nil
    }
  }
}

object statementTranslator {
  def translate_Statement(st:Statement with TopLevel)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = st match {
    case choice_Statement: Choice_Statement =>
      val (addArgs, (claim, proof)) = translate_Choice_Statement(choice_Statement)
      val gn = makeNewGlobalName("Choice_Statement", incrementAndGetAnonymousTheoremCount())
      implicit val defCtx = defContext.copy(args = defContext.args ++ addArgs, assumptions = defContext.assumptions :+ claim)
      val theoremDecl = makeConstantInContext(gn.name, Some(claim), proof)(defContext = defCtx)
      theoremDecl
    case type_Changing_Statement: Type_Changing_Statement => translate_Type_Changing_Statement(type_Changing_Statement)
    case theorem_Item: Theorem_Item => translate_Theorem_Item(theorem_Item)
    case regular_Statement: Regular_Statement => translate_Regular_Statement(regular_Statement)
  }
  def translate_Conclusion(conclusion: Conclusion) = { Nil }
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement)(implicit defContext: => DefinitionContext) : Declaration with HasType with HasDefiniens with HasNotation = {
    val gn = makeNewGlobalName("Type_Changing_Statement", incrementAndGetAnonymousTheoremCount())
    val (claim, proof) = translate_Proved_Claim(type_Changing_Statement.prfClaim)(defContext)
    makeConstantInContext(gn.name, Some(claim), proof)
  }
  def translate_Theorem_Item(theorem_Item: Theorem_Item)(implicit defContext: DefinitionContext) = {
    implicit val gn = theorem_Item.referencedLabel
    val (claim, proof) = translate_Proved_Claim(theorem_Item.prfClaim)
    TranslationController.articleStatistics.incrementStatisticsCounter("thm")
    makeConstantInContext(gn.name, Some(claim), proof)
  }
  def translate_Choice_Statement(choice_Statement: Choice_Statement)(implicit defContext: DefinitionContext): (Context, (Term, Option[Term])) = {
    val vars: Context = choice_Statement._qual._children.flatMap(translate_Context(_))
    val facts = translate_Proved_Claim(choice_Statement.prfClaim)
    (vars, facts)
  }
  def translate_Regular_Statement(regular_Statement: Regular_Statement)(implicit defContext: DefinitionContext) = {
    val gn = makeNewGlobalName("Regular-Statement", incrementAndGetAnonymousTheoremCount())
    val (claim, proof) = translate_Proved_Claim(regular_Statement.prfClaim)
    val theoremDecl = makeConstantInContext(gn.name, Some(claim), proof)
    theoremDecl
  }
}

object definitionTranslator {
  def translate_Definition(defn:Definition)(implicit defContext: => DefinitionContext) : List[Declaration with HasType with HasDefiniens with HasNotation] = {
    val translatedDecls: List[Declaration with HasType with HasDefiniens with HasNotation] = defn match {
      case d: Structure_Definition => translate_Structure_Definition(d)(defContext)
      case cd: Constant_Definition => translate_Constant_Definition(cd)(defContext)
      case rld: RedefinableLabeledDefinition => translate_Redefinable_Labelled_Definition(rld)(defContext)
      case d: Private_Functor_Definition => List(translate_Private_Functor_Definition(d)(defContext))
      case d: Private_Predicate_Definition => List(translate_Private_Predicate_Definition(d)(defContext))
      case md: Mode_Definition => List(translate_Mode_Definition(md)(defContext))
    }
    val declRef = Some(translatedDecls.head.path)
    lazy val justProps = defContext.props.map(translate_JustifiedProperty(_, declRef)(defContext))
    translatedDecls ++ (if (defContext.withinProof) justProps else Nil)
  }
  def translate_Redefinable_Labelled_Definition(redefinableLabeledDefinition: RedefinableLabeledDefinition)(implicit defContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    redefinableLabeledDefinition.check
    val (pat, _defn, label) = (redefinableLabeledDefinition._pat, redefinableLabeledDefinition._def, redefinableLabeledDefinition.mmlIdO)
    val (ln, gn, notC) = translate_Pattern(pat)
    val path = gn
    val name = path.name
    implicit val notCon = notC
    val defn = _defn.map(translate_Definiens(_))
    val firstRes = if (defn.isEmpty) {
      if(redefinableLabeledDefinition.redefinition) {
        val ret = redefinableLabeledDefinition match {
          case Functor_Definition(mmlIdO, _redef, _pat, _tpSpec, _def) =>
            _tpSpec map (tpSpec => translate_Type(tpSpec._types))
          case _ => None
        }
        translate_Redefine(pat, ln, ret)(notC)
      } else {
        throw ImplementationError("This should never occur. ")
      }
    } else {
      lazy val consistencyOf = defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap(_.just)
      val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
      val resDef = redefinableLabeledDefinition match {
        case ad : Attribute_Definition =>
          val motherTp = inferType(defn.get.someCase)
          TranslationController.articleStatistics.incrementStatisticsCounter("attr")
          defn.get match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, true, "attr")
              DirectPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes, consistency)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, false, "attr")
              IndirectPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes, consistency)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, true, "attr")
              DirectCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, consistency)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, false, "attr")
              IndirectCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, consistency)
          }
        case Functor_Definition(mmlIdO, _redef, _pat, _tpSpec, _def) =>
          val gn = computeGlobalName(_pat, true)
          val ret = if (_redef.occurs) {
            def getRet(gn: GlobalName): Term = {
              val ddO = TranslationController.controller.getO(gn)
              (ddO map {
                case dd: symbols.DerivedDeclaration => dd match {
                case DirectPartialFunctorDefinition(_, _, _, ret, _, _, _, _, _) => ret
                case IndirectPartialFunctorDefinition(_, _, _, ret, _, _, _, _, _) => ret
                case DirectCompleteFunctorDefinition(_, _, _, ret, _, _, _, _, _) => ret
                case IndirectCompleteFunctorDefinition(_, _, _, ret, _, _, _, _, _) => ret
                case NymicNotation(_, "synonymicNotation", _, _, v) =>
                  val OMS(p) = v
                  getRet(p.module ? p.name.init)
                case _ =>
                  val err = new TranslatingError("dsjfljlsjlf")
                  throw err
              }}).get
            }
            getRet(gn)
          } else translate_Type(_tpSpec.get._types)
          TranslationController.articleStatistics.incrementStatisticsCounter("funct")
          defn.get match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, true, "func")
              DirectPartialFunctorDefinition(name, argNum, argTps, ret, defn.get.caseNum, cases, caseRes, defRes, consistency)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, false, "func")
              IndirectPartialFunctorDefinition(name, argNum, argTps, ret, defn.get.caseNum, cases, caseRes, defRes, consistency)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, true, "func")
              DirectCompleteFunctorDefinition(name, argNum, argTps, ret, defn.get.caseNum, cases, caseRes, consistency)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, false, "func")
              IndirectCompleteFunctorDefinition(name, argNum, argTps, ret, defn.get.caseNum, cases, caseRes, consistency)
          }
        case prd: Predicate_Definition =>
          TranslationController.articleStatistics.incrementStatisticsCounter("pred")
          defn.get match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, true, "pred")
              DirectPartialPredicateDef(name, argNum, argTps, defn.get.caseNum, cases, caseRes, defRes, consistency)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, true, "pred")
              DirectCompletePredicateDef(name, argNum, argTps, defn.get.caseNum, cases, caseRes, consistency)
            case _ => throw DeclarationTranslationError("Predicate definition can't be indirect. ", prd)
          }
      }
      resDef
    }
    firstRes :: ((label map(_ => makeConstantInContext(redefinableLabeledDefinition.globalName.name, None, Some(OMS(gn)))) map(List(_)) getOrElse Nil))
  }
  def translate_Structure_Definition(strDef: Structure_Definition)(implicit defContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    val l = defContext.args.length
    implicit var selectors: List[(Int, VarDecl)] = Nil
    val substr: List[Term] = strDef._ancestors._structTypes.map(translate_Type) map {
      case ApplyGeneral(typeDecl, args) => typeDecl
      case _ => throw ImplementationError("This should be impossible.")
    }
    val n = substr.length
    var substitutions : List[Sub] = Nil
    val declarationPath = strDef._strPat.globalPatternName
    val Structure_Patterns_Rendering(_aggrFuncPat, _, _strFuncPat, Selectors_List(_selectorFuncPat)) = strDef._rendering
    val aggrNot::strNot::selNots  = _aggrFuncPat::_strFuncPat::_selectorFuncPat map(translate_Pattern(_)) map(t=> (t._1, t._3))

    def translate_Field_Segments(field_Segments: Field_Segments)(implicit defContext: DefinitionContext) : List[VarDecl] = field_Segments._fieldSegments flatMap {
      case field_Segment: Field_Segment =>
			val tp = translate_Type(field_Segment._tp)
      field_Segment._selectors._loci.reverse map { case selector =>

        val selName = OMV(selector.spelling)//translate_Locus(selector._loci)
        val sel = (selector.nr, selName % tp)
        selectors ::= sel
        substitutions ::= selName / PatternUtils.referenceExtDecl(declarationPath, selName.name.toString)
        (sel._2 ^ substitutions).copy(not = selNots.find(_._1 == sel._2.name).map(_._2.getAllNotations.head))
      }
    }
    val fieldDecls = translate_Field_Segments(strDef._fieldSegms)
    val m = fieldDecls.length

    TranslationController.articleStatistics.incrementStatisticsCounter("struct")
    StructureInstance(declarationPath, l, defContext.args, n, substr, m, Context.list2context(fieldDecls) ^ namedDefArgsSubstition())
  }
  def translate_Constant_Definition(constant_Definition: Constant_Definition)(implicit defContext: DefinitionContext): List[Constant] = {
    constant_Definition._children map {eq =>
      val name = LocalName(eq._var.varAttr.copy(kind = "DefConstant").toIdentifier)
      val df = translate_Term(eq._tm)
      val notC = makeNotationCont(eq._var.varAttr.spelling, 0, 0)
      makeConstantInContext(name, None, Some(df))(notC)
    }
  }
  def translate_Mode_Definition(mode_Definition: Mode_Definition)(implicit defContext: DefinitionContext) = {
    val (name, declarationPath, notC) = translate_Pattern(mode_Definition._pat)
    implicit val notCon = notC
    mode_Definition._expMode match {
      case Expandable_Mode(_tp) =>
        val tp = translate_Type(_tp)
        makeConstantInContext(declarationPath.name, Some(tp), Some(tp))
      case stm @ Standard_Mode(_tpSpec, _def) =>
        val name = declarationPath.name
        val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
        val defnO = _def map(translate_Definiens(_))

        if (defnO.isEmpty) {
          if (_tpSpec.isDefined) {
            makeConstant(declarationPath.name, translate_Type(_tpSpec.get._types))
          } else {
            //Only here we may have content that doesn't typecheck
            assert(hiddenArts contains declarationPath.module)
            ???
          }
        } else {
          val defn = defnO.get
          TranslationController.articleStatistics.incrementStatisticsCounter("mode")
          val consistencyOf = defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap(_.just)
          defn match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, true, "mode")
              DirectPartialModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, defRes, consistency)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, false, "mode")
              IndirectPartialModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, defRes, consistency)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, true, "mode")
              DirectCompleteModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, consistency)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              val consistency = translate_consistency(consistencyOf, argTps, cases, caseRes, false, "mode")
              IndirectCompleteModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, consistency)
          }
        }
    }
  }
  /**
   * translates a private functor definition
   * @param private_Functor_Definition the private functor definition to translate
   * @param defContext the definition context
   * @effect if within a proof add the translated functor definition to the lists of local definitions
   * @return the translated functor definition
   */
  def translate_Private_Functor_Definition(private_Functor_Definition: Private_Functor_Definition)(implicit defContext: => DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = {
    val v = translate_Variable(private_Functor_Definition._var)(defContext)
    val gn = makeNewGlobalName("Private-Functor", private_Functor_Definition._var.varAttr.idnr)
    val args: Context = private_Functor_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map({case (tp, i) => OMV("placeholder_"+i.toString) % tp})
    val tp = PiOrEmpty(args, any)
    val dfBody = translate_Term(private_Functor_Definition._tm)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    if (defContext.withinProof) defContext.addLocalDefinition(gn.name, df)
    makeConstantInContext(gn.name, Some(tp), Some(df))
  }
  /**
   * translates a private predicate definition
   * @param private_Predicate_Definition the private predicate definition to translate
   * @param defContext the definition context
   * @effect if within a proof add the translated predicate definition to the lists of local definitions
   * @return the translated predicate definition
   */
  def translate_Private_Predicate_Definition(private_Predicate_Definition: Private_Predicate_Definition)(implicit defContext: => DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = {
    val v = translate_Variable(private_Predicate_Definition._var)(defContext)
    val gn = makeNewGlobalName("Private-Predicate", private_Predicate_Definition._var.varAttr.idnr)
    val args: Context = private_Predicate_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map({case (tp, i) => OMV("placeholder_"+i.toString) % tp})
    val tp = PiOrEmpty(args, prop)
    val dfBody = translate_Formula(private_Predicate_Definition._form)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    if (defContext.withinProof) defContext.addLocalDefinition(gn.name, df)
    makeConstantInContext(gn.name, Some(tp), Some(df))
  }
  def translate_Redefine(p: RedefinablePatterns, ln: LocalName, ret: Option[Term])(implicit notC: NotationContainer) = {
    TranslationController.controller.get(globalLookup(p, true)) match {
      case c: Constant => makeConstant(ln, Some(ret getOrElse c.tp.get), c.df)
      case dd@MizarPatternInstance(_, pat, params) => makeConstant(ln, dd.tp, Some(dd.path()))//MizarPatternInstance(ln, pat, params)
      case dd@MizarPatternInstance(_, pat, params) => makeConstant(ln, dd.tp, Some(dd.path()))//MizarPatternInstance(ln, pat, params)
      case d: Declaration => throw PatternTranslationError("Failure to look up the referenced definition to redefine at "+d.path.module.last+"?"+d.path.name+"(full path "+d.path+"). ", p)
    }
  }
}

object clusterTranslator {
  def translate_Registration(reg: Registrations)(implicit definitionContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    TranslationController.articleStatistics.incrementStatisticsCounter("registr")
    val resDecl: Option[Declaration with HasType with HasDefiniens with HasNotation] = reg match {
      case Conditional_Registration(_attrs, _at, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_attrs)
        val ats = attributeTranslator.translateAttributes(_at)
        val name = LocalName("condReg_"+TranslationController.articleStatistics.numRegistrs)
        Some(ConditionalRegistration(name, definitionContext.args map(_.tp.get), tp, adjs, ats))
      case Existential_Registration(_adjClust, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_adjClust)
        //TODO:
        val name = LocalName("existReg_"+TranslationController.articleStatistics.numRegistrs)
        Some(ExistentialRegistration(name, definitionContext.args map(_.tp.get), tp, adjs))
      case Functorial_Registration(_aggrTerm, _adjCl, _tp) =>
        val tm = translate_Term(_aggrTerm)
        val adjs = attributeTranslator.translateAttributes(_adjCl)
        val isQualified = _tp.isDefined
        val tp = _tp map translate_Type getOrElse({
          inferType(tm)})
        val name = LocalName("funcReg_"+TranslationController.articleStatistics.numRegistrs)
        Some(if (isQualified) {
          QualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs)
        } else {
          UnqualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs)
        })
      case Property_Registration(_props, _just) => Some(translate_JustifiedProperty(_props.matchProperty(Some(_just)), None)(definitionContext))
    }
    resDecl.map(List(_)).getOrElse(Nil)
  }
  def translate_Identify(identify: syntax.Identify)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = identify match {
    case syntax.Identify(_firstPat, _sndPat, _lociEqns) =>
      val translatedLociEqns = _lociEqns._lociEqns map {
        case Loci_Equality(_fstLoc, _sndLoc) => (translate_Locus(_fstLoc)(defContext), translate_Locus(_sndLoc)(defContext))
      }
      val num = incrementAndGetIdentifyCount()
      val name = LocalName("identify"+num)
      val (_, _, f, _, fparams) = translate_Referencing_Pattern(_firstPat)
      val (_, _, g, _, gparams) = translate_Referencing_Pattern(_sndPat)
      mmtwrapper.Identify(name, defContext.args map(_.tp.get), translatedLociEqns, f(fparams:_*), g(gparams:_*))
  }
  def translate_Cluster(cl:Cluster)(implicit definitionContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    //TODO: Also translate the proofs of the correctness conditions
    cl._registrs flatMap translate_Registration
  }
}
object blockTranslator {
  def collectSubitems[mainSort <: BlockSubitem](cls: Class[mainSort], block: Block) : List[(mainSort, DefinitionContext)] = {
    val items = block._items
    implicit var defContext = DefinitionContext.empty()
    var resDecls: List[(mainSort, DefinitionContext)] = Nil

    items.zipWithIndex foreach { case (it: Item, ind: Int) =>
      it._subitem match {
        case loci_Declaration: Loci_Declaration =>
          loci_Declaration._qualSegms._children foreach {segm =>
            defContext.addArguments(translate_Context(segm)(defContext))
          }
        case ass: Assumptions => defContext.addAssumption(translate_Assumption_Claim(ass))
        case Assumption(ass) => defContext.addAssumption(translate_Assumption_Claim(ass))
        case choice_Statement: Choice_Statement =>
          val (addArgs, addFacts) = translate_Choice_Statement(choice_Statement)
          defContext.addArguments(addArgs)
          defContext.addUsedFact(addFacts)
        case statement: Regular_Statement =>
          defContext.addUsedFact(translate_Proved_Claim(statement.prfClaim))
        //We put the guard, since the type pattern is eliminated by the compiler, it is for better error messages only, as we shouldn't ever encounter any other subitems (not to be ignored) at this point
        case defin if (cls.isInstance(defin)) =>
          val defn = defin.asInstanceOf[mainSort]
          implicit var corr_conds: List[JustifiedCorrectnessConditions] = Nil
          implicit var props: List[MizarProperty] = Nil
          items.drop(ind).foreach(_._subitem match {
            case Correctness_Condition(_cor, _justO) => corr_conds ++:= JustifiedCorrectnessConditions.translate(List(_cor), _justO).reverse
            case Correctness(_cor, _just) => corr_conds ++:= JustifiedCorrectnessConditions.translate(_cor._cond, Some(_just)).reverse
            case prop: Property => props ::= prop.matchProperty()
            case _ =>
          })
          corr_conds = corr_conds.reverse
          props = props.reverse
          val correspondingDefContext = defContext.copy(corr_conds = corr_conds, props = props)
          resDecls +:= (defn, correspondingDefContext)
        case corr: Correctness =>
        case correctness_Condition: Correctness_Condition =>
        case prop:Property =>
        case prag: Pragma =>
        //TODO: figure out what to do with this
        case red: Reduction =>
        /*This is tricky to translate:
        It corresponds to a (proven) statement which is later used as an assumption in the definition

        For now, we simply pass it on to the definition context
        TODO: Actually use them
        */
        case defIt => throw DeclarationTranslationError("Unexpected item of type " + defIt.kind+" found, in "+block.kind+" in file "+currentAid+".miz", defIt)
      }
    }
    resDecls
  }
  def translate_Definitional_Block(block:Block):List[Declaration with HasType with HasDefiniens with HasNotation] = {
    val definitionItems = collectSubitems[Definition](classOf[Definition], block)
    definitionItems flatMap {
      case (defn: Definition, defContext: DefinitionContext) =>
        translate_Definition(defn)(defContext)
    }
  }
  def translate_Registration_Block(block: Block) : List[Declaration with HasType with HasDefiniens with HasNotation] = {
    val clusterItems = collectSubitems[RegistrationSubitems](classOf[RegistrationSubitems], block)
    clusterItems flatMap {
      case (cl: Cluster, defContext: DefinitionContext) =>
        translate_Cluster(cl)(defContext)
      case (reg: Registrations, defContext: DefinitionContext) => translate_Registration(reg)(defContext)
      case (id: syntax.Identify, defContext: DefinitionContext) => List(translate_Identify(id)(defContext))
    }
  }
  def translate_Notation_Block(block: Block): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    val notationItems = collectSubitems[Nyms](classOf[Nyms], block)
    notationItems flatMap {
      case (nym: Nyms, defContext: DefinitionContext) =>
        translate_Nym(nym)(defContext)
    }
  }
}