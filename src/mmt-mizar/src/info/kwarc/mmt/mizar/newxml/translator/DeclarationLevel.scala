package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api._
import notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mizar.newxml._
import mmtwrapper.{FunctorDefinitionInstance, _}
import MMTUtils._
import MizarPrimitiveConcepts._
import syntax._
import expressionTranslator._
import termTranslator._
import typeTranslator._
import contextTranslator._
import formulaTranslator._
import TranslatorUtils._
import claimTranslator._
import definitionTranslator._
import blockTranslator._
import info.kwarc.mmt.api.symbols.{Constant, Declaration, HasDefiniens, HasNotation, HasType}
import info.kwarc.mmt.mizar.newxml.mmtwrapper.PatternUtils.{LambdaOrEmpty, PiOrEmpty, argsVarName, lambdaBindArgs}
import info.kwarc.mmt.mizar.newxml.syntax.Utils._
import clusterTranslator._
import definiensTranslator._
import info.kwarc.mmt.mizar.newxml.mmtwrapper.MizSeq.{Index, OMI, Sequence, nTerms}
import info.kwarc.mmt.mizar.newxml.translator.correctnessConditionTranslator.{translate_def_correctness_condition, translate_reg_correctness_condition}
import info.kwarc.mmt.mizar.newxml.translator.TranslationController.{currentAid, incrementAndGetAnonymousTheoremCount, incrementAndGetIdentifyCount, incrementAndGetReduceCount, inferType, localPath, makeConstant, makeConstantInContext}
import info.kwarc.mmt.mizar.newxml.translator.statementTranslator.translate_Choice_Statement
import JustificationTranslator._
import propertyTranslator._
import nymTranslator._
import patternTranslator._

object correctnessConditionTranslator {
  def translate_def_correctness_condition(cor_cond: CorrectnessConditions, just: Option[Justification], defn: CaseByCaseDefinien, kind : String, retO: Option[Term] = None)(implicit defContext: DefinitionContext) = {
    val pattern = kind match {
      case "func" => "FuncDef"
      case "pred" | "attr" => "PredAttrDef"
      case "mode" => "ModeDef"
    }
    val arguments = defContext.args map (_.tp.get)
    implicit val args = arguments map (lambdaBindArgs(_)(arguments))
    val argNum = OMI(args.length)
    val ret = retO map(List(_)) getOrElse Nil
    val caseNum = OMI(defn.caseNum)
    val cases = Sequence(defn.cases map lambdaBindArgs)
    val caseRes = Sequence(defn.caseRes map lambdaBindArgs)
    val defRes = defn.defResult map(tm => List(lambdaBindArgs(tm))) getOrElse Nil
    val claim = ApplyGeneral(correctnessCondClaim(cor_cond, defn.prefixes+pattern), argNum::Sequence(args)::ret:::caseNum::cases::caseRes::defRes)
    just map(translate_Justification(_, claim)) getOrElse uses(claim, Nil)
  }
  def translate_reg_correctness_condition(just: Option[Justification], kind: String, m: Int, tO: Option[Term], tmO: Option[Term], aO: Option[List[Term]], bO: Option[List[Term]], IdentifyPairs: Option[List[Term]])(implicit defContext: DefinitionContext) = {
    val ln = LocalName(kind match {
      case "existRegistration" => "existenceExistRegistration"
      case "condRegistration" => "coherenceCondRegistration"
      case "qualFuncRegistration" => "coherenceQualFuncRegistration"
      case "unqualFuncRegistration" => "coherenceUnqualFuncRegistration"
      case "identify" => "compatibilityIdentify"
      case "reduce" => "reducibilityReduction"
    })
    val arguments = defContext.args map (_.tp.get)
    implicit val argTps = arguments map (lambdaBindArgs(_)(arguments))
    val argNum = OMI(argTps.length)
    lazy val t = tO.get
    lazy val tm = tmO map lambdaBindArgs map (List(_)) getOrElse Nil
    lazy val a = Sequence (aO.get)
    lazy val qb = bO map (b => List(OMI(b.length), Sequence(b))) getOrElse Nil

    val furtherParams = if (List("identify", "reduce") contains kind) IdentifyPairs.get map lambdaBindArgs else t::tm:::a::qb
    val claim = ApplyGeneral(OMS(MizarPatternsTh ? ln), argNum::Sequence(argTps)::OMI(m)::furtherParams)
    just map(translate_Justification(_, claim)) getOrElse uses(claim, Nil)
  }
}

case class DefinitionContext(var args: Context = Context.empty, var assumptions: List[Term] = Nil, var usedFacts: List[(Term, Term)] = Nil, corr_conds: List[Correctness_Condition] = Nil, props: List[MizarProperty] = Nil) {
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
   * Add a binding variable with scope the current proof
   * @param vd the binding variable
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
      case w if w.name == newArg.name => newArg
      case w => w
    }
  }
  def addAssumptions(assumptions: List[Term]): Unit = { this.assumptions ++= assumptions }
  def addAssumption(assumption: Term): Unit = { this.assumptions :+= assumption }
  def addUsedFacts(usedFacts: List[(Term, Term)]): Unit = { this.usedFacts ++= usedFacts }
  def addUsedFact(usedFact: (Term, Term)): Unit = { this.usedFacts :+= usedFact }
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
  def cases: List[Term]
  def caseRes: List[Term]
  def caseNum = cases.length
  def defResult: Option[Term]
  /**
   * Used to do inference on, mainly
   * @return
   */
  def someCase: Term = defResult getOrElse caseRes.head
  def direct: Boolean
  def partial: Boolean
  def prefixes = (if (direct) "direct" else "indirect")+(if (partial) "Part" else "Compl")
}
sealed trait DirectCaseByCaseDefinien extends CaseByCaseDefinien {
  override def direct: Boolean = true
}
sealed trait IndirectCaseByCaseDefinien extends CaseByCaseDefinien {
  override def direct: Boolean = false
}
sealed abstract class PartialCaseByCaseDefinien extends CaseByCaseDefinien {
  def defRes: Term
  override def defResult = Some(defRes)
  override def partial: Boolean = true
}
sealed abstract class CompleteCaseByCaseDefinien extends CaseByCaseDefinien {
  override def defResult: Option[Term] = None
  def completenessProof: Option[Term]
  override def partial: Boolean = true
}
case class DirectPartialCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], defRes: Term) extends PartialCaseByCaseDefinien with DirectCaseByCaseDefinien
object DirectPartialCaseByCaseDefinien {
  def apply(tm: Term): DirectPartialCaseByCaseDefinien = DirectPartialCaseByCaseDefinien(Nil, Nil, tm)
}
case class IndirectPartialCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], defRes: Term) extends PartialCaseByCaseDefinien with IndirectCaseByCaseDefinien
object IndirectPartialCaseByCaseDefinien {
  def apply(tm: Term): IndirectPartialCaseByCaseDefinien = IndirectPartialCaseByCaseDefinien(Nil, Nil, Lam("it", any, tm))
}
case class DirectCompleteCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], completenessProof: Option[Term] = None) extends CompleteCaseByCaseDefinien with DirectCaseByCaseDefinien
case class IndirectCompleteCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], completenessProof: Option[Term] = None) extends CompleteCaseByCaseDefinien with IndirectCaseByCaseDefinien

object definiensTranslator {
  def translate_Definiens(defs:Definiens, isModeDef: Boolean = false)(implicit defContext: DefinitionContext): CaseByCaseDefinien = {
    translate_CaseBasedExpr(defs._expr, isModeDef)
  }
  def translate_CaseBasedExpr(defn:CaseBasedExpr, isModeDef: Boolean = false)(implicit defContext: DefinitionContext): CaseByCaseDefinien = {
    defn.check()
    if (defn.isSingleCase()) {
      val defRes = translate_Expression(defn.singleCasedExpr._expr.get)
      if (defRes.freeVars.contains(LocalName("it"))) {
        IndirectPartialCaseByCaseDefinien(defRes)
      } else {
        DirectPartialCaseByCaseDefinien(defRes)
      }
    } else {
      translate_Cased_Expression(defn.partialCasedExpr, isModeDef)
    }
  }
  def translate_Cased_Expression(partDef:PartialDef, isModeDef: Boolean = false)(implicit defContext: DefinitionContext): CaseByCaseDefinien = {
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
    val caseRes = indCaseRes map(Lam("it", if (isModeDef) Arrow(any, prop) else any, _))
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
    assert (del.nonEmpty, "Encountered empty delimiter in PrePostFix. ")
    PrePostfix(Delim(del), infixArgNum, infixArgNum+suffixArgNum)
  }
  private def CircumfixMarkers(leftDel: String, rightDel: String, circumfixArgNr: Int) = {
    assert (leftDel.nonEmpty && rightDel.nonEmpty, "Encountered empty delimiter in CircumMarkers.\nThe delimiters are \""+leftDel+"\" and \""+rightDel+"\".")
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
      case c if c == shortKind(FunctorKind()) => gn / "func"
      case c if c == shortKind(AttributeKind()) => gn / "attr"
      case c if c == shortKind(PredicateKind()) => gn / "pred"
      case c if c == shortKind(ModeKind()) => gn / "mode"
      case _ => gn
    }
  }
  def translate_Pattern(pattern:Patterns, orgVersion: Boolean = false) : (LocalName, GlobalName, NotationContainer) = {
    val gn = if (orgVersion) globalLookup(pattern, true) else computeGlobalName(pattern)
    val name = gn.name

    val (infixArgNr, circumfixArgNr, suffixArgNr) = parseFormatDesc(pattern.patternAttrs.formatdes)
    val fstDel = pattern.patternAttrs.spelling
    assert (fstDel.nonEmpty)
    //val fstDel = if (spl != "") spl else name.toString
    val fixity = pattern match {
      case InfixFunctor_Pattern(rightargsbracketedO, _) =>
        val rightArgsBracketed = rightargsbracketedO.getOrElse(false)
        PrePostFixMarkers(fstDel, infixArgNr, suffixArgNr, rightArgsBracketed)
      case CircumfixFunctor_Pattern(orgExtPatAttr, _right_Circumflex_Symbol, _loci, _locis) =>
        val sndDel = _right_Circumflex_Symbol.spelling
        assert (sndDel.nonEmpty)
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
    val (mainDecl, addArgsTps): (Term, List[Term]) = TranslationController.controller.getO(gn) match {
      case Some(c: Constant) =>
        val FunType(addArgsTps, prop) = c.tp.getOrElse(
          throw new ObjectLevelTranslationError("Trying to retrieve type of looked up declaration " + c.name + " referenced by pattern: " + pat + ", but declaration has not type defined. ", pat))
        (c.toTerm, addArgsTps.map(_._2))
      case Some(dd: symbols.DerivedDeclaration) =>
        val addArgTps = dd match {
          case AttributeDefinitionInstance(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case FunctorDefinitionInstance(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case ModeDefinitionInstance(_, _, addArgTps, _, _, _, _) => addArgTps
          case PredicateDefinitionInstance(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case other => throw PatternTranslationError("Expected reference to original declaration of same kind in redefinition, but instead found "+other.feature+" at referenced location "+other.path+"\nReferenced by the pattern: "+pat+". \nFull description of the referenced declaration: "+other.toString+". \n", pat)
        }
        (OMS(globalLookup(pat)), addArgTps)
      case _ =>
        throw PatternTranslationError("this should never happen: "+pat.toString+" could not be dereferenced. ", pse._pat)
    }
    (name, addArgsTps, mainDecl, notC, params)
  }
}

object propertyTranslator {
  def translate_JustifiedProperty(justProp: MizarProperty, definitionRef: Option[GlobalName])(implicit definitionContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = (justProp, definitionRef) match {
    case (Sethood(_just, Some(_tp)), None) =>
      val tp = translate_Type(_tp)
      val claim = constant("sethood")(tp)
      val just = _just map (translate_Justification(_, claim))
      val name = LocalName("sethood_of_"+tp.toStr(true))
      makeConstantInContext(name, Some(Univ(1)), just)
    case (p: MizarProperty, Some(defRef)) =>
      val claim =constant(p.property)(defRef())
      val just = p.just map (translate_Justification(_, claim))
      val name = LocalName(p.property+"_of_"+defRef.name.toString)
      makeConstantInContext(name, Some(Univ(1)), just)
    case _ => throw ImplementationError("Invalid parsing assumption: Found property outside of definition block, which isn't sethood. ")
  }
}

object subitemTranslator {
  def notToplevel = new TranslatingError("This kind of declaration should not occur on toplevel. ")
  def translate_Reservation(reservation: Reservation) = { Nil }
  def translate_Definition_Item(definition_Item: Definition_Item): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    definition_Item.blockKind() match {
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
      val provenSentence = sbi.provenSentence
      val Scheme_Head(_sch, _vars, _form, _provForm) = sbi.scheme_head
      val spelling = _sch.spelling match { case "" => "AnonymousScheme_"+TranslationController.incrementAndGetAnonymousSchemeCount().toString case str => str }
      _vars._segms foreach (segm => translateBindingVariables(segm)(defContext))
      val ass : List[Term] = _provForm.map(_._props map(translate_Claim(_)(defContext))) getOrElse Nil
      implicit val notC = makeNotationCont(spelling, 0, defContext.args.length, true)
      val (p, prf) = translate_Proved_Claim(provenSentence)(defContext)
      TranslationController.articleStatistics.incrementStatisticsCounter("scheme")
      List(SchemeDefinitionInstance(gn.name, defContext.args map (_.tp.get), ass, p, prf))
  }
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
      val theoremDecl = makeConstantInContext(gn.name, Some(claim), Some(proof))(defContext = defCtx)
      theoremDecl
    case type_Changing_Statement: Type_Changing_Statement => translate_Type_Changing_Statement(type_Changing_Statement)
    case theorem_Item: Theorem_Item => translate_Theorem_Item(theorem_Item)
    case regular_Statement: Regular_Statement => translate_Regular_Statement(regular_Statement)
  }
  def translate_Conclusion(conclusion: Conclusion) = { Nil }
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement)(implicit defContext: => DefinitionContext) : Declaration with HasType with HasDefiniens with HasNotation = {
    val gn = makeNewGlobalName("Type_Changing_Statement", incrementAndGetAnonymousTheoremCount())
    val (claim, proof) = translate_Proved_Claim(type_Changing_Statement.prfClaim)(defContext)
    makeConstantInContext(gn.name, Some(claim), Some(proof))(defContext = defContext)
  }
  def translate_Theorem_Item(theorem_Item: Theorem_Item)(implicit defContext: DefinitionContext) = {
    implicit val gn = theorem_Item.referencedLabel
    val (claim, proof) = translate_Proved_Claim(theorem_Item.prfClaim)
    TranslationController.articleStatistics.incrementStatisticsCounter("thm")
    makeConstantInContext(gn.name, Some(claim), Some(proof))
  }
  def translate_Choice_Statement(choice_Statement: Choice_Statement)(implicit defContext: DefinitionContext): (Context, (Term, Term)) = {
    val vars: Context = choice_Statement._qual._children.flatMap(translate_Context(_))
    val facts = translate_Proved_Claim(choice_Statement.prfClaim)
    (vars, facts)
  }
  def translate_Regular_Statement(regular_Statement: Regular_Statement)(implicit defContext: DefinitionContext) = {
    val gn = makeNewGlobalName("Regular-Statement", incrementAndGetAnonymousTheoremCount())
    val (claim, proof) = translate_Proved_Claim(regular_Statement.prfClaim)
    val theoremDecl = makeConstantInContext(gn.name, Some(claim), Some(proof))
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
    lazy val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
    implicit lazy val (kind: String, ret: Option[Term]) = redefinableLabeledDefinition match {
      case ad: Attribute_Definition => ("attr", None)
      case fd: Functor_Definition => ("func", fd._tpSpec map (tpSpec => translate_Type(tpSpec._types)) orElse defn.map(d => inferType(d.someCase)))
      case pd :Predicate_Definition => ("pred", None)
    }
    val firstRes = if (redefinableLabeledDefinition.redefinition && defn.isEmpty) {
        //In this case we have a type redefinition
        translate_Redefine(pat, ln, ret, defn, argNum, argTps)
    } else {
      //We either don't have a redefinition
      //or we define both a new type and definien, i.e. we give a completely new definition
      def corrConds(implicit kind: String, retO: Option[Term] = None) = defContext.corr_conds.map(jcc => translate_def_correctness_condition(jcc._cond, jcc._just, defn.get, kind, ret))
      def get (cc: CorrectnessConditions): Term = corrConds zip defContext.corr_conds find (_._2._cond == cc) map (_._1) getOrElse (translate_def_correctness_condition(cc, None, defn.get, kind, ret))
      lazy val consistencyProof = get (consistency())
      lazy val coherenceProof = get (coherence())
      lazy val existenceProof = get (existence())
      lazy val uniquenessProof = get (uniqueness())
      kind match {
        case "attr" =>
          val motherTp = argTps.last
          TranslationController.articleStatistics.incrementStatisticsCounter
          defn.get match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes, consistencyProof)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              IndirectPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes, consistencyProof)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              DirectCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              IndirectCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes)
          }
        case "func" =>
          TranslationController.articleStatistics.incrementStatisticsCounter
          defn.get match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectPartialFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, defRes, consistencyProof, coherenceProof)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              IndirectPartialFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, defRes, consistencyProof, existenceProof, uniquenessProof)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              DirectCompleteFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, coherenceProof)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              IndirectCompleteFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, existenceProof, uniquenessProof)
          }
        case "pred" =>
          TranslationController.articleStatistics.incrementStatisticsCounter
          defn.get match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectPartialPredicateDef(name, argNum, argTps, defn.get.caseNum, cases, caseRes, defRes, consistencyProof)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              DirectCompletePredicateDef(name, argNum, argTps, defn.get.caseNum, cases, caseRes)
            case _ => throw new TranslatingError("Predicate definition can't be indirect. ")
          }
      }
    }
    firstRes :: ((label map(_ => makeConstantInContext(redefinableLabeledDefinition.globalName.name, None, Some(OMS(gn)))) map(List(_)) getOrElse Nil))
  }
  def translate_Structure_Definition(strDef: Structure_Definition)(implicit defContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    val l = defContext.args.length
    implicit var selectors: List[(Int, VarDecl)] = Nil
    val substrTps: List[Term] = strDef._ancestors._structTypes.map(translate_Type)
    val Structure_Patterns_Rendering(_aggrFuncPat, _forgetfulFuncPat, _strFuncPat, Selectors_List(_selectorFuncPat)) = strDef._rendering
    val substrs = (substrTps :+ ApplyGeneral(OMS(computeGlobalName(_strFuncPat)), defContext.args map (_.toTerm))) flatMap(MizarStructure.getTransitiveAncestors(_))
    val n = substrs.length
    var substitutions : List[Sub] = Nil
    val declarationPath = strDef._strPat.globalPatternName
    val forgNot = translate_Pattern(_forgetfulFuncPat)._3
    val nots@strNot::aggrNot::selNots  = _strFuncPat::_aggrFuncPat::_selectorFuncPat map(translate_Pattern(_)) map(t=> (t._1, t._3))
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
    StructureInstance(declarationPath, l, defContext.args, n, substrs, m, Context.list2context(fieldDecls) ^ namedDefArgsSubstition(), nots.map(_._2) :+ forgNot)
  }
  def translate_Constant_Definition(constant_Definition: Constant_Definition)(implicit defContext: => DefinitionContext): List[Constant] = {
    constant_Definition._children map { eq =>
      val name = LocalName(eq._var.varAttr.copy(kind = "Constant").toIdentifier)
      val dfU = translate_Term(eq._tm)(defContext)
      val argsContext = defContext.getLocalBindingVars filter (dfU.freeVars contains _.name)
      val df = LambdaOrEmpty(defContext.args++argsContext, dfU ^ namedDefArgsSubstition()(defContext))
      if (defContext.withinProof) {
        defContext.addLocalDefinition(name, df)
      }
      val notC = makeNotationCont(eq._var.varAttr.spelling, 0, 0)
      makeConstantInContext(name, None, Some(df))(notC, defContext)
    }
  }
  def translate_Mode_Definition(mode_Definition: Mode_Definition)(implicit defContext: DefinitionContext) = {
    val (name, declarationPath, notC) = translate_Pattern(mode_Definition._pat)
    implicit val notCon = notC
    mode_Definition._mode match {
      case Expandable_Mode(_tp) =>
        val tp = translate_Type(_tp)
        makeConstantInContext(declarationPath.name, Some(tp), None)
      case stm @ Standard_Mode(_tpSpec, _def) =>
        val name = declarationPath.name
        val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
        val defnO = _def map(translate_Definiens(_, true))

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
          lazy val corrConds = defContext.corr_conds.map(jcc => translate_def_correctness_condition(jcc._cond, jcc._just, defn, "mode"))
          defn match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectPartialModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, defRes, corrConds head, corrConds.last)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              IndirectPartialModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, defRes, corrConds head, corrConds.last)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              DirectCompleteModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, corrConds.last)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              IndirectCompleteModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, corrConds.last)
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
    //placeholder terms are numbered starting at 1
    val args: Context = private_Functor_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map({case (tp, i) => OMV("placeholder_"+(i+1)) % tp})
    val tp = PiOrEmpty(args, any)
    val dfBody = translate_Term(private_Functor_Definition._tm)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    if (defContext.withinProof) defContext.addLocalDefinition(gn.name, df)
    makeConstantInContext(gn.name, Some(tp), Some(df))(defContext = defContext)
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
    //placeholder terms are numbered starting at 1
    val args: Context = private_Predicate_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map({case (tp, i) => OMV("placeholder_"+(i+1)) % tp})
    val tp = PiOrEmpty(args, prop)
    val dfBody = translate_Formula(private_Predicate_Definition._form)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    if (defContext.withinProof) defContext.addLocalDefinition(gn.name, df)
    makeConstantInContext(gn.name, Some(tp), Some(df))(defContext = defContext)
  }
  def translate_Redefine(p: RedefinablePatterns, ln: LocalName, ret: Option[Term], defn: Option[CaseByCaseDefinien], argNum: Int, argTps: List[Term])(implicit kind: String, notC: NotationContainer) = {
    val origGn = globalLookup(p, true)
    val origDD = TranslationController.controller.getO(origGn)
    lazy val tp = kind match {
      case "func" => ret map (r => Pi(LocalName(argsVarName), nTerms(argNum), r))
      case "attr" => Some(Pi(LocalName(argsVarName), nTerms(argNum), Arrow(any, prop)))
      case "pred" => Some(Pi(LocalName(argsVarName), nTerms(argNum), prop))
    }
    val origArgTps = origDD match {
      case Some(AttributeDefinitionInstance(_, _, origArgTps, _, _, _, _, _)) if (kind == "attr") => Some(origArgTps)
      case Some(PredicateDefinitionInstance(_, _, origArgTps, _, _, _, _, _)) if (kind == "pred") => Some(origArgTps)
      case Some(FunctorDefinitionInstance(_, _, origArgTps, _, _, _, _, _, _)) if (kind == "func") => Some(origArgTps)
      case _ => None
    }
    if (origArgTps.map(_.length) == Some(argNum)) {
        val df = Pi(LocalName(argsVarName), nTerms(argNum), (ApplyGeneral(OMS(origGn), (0 until argNum).toList map (i => Index(OMV(argsVarName),  OMI(i))))))
        makeConstant(ln, tp, Some(df))(notC)
      } else {
        //We have extra arguments and would have to match the types
        makeConstant(ln, tp, None)(notC)
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
        val coherenceCond = definitionContext.corr_conds.find(_._cond == syntax.coherence()) getOrElse Correctness_Condition(coherence(), None)
        val coherenceProof = translate_reg_correctness_condition(coherenceCond._just, "condRegistration", adjs.length, Some(tp), None, Some(adjs), Some(ats), None)
        Some(ConditionalRegistration(name, definitionContext.args map(_.tp.get), tp, adjs, ats, coherenceProof))
      case Existential_Registration(_adjClust, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_adjClust)
        //TODO:
        val name = LocalName("existReg_"+TranslationController.articleStatistics.numRegistrs)
        val existenceCond = definitionContext.corr_conds.find(_._cond == syntax.existence()) getOrElse Correctness_Condition(existence(), None)
        val existenceProof = translate_reg_correctness_condition(existenceCond._just, "existRegistration", adjs.length, Some(tp), None, Some(adjs), None, None)
        Some(ExistentialRegistration(name, definitionContext.args map(_.tp.get), tp, adjs, existenceProof))
      case Functorial_Registration(_aggrTerm, _adjCl, _tp) =>
        val tm = translate_Term(_aggrTerm)
        val adjs = attributeTranslator.translateAttributes(_adjCl)
        val isQualified = _tp.isDefined
        val tp = _tp map translate_Type getOrElse({
          inferType(tm)})
        val name = LocalName("funcReg_"+TranslationController.articleStatistics.numRegistrs)
        val coherenceCond = definitionContext.corr_conds.find(_._cond == syntax.coherence()) getOrElse Correctness_Condition(coherence(), None)
        def coherenceProof(kind: String) = translate_reg_correctness_condition(coherenceCond._just, kind+"FuncRegistration", adjs.length, Some(tp), Some(tm), Some(adjs), None, None)
        Some(if (isQualified) {
          QualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs, coherenceProof("qual"))
        } else {
          UnqualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs, coherenceProof("unqual"))
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
      val (c, d) = (ApplyGeneral(f, fparams), ApplyGeneral(g, gparams))
      val compatibility: Term = defContext.corr_conds.find(_._cond.kind == syntax.compatibility().kind).map({comp: Correctness_Condition =>
        val (aL, bL) = translatedLociEqns.unzip
        val List(a, b) = List(aL, bL) map(Sequence(_))
        translate_reg_correctness_condition(comp._just, "identify", translatedLociEqns.length, None, None, None, None, Some(List(a, b, c, d)))
      }).get
      mmtwrapper.Identify(name, defContext.args map(_.tp.get), translatedLociEqns, c, d, compatibility)
  }
  def translate_Reduction(identify: syntax.Reduction)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = identify match {
    case syntax.Reduction(_predecessor, _successor) =>

      val num = incrementAndGetReduceCount()
      val name = LocalName("reduce"+num)
      val predecessor = translate_Term(_predecessor)
      val successor = translate_Term(_successor)
      val reducibility: Term = defContext.corr_conds.find(_._cond.kind == syntax.reducibility().kind).map({comp: Correctness_Condition =>
        translate_reg_correctness_condition(comp._just, "reduce", 0, None, None, None, None, Some(List(predecessor, successor)))
      }).get
      mmtwrapper.Reduction(name, defContext.args map(_.tp.get), predecessor, successor, reducibility)
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
        case Loci_Declaration(_qualSegms, _conds) =>
          _qualSegms._children foreach {segm =>
            defContext.addArguments(translate_Context(segm)(defContext))
            _conds map (translate_Claim(_)(defContext)) map defContext.addAssumption
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
          implicit var corr_conds: List[Correctness_Condition] = Nil
          implicit var props: List[MizarProperty] = Nil
          items.drop(ind).foreach(_._subitem match {
            case cc@Correctness_Condition(_cor, _justO) => corr_conds :+= cc
            case Correctness(_cor, _just) => corr_conds :::= _cor._cond map (Correctness_Condition(_, Some(_just)))
            case prop: Property => props :+= prop.matchProperty()
            case _ =>
          })
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
      case (id: syntax.Reduction, defContext: DefinitionContext) => List(translate_Reduction(id)(defContext))
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