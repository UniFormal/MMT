package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api._
import notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mizar.newxml._
import mmtwrapper._
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
import info.kwarc.mmt.mizar.newxml.mmtwrapper.PatternUtils._
import info.kwarc.mmt.mizar.newxml.syntax.Utils._
import clusterTranslator._
import definiensTranslator._
import info.kwarc.mmt.mizar.newxml.mmtwrapper.MizSeq.{Index, OMI, Sequence, nTerms}
import info.kwarc.mmt.mizar.newxml.translator.correctnessConditionTranslator._
import info.kwarc.mmt.mizar.newxml.translator.TranslationController._
import info.kwarc.mmt.mizar.newxml.translator.statementTranslator.translate_Choice_Statement
import JustificationTranslator._
import propertyTranslator._
import nymTranslator._
import patternTranslator._
import itemTranslator.add

object correctnessConditionTranslator {
  def translate_def_correctness_condition(cor_cond: CorrectnessConditions, just: Option[Justification], defn: CaseByCaseDefinien, kind : String, retO: Option[Term] = None)(implicit defContext: DefinitionContext) = {
    val pattern = kind match {
      case "funct" => "FuncDef"
      case "pred" | "attribute" => "PredAttrDef"
      case "mode" => "ModeDef"
    }
    val arguments = defContext.args map (_.tp.get)
    implicit val args = arguments map (lambdaBindArgs(_)(arguments))
    val argNum = OMI(args.length)
    val ret = retO map(List(_)) getOrElse Nil
    val caseNum = OMI(defn.caseNum)
    val cases = Sequence(defn.cases map (lambdaBindArgs(_, true)))
    val caseRes = Sequence(defn.caseRes map (lambdaBindArgs(_, true)))
    val defRes = List(lambdaBindArgs(defn.defRes))
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
    lazy val tm = tmO map (lambdaBindArgs(_, true)) map (List(_)) getOrElse Nil
    lazy val a = Sequence (aO.get)
    lazy val qb = bO map (b => List(OMI(b.length), Sequence(b))) getOrElse Nil

    val furtherParams = if (List("identify", "reduce") contains kind) IdentifyPairs.get map (lambdaBindArgs(_, true)) else t::tm:::a::qb
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
  private def addLocalDefinition(n: LocalName, defn: Term) = {
    assert (withinProof)
    val toAdd = (n, defn)
    localDefinitions = localDefinitions.head.:+(toAdd) :: localDefinitions.tail
  }
  def addLocalDefinitionInContext(n: LocalName, defn: Term) = addLocalDefinition(n, lambdaBindContext(defn)(this))
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

sealed abstract class CaseByCaseDefinien(isDirect: Boolean) {
  def cases: List[Term]
  def caseRes: List[Term]
  def caseNum = cases.length
  def defRes: Term
  def direct: Boolean = isDirect
  def prefixes = if (direct) "direct" else "indirect"
}
case class DirectCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], defRes: Term) extends CaseByCaseDefinien(true)
object DirectCaseByCaseDefinien {
  def apply(tm: Term): DirectCaseByCaseDefinien = DirectCaseByCaseDefinien(Nil, Nil, tm)
}
case class IndirectCaseByCaseDefinien(cases: List[Term], caseRes: List[Term], defRes: Term) extends CaseByCaseDefinien(false)
object IndirectCaseByCaseDefinien {
  def apply(tm: Term): IndirectCaseByCaseDefinien = IndirectCaseByCaseDefinien(Nil, Nil, Lam("it", any, tm))
}

object definiensTranslator {
  def translate_Definiens(defs:Definiens, isModeDef: Boolean = false)(implicit defContext: DefinitionContext): CaseByCaseDefinien = {
    translate_CaseBasedExpr(defs._expr, isModeDef)
  }
  def translate_CaseBasedExpr(defn:CaseBasedExpr, isModeDef: Boolean = false)(implicit defContext: DefinitionContext): CaseByCaseDefinien = {
    defn.check()
    if (defn.isSingleCase()) {
      val defRes = translate_Expression(defn.singleCasedExpr._expr.get)
      if (defRes.freeVars.contains(LocalName("it"))) {
        IndirectCaseByCaseDefinien(defRes)
      } else {
        DirectCaseByCaseDefinien(defRes)
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
      IndirectCaseByCaseDefinien(cases, caseRes, Lam("it", any, defRes.get))
    } else {
      DirectCaseByCaseDefinien(cases, caseRes, defRes.get)
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
      case c if c == shortKind(FunctorKind()) => gn / "funct"
      case c if c == shortKind(AttributeKind()) => gn / "attribute"
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
   * @param pse the pattern shaped expression to dereference
   * @return
   */
  def translate_Referencing_Pattern(pse: Pattern_Shaped_Expression)(implicit defContext: DefinitionContext): (LocalName, Term, NotationContainer, List[Term]) = {
    val pat: Patterns = pse._pat
    val params = pat._locis.flatMap(_._loci map translate_Locus)
    val (name, gn, notC) = translate_Pattern(pat, true)
    val mainDecl = OMS(globalLookup(pat))
    (name, mainDecl, notC, params)
  }
}

object propertyTranslator {
  def translate_JustifiedProperty(justProp: MizarProperty, definitionRef: Option[GlobalName], defRefNot: Option[String] = None)(implicit definitionContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = (justProp, definitionRef) match {
    case (Sethood(_just, Some(_tp)), None) =>
      val tp = translate_Type(_tp)
      val claim = constant("sethood")(tp)
      val just = _just map (translate_Justification(_, claim))
      val backsubstitutedType = tp ^ definitionContext.args.zipWithIndex.map({case (vd, ind) => OMV(LocalName(argsVarName) / ind.toString) / vd.toTerm})
      val name = Utils.makeGlobalName(currentAid, "sethood_of_", TranslationController.controller.presenter.asString(backsubstitutedType)).name
      makeConstant(name, Some(piBindContext(proof(claim))), just)
    case (p: MizarProperty, Some(defRef)) =>
      val claim =constant(p.property)(defRef())
      val just = p.just map (translate_Justification(_, claim)(definitionContext, false))
      val defName = defRefNot.getOrElse(defRef.name.toString)
      val fullDefName = Utils.makeGlobalName(currentAid, p.property+"_of_", defName).name
      val declName = if (currentTheory.domain.contains(fullDefName)) defRef.name.toString else defName
      val name = Utils.makeGlobalName(currentAid, p.property+"_of_", declName).name
      makeConstant(name, Some(proof(claim)), just)
    case _ => throw ImplementationError("Invalid parsing assumption: Found property outside of definition block, which isn't sethood. ")
  }
}

object subitemTranslator {
  def notToplevel = new TranslatingError("This kind of declaration should not occur on toplevel. ")
  def translate_Reservation(reservation: Reservation) = { Nil }
  def translate_Definition_Item(definition_Item: Definition_Item): Unit = {
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
      val spelling = _sch.spelling match { case "" => "AnonymousScheme_"+articleData.incrementAndGetAnonymousSchemeCount().toString case str => str }
      _vars._segms foreach (segm => translateBindingVariables(segm)(defContext))
      val ass : List[Term] = _provForm.map(_._props map(translate_Claim(_)(defContext))) getOrElse Nil
      implicit val notC = makeNotationCont(spelling, 0, defContext.args.length, true)
      val (p, prf) = translate_Proved_Claim(provenSentence)(defContext)
      articleData.articleStatistics.incrementStatisticsCounter("scheme")
      List(SchemeDefinitionInstance(gn.name, defContext.args map (_.tp.get), ass, p, prf))
  }
}

object nymTranslator {
  def translate_Nym(nym:Nyms)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = {
    val oldPatRef = nym._patRefOld
    val newPat: Patterns = nym._patNew
    val (name, newName, notC) = translate_Pattern(newPat)
    val (_, mainDecl, _, _) = translate_Referencing_Pattern(oldPatRef)
    val allArgs = defContext.args.map(_.tp.get)
    articleData.articleStatistics.incrementStatisticsCounter("nym")
    SynonymicNotation(newName.name, allArgs.length, allArgs, mainDecl)(notC, nym.defKind)
  }
}

object statementTranslator {
  def translate_Statement(st:Statement with TopLevel)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = st match {
    case choice_Statement: Choice_Statement =>
      val (addArgs, (claim, proof)) = translate_Choice_Statement(choice_Statement)
      val gn = makeNewGlobalName("Choice_Statement", articleData.incrementAndGetAnonymousTheoremCount())
      implicit val defCtx = defContext.copy(args = defContext.args ++ addArgs, assumptions = defContext.assumptions :+ claim)
      val theoremDecl = makeConstantInContext(gn.name, Some(claim), Some(proof))(defContext = defCtx)
      theoremDecl
    case type_Changing_Statement: Type_Changing_Statement => translate_Type_Changing_Statement(type_Changing_Statement)
    case theorem_Item: Theorem_Item => translate_Theorem_Item(theorem_Item)
    case regular_Statement: Regular_Statement => translate_Regular_Statement(regular_Statement)
  }
  def translate_Conclusion(conclusion: Conclusion) = { Nil }
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement)(implicit defContext: => DefinitionContext) : Declaration with HasType with HasDefiniens with HasNotation = {
    val gn = makeNewGlobalName("Type_Changing_Statement", articleData.incrementAndGetAnonymousTheoremCount())
    val (claim, proof) = translate_Proved_Claim(type_Changing_Statement.prfClaim)(defContext)
    makeConstantInContext(gn.name, Some(claim), Some(proof))(defContext = defContext)
  }
  def translate_Theorem_Item(theorem_Item: Theorem_Item)(implicit defContext: DefinitionContext) = {
    implicit val gn = theorem_Item.referencedLabel
    val (claim, proof) = translate_Proved_Claim(theorem_Item.prfClaim)
    articleData.articleStatistics.incrementStatisticsCounter("thm")
    makeConstantInContext(gn.name, Some(claim), Some(proof))
  }
  def translate_Choice_Statement(choice_Statement: Choice_Statement)(implicit defContext: DefinitionContext): (Context, (Term, Term)) = {
    val vars: Context = choice_Statement._qual._children.flatMap(translate_Context(_))
    val facts = translate_Proved_Claim(choice_Statement.prfClaim)
    (vars, facts)
  }
  def translate_Regular_Statement(regular_Statement: Regular_Statement)(implicit defContext: DefinitionContext) = {
    val gn = makeNewGlobalName("Regular-Statement", articleData.incrementAndGetAnonymousTheoremCount())
    val (claim, proof) = translate_Proved_Claim(regular_Statement.prfClaim)
    val theoremDecl = makeConstantInContext(gn.name, Some(claim), Some(proof))
    theoremDecl
  }
}

object definitionTranslator {
  def translate_Definition(defn:Definition)(implicit defContext: => DefinitionContext): Unit = {
    val translatedDecls: List[Declaration with HasType with HasDefiniens with HasNotation] = defn match {
      case d: Structure_Definition => translate_Structure_Definition(d)(defContext)
      case cd: Constant_Definition => translate_Constant_Definition(cd)(defContext)
      case rld: RedefinableLabeledDefinition => translate_Redefinable_Labelled_Definition(rld)(defContext)
      case d: Private_Functor_Definition => List(translate_Private_Functor_Definition(d)(defContext))
      case d: Private_Predicate_Definition => List(translate_Private_Predicate_Definition(d)(defContext))
      case md: Mode_Definition => List(translate_Mode_Definition(md)(defContext))
    }
    val declRef = Some(translatedDecls.head.path)
    lazy val justProps = defContext.props.map(translate_JustifiedProperty(_, declRef, defn.pat map (_.patternAttrs.spelling))(defContext))
    if (defContext.withinProof) justProps map (d => defContext.addUsedFact(d.tp.get, d.df.get))
    (translatedDecls:::(if (!defContext.withinProof) justProps else Nil)) foreach (itemTranslator.add(_)(defContext))
  }
  private def translate_Redefinable_Labelled_Definition(redefinableLabeledDefinition: RedefinableLabeledDefinition)(implicit defContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    redefinableLabeledDefinition.check
    val (pat, _defn, label) = (redefinableLabeledDefinition._pat, redefinableLabeledDefinition._def, redefinableLabeledDefinition.mmlIdO)
    val (ln, gn, notC) = translate_Pattern(pat)
    val path = gn
    val name = path.name
    implicit val notCon = notC
    val defn = _defn.map(translate_Definiens(_))
    lazy val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
    implicit lazy val (kind: String, ret: Option[Term]) = redefinableLabeledDefinition match {
      case ad: Attribute_Definition => ("attribute", None)
      case fd: Functor_Definition => ("funct", fd._tpSpec map (tpSpec => translate_Type(tpSpec._types)) orElse defn.map(d => inferType(d.defRes)) map(lambdaBindArgs(_)(defContext.args map(_.toTerm))))
      case pd :Predicate_Definition => ("pred", None)
    }
    val firstRes = if (redefinableLabeledDefinition.redefinition && defn.isEmpty) {
        //In this case we have a type redefinition
        translate_Redefine(pat, ln, ret, defn, argNum, argTps)
    } else {
      //We either don't have a redefinition
      //or we define both a new type and definien, i.e. we give a completely new definition
      lazy val corrConds = defContext.corr_conds.map(jcc => translate_def_correctness_condition(jcc._cond, jcc._just, defn.get, kind, ret))
      def get (cc: CorrectnessConditions): Term = corrConds zip defContext.corr_conds find (_._2._cond == cc) map (_._1) getOrElse translate_def_correctness_condition(cc, None, defn.get, kind, ret)
      lazy val consistencyProof = get (consistency())
      lazy val coherenceProof = get (coherence())
      lazy val existenceProof = get (existence())
      lazy val uniquenessProof = get (uniqueness())
      kind match {
        case "attribute" =>
          val motherTp = argTps.last
          articleData.articleStatistics.incrementStatisticsCounter
          defn.get match {
            case DirectCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes, consistencyProof)
            case IndirectCaseByCaseDefinien(cases, caseRes, defRes) =>
              IndirectAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes, consistencyProof)
          }
        case "funct" =>
          articleData.articleStatistics.incrementStatisticsCounter
          defn.get match {
            case DirectCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, defRes, consistencyProof, coherenceProof)
            case IndirectCaseByCaseDefinien(cases, caseRes, defRes) =>
              IndirectFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, defRes, consistencyProof, existenceProof, uniquenessProof)
          }
        case "pred" =>
          articleData.articleStatistics.incrementStatisticsCounter
          defn.get match {
            case DirectCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectPredicateDef(name, argNum, argTps, defn.get.caseNum, cases, caseRes, defRes, consistencyProof)
            case _ => throw new TranslatingError("Predicate definition can't be indirect. ")
          }
      }
    }
    firstRes :: (label map(_ => makeConstantInContext(redefinableLabeledDefinition.globalName.name, None, Some(OMS(gn)))) map(List(_)) getOrElse Nil)
  }
  def translate_Structure_Definition(strDef: Structure_Definition)(implicit defContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    val l = defContext.args.length
    implicit var selectors: List[(Int, VarDecl)] = Nil
    val ancestorTps: List[Term] = strDef._ancestors._structTypes.map(translate_Type)
    val Structure_Patterns_Rendering(_aggrFuncPat, _forgetfulFuncPat, _strFuncPat, Selectors_List(_selectorFuncPat)) = strDef._rendering
    val n = ancestorTps.length
    var substitutions : List[Sub] = Nil
    implicit val declarationPath = strDef._pat.globalPatternName
    val nots@strNot::aggrNot::forgNot::strictNot::selNots = strDef._pat::_aggrFuncPat::_forgetfulFuncPat::_strFuncPat::_selectorFuncPat map(translate_Pattern(_)) map(t=> (t._1, t._3))
    def translate_Field_Segments(field_Segments: Field_Segments)(implicit defContext: DefinitionContext) : List[VarDecl] = field_Segments._fieldSegments flatMap {
      field_Segment: Field_Segment =>
			val tp = translate_Type(field_Segment._tp)
      field_Segment._selectors._loci.reverse map { selector: Selector =>
        val selName = OMV(selector.spelling)//translate_Locus(selector._loci)
        val sel = (selector.nr, selName % tp)
        selectors ::= sel
        substitutions ::= selName / OMS(structureSelectorPath(selName.name))
        (sel._2 ^ substitutions).copy(not = selNots.find(_._1 == sel._2.name).map(_._2.getAllNotations.head))
      }
    }
    val fieldDecls = translate_Field_Segments(strDef._fieldSegms)
    val m = fieldDecls.length

    articleData.articleStatistics.incrementStatisticsCounter("struct")
    StructureInstance(l, defContext.args, n, ancestorTps, m, Context.list2context(fieldDecls) ^ namedDefArgsSubstition(), nots.map(_._2))
  }
  private def translate_Constant_Definition(constant_Definition: Constant_Definition)(implicit defContext: => DefinitionContext): List[Constant] = {
    constant_Definition._children map { eq =>
      val name = LocalName(eq._var.varAttr.copy(kind = "Constant").toIdentifier)
      val dfU = translate_Term(eq._tm)(defContext)
      val argsContext = defContext.getLocalBindingVars filter (dfU.freeVars contains _.name)
      val df = LambdaOrEmpty(defContext.args++argsContext, dfU ^ namedDefArgsSubstition()(defContext))
      if (defContext.withinProof) {
        defContext.addLocalDefinitionInContext(name, df)
      }
      val notC = makeNotationCont(eq._var.varAttr.spelling, 0, 0)
      makeConstantInContext(name, None, Some(df))(notC, defContext)
    }
  }
  def translate_Mode_Definition(mode_Definition: Mode_Definition)(implicit defContext: DefinitionContext) = {
    val (ln, declarationPath, notC) = translate_Pattern(mode_Definition._pat)
    val name = ln / "mode"
    implicit val notCon = notC
    mode_Definition._mode match {
      case Expandable_Mode(_tp) =>
        val tp = translate_Type(_tp)
        makeConstantInContext(name, Some(tp), None)
      case stm @ Standard_Mode(_tpSpec, _def) =>
        val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
        val defnO = _def map(translate_Definiens(_, true))
        if (defnO.isEmpty) {
          if (_tpSpec.isDefined) {
            makeConstantInContext(name, Some(Pi(LocalName(argsVarName), nTerms(argNum), Arrow(any, prop))),
              Some(Lam(argsVarName, nTerms(argNum), translate_Type(_tpSpec.get._types))))
          } else {
            assert(mode_Definition._redef.occurs)
            translate_Redefine(mode_Definition._pat, name, None, None, argNum, argTps)("mode", notC)
          }
        } else {
          val defn = defnO.get
          articleData.articleStatistics.incrementStatisticsCounter("mode")
          lazy val corrConds = defContext.corr_conds.map(jcc => translate_def_correctness_condition(jcc._cond, jcc._just, defn, "mode"))
          defn match {
            case DirectCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectModeDefinition(ln, argNum, argTps, defn.caseNum, cases, caseRes, defRes, corrConds head, corrConds.last)
            case IndirectCaseByCaseDefinien(cases, caseRes, defRes) =>
              IndirectModeDefinition(ln, argNum, argTps, defn.caseNum, cases, caseRes, defRes, corrConds head, corrConds.last)
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
    val gn = makeNewGlobalName("Private-Functor", private_Functor_Definition._var.varAttr.serialnr)
    //placeholder terms are numbered starting at 1
    val args: Context = private_Functor_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map {case (tp, i) => OMV("placeholder_"+(i+1)) % tp}
    val tp = PiOrEmpty(args, any)
    val dfBody = translate_Term(private_Functor_Definition._tm)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    if (defContext.withinProof) defContext.addLocalDefinitionInContext(gn.name, df)
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
    val gn = makeNewGlobalName("Private-Predicate", private_Predicate_Definition._var.varAttr.serialnr)
    //placeholder terms are numbered starting at 1
    val args: Context = private_Predicate_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map {case (tp, i) => OMV("placeholder_"+(i+1)) % tp}
    val tp = PiOrEmpty(args, prop)
    val dfBody = translate_Formula(private_Predicate_Definition._form)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    if (defContext.withinProof) defContext.addLocalDefinitionInContext(gn.name, df)
    makeConstantInContext(gn.name, Some(tp), Some(df))(defContext = defContext)
  }
  def translate_Redefine(p: Patterns, ln: LocalName, ret: Option[Term], defn: Option[CaseByCaseDefinien], argNum: Int, argTps: List[Term])(implicit kind: String, notC: NotationContainer) = {
    val origGn = globalLookup(p, true)
    TranslationController.processDependencyTheory(origGn.module)
    val origDD = TranslationController.controller.getO(origGn)
    lazy val tp = kind match {
      case "funct" => ret map (r => Pi(LocalName(argsVarName), nTerms(argNum), r))
      case "attribute" | "mode" => Some(Pi(LocalName(argsVarName), nTerms(argNum), Arrow(any, prop)))
      case "pred" => Some(Pi(LocalName(argsVarName), nTerms(argNum), prop))
    }
    val origArgTpsO = origDD match {
      case Some(AttributeDefinitionInstance(_, _, origArgTps, _, _, _, _, _)) if kind == "attribute" => Some(origArgTps)
      case Some(PredicateDefinitionInstance(_, _, origArgTps, _, _, _, _)) if kind == "pred" => Some(origArgTps)
      case Some(FunctorDefinitionInstance(_, _, origArgTps, _, _, _, _, _, _)) if kind == "funct" => Some(origArgTps)
      case Some(ModeDefinitionInstance(_, _, origArgTps, _, _, _, _)) if kind == "mode" => Some(origArgTps)
      case Some(NymicNotation(_, _, _, origArgTps, _)) => Some(origArgTps)
      //the only predicates (there are no functors or attributes) in hidden take two terms as arguments
      case _ if origGn.module == HiddenTh => Some(List(any, any))
      case _ => None
    }
    origArgTpsO flatMap {
      origArgTps: List[Term] =>
        val origLength = origArgTps.length
        val addArgsLength = argNum - origLength
        if (addArgsLength < 0) {
          println ("Error: The looked up original "+kind+" definition to redefine (without new definien) seems to have "+addArgsLength+" more arguments than this one (which should never happen). \nFor now, we record this definition without definien. ")
          None
        } else {
          val df = Pi(LocalName(argsVarName), nTerms(argNum), ApplyGeneral(OMS(origGn), (addArgsLength until argNum).toList map (i => Index(OMV(argsVarName),  OMI(i)))))
          Some (makeConstant(ln / LocalName(kind), tp, Some(df))(notC))
        }} getOrElse {
        //Failure to lookup the original definition, then we should at least record this declaration without its definien
        println ("Error: failure looking up original definition to redefine (without new definien), hence recording definition without definien. ")
        makeConstant(ln / LocalName(kind), tp, None)(notC)
      }
  }
}

object clusterTranslator {
  def translate_Registration_Subitem(reg: RegistrationSubitems)(implicit definitionContext: DefinitionContext): Unit = reg match {
    case cluster: Cluster => translate_Cluster(cluster)
    case rg: Registrations => translate_Registration(rg)
    case identfy: Identify => itemTranslator.add (translate_Identify(identfy))
    case reduct: Reduction => add (translate_Reduction(reduct))
  }
  def translate_Registration(reg: Registrations)(implicit definitionContext: DefinitionContext): Unit = {
    articleData.articleStatistics.incrementStatisticsCounter("registr")
    reg match {
      case Conditional_Registration(_attrs, _at, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_attrs)
        val ats = attributeTranslator.translateAttributes(_at)
        val name = makeNewGlobalName("condReg", articleData.articleStatistics.numRegistrs).name
        val coherenceCond = definitionContext.corr_conds.find(_._cond == syntax.coherence()) getOrElse Correctness_Condition(coherence(), None)
        val coherenceProof = translate_reg_correctness_condition(coherenceCond._just, "condRegistration", adjs.length, Some(tp), None, Some(adjs), Some(ats), None)
        add (ConditionalRegistration(name, definitionContext.args map(_.tp.get), tp, adjs, ats, coherenceProof))
      case Existential_Registration(_adjClust, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_adjClust)
        //TODO:
        val name = makeNewGlobalName("existReg", articleData.articleStatistics.numRegistrs).name
        val existenceCond = definitionContext.corr_conds.find(_._cond == syntax.existence()) getOrElse Correctness_Condition(existence(), None)
        val existenceProof = translate_reg_correctness_condition(existenceCond._just, "existRegistration", adjs.length, Some(tp), None, Some(adjs), None, None)
        add (ExistentialRegistration(name, definitionContext.args map(_.tp.get), tp, adjs, existenceProof))
      case Functorial_Registration(_aggrTerm, _adjCl, _tp) =>
        val tm = translate_Term(_aggrTerm)
        val adjs = attributeTranslator.translateAttributes(_adjCl)
        val isQualified = _tp.isDefined
        val tp = _tp map translate_Type getOrElse inferType(tm)
        val name = makeNewGlobalName("funcReg", articleData.articleStatistics.numRegistrs).name
        val coherenceCond = definitionContext.corr_conds.find(_._cond == syntax.coherence()) getOrElse Correctness_Condition(coherence(), None)
        def coherenceProof(kind: String) = translate_reg_correctness_condition(coherenceCond._just, kind+"FuncRegistration", adjs.length, Some(tp), Some(tm), Some(adjs), None, None)
        if (isQualified) {
          add (QualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs, coherenceProof("qual")))
        } else {
          add (UnqualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs, coherenceProof("unqual")))
        }
      case Property_Registration(_props, _just) => add (translate_JustifiedProperty(_props.matchProperty(Some(_just)), None)(definitionContext))
    }
  }
  def translate_Identify(identify: syntax.Identify)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = identify match {
    case syntax.Identify(_firstPat, _sndPat, _lociEqns) =>
      val translatedLociEqns = _lociEqns._lociEqns map {
        case Loci_Equality(_fstLoc, _sndLoc) => (translate_Locus(_fstLoc)(defContext), translate_Locus(_sndLoc)(defContext))
      }
      val num = articleData.incrementAndGetIdentifyCount()
      val name = LocalName("identify"+num)
      val (_, f, _, fparams) = translate_Referencing_Pattern(_firstPat)
      val (_, g, _, gparams) = translate_Referencing_Pattern(_sndPat)
      val (c, d) = (ApplyGeneral(f, fparams), ApplyGeneral(g, gparams))
      val compatibility: Term = defContext.corr_conds.find(_._cond.kind == syntax.compatibility().kind).map({comp: Correctness_Condition =>
        val (aL, bL) = translatedLociEqns.unzip
        val List(a, b) = List(aL, bL) map(Sequence(_))
        translate_reg_correctness_condition(comp._just, "identify", translatedLociEqns.length, None, None, None, None, Some(List(a, b, c, d)))
      }).get
      mmtwrapper.Identify(name, defContext.args map(_.tp.get), translatedLociEqns, c, d, compatibility)
  }
  def translate_Reduction(reduction: syntax.Reduction)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = reduction match {
    case syntax.Reduction(_predecessor, _successor) =>

      val num = articleData.incrementAndGetReduceCount()
      val name = makeNewGlobalName("reduce", num).name
      val predecessor = translate_Term(_predecessor)
      val successor = translate_Term(_successor)
      val reducibility: Term = defContext.corr_conds.find(_._cond.kind == syntax.reducibility().kind).map({comp: Correctness_Condition =>
        translate_reg_correctness_condition(comp._just, "reduce", 0, None, None, None, None, Some(List(predecessor, successor)))
      }).get
      mmtwrapper.Reduction(name, defContext.args map(_.tp.get), predecessor, successor, reducibility)
  }
  def translate_Cluster(cl:Cluster)(implicit definitionContext: DefinitionContext): Unit = {
    //TODO: Also translate the proofs of the correctness conditions
    cl._registrs foreach translate_Registration
  }
}
object blockTranslator {
  def collectSubitems[mainSort <: BlockSubitem](cls: Class[mainSort], block: Block) : List[(mainSort, DefinitionContext)] = {
    val items = block._items
    implicit var defContext = DefinitionContext.empty()

    def recurse(remainingItems: List[Subitem]):List[(mainSort, DefinitionContext)] = remainingItems match {
      case Nil => Nil
        case Loci_Declaration(_qualSegms, _conds)::tl =>
          _qualSegms._children foreach {segm =>
            defContext.addArguments(translate_Context(segm)(defContext))
            _conds map (translate_Claim(_)(defContext)) map defContext.addAssumption
          }
          recurse (tl)
        case (ass: Assumptions)::tl => defContext.addAssumption(translate_Assumption_Claim(ass))
          recurse (tl)
        case Assumption(ass)::tl => defContext.addAssumption(translate_Assumption_Claim(ass))
          recurse (tl)
        case (choice_Statement: Choice_Statement)::tl =>
          val (addArgs, addFacts) = translate_Choice_Statement(choice_Statement)
          defContext.addArguments(addArgs)
          defContext.addUsedFact(addFacts)
          recurse (tl)
        case (statement: Regular_Statement)::tl =>
          defContext.addUsedFact(translate_Proved_Claim(statement.prfClaim))
          recurse (tl)
        case defin::tl if cls.isInstance(defin) =>
          val defn = defin.asInstanceOf[mainSort]
          implicit var corr_conds: List[Correctness_Condition] = Nil
          implicit var props: List[MizarProperty] = Nil
          val (corProps, remaining) = tl.span {case _: Property_or_Correctness_Condition => true case _ => false}
          corProps.map(_.asInstanceOf[Property_or_Correctness_Condition]) foreach {
            case cc: Correctness_Condition => corr_conds :+= cc
            case Correctness(_cor, _just) => corr_conds :::= _cor._cond map (Correctness_Condition(_, Some(_just)))
            case prop: Property => props :+= prop.matchProperty
          }
          val correspondingDefContext = defContext.copy(corr_conds = corr_conds, props = props)
          (defn, correspondingDefContext)::recurse(remaining)
        case (prag: Pragma)::tl => recurse (tl)
        case defIt::tl => throw DeclarationTranslationError("Unexpected item of type " + defIt.shortKind+" found, in "+block.kind+" in file "+currentAid+".miz", defIt)
      }
      recurse (items map (_._subitem))
  }
  def translate_Definitional_Block(block:Block): Unit = {
    val definitionItems = collectSubitems[Definition](classOf[Definition], block)
    definitionItems foreach  (dd => translate_Definition(dd._1)(dd._2))
  }
  def translate_Registration_Block(block: Block): Unit = {
    val clusterItems = collectSubitems[RegistrationSubitems](classOf[RegistrationSubitems], block)
    clusterItems foreach (rd => translate_Registration_Subitem(rd._1)(rd._2))
  }
  def translate_Notation_Block(block: Block): Unit = {
    val notationItems = collectSubitems[Nyms](classOf[Nyms], block)
    notationItems foreach (nd => add (translate_Nym(nd._1)(nd._2))(nd._2))
  }
}