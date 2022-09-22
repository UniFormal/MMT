package info.kwarc.mmt.mizar.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api._
import notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mizar._
import mmtwrapper._
import MMTUtils._
import MizarPrimitiveConcepts._
import syntax._
import expressionTranslator._
import termTranslator._
import typeTranslator._
import contextTranslator.{translateLocisWithoutSubstitution, _}
import formulaTranslator._
import TranslatorUtils._
import claimTranslator._
import definitionTranslator._
import blockTranslator._
import info.kwarc.mmt.api.symbols.{Constant, Declaration, HasDefiniens, HasNotation, HasType}
import mmtwrapper.PatternUtils._
import syntax.Utils._
import clusterTranslator._
import definiensTranslator._
import mmtwrapper.MizSeq.{Index, OMI, Sequence, nTerms}
import translator.correctnessConditionTranslator._
import translator.TranslationController._
import translator.statementTranslator.{translate_Choice_Statement, translate_Type_Changing_Statement}
import JustificationTranslator._
import info.kwarc.mmt.api.parser.SourceRegion
import propertyTranslator._
import nymTranslator._
import patternTranslator._
import itemTranslator.add

object correctnessConditionTranslator {
  /**
   * TRanslate a corectness condition for a functor, predicate, attribute or mode definition
   * @param cor_cond
   * @param just
   * @param defn
   * @param kind
   * @param retO
   * @param defContext
   */
  def translate_def_correctness_condition(cor_cond: CorrectnessConditions, just: Option[Justification], defn: CaseByCaseDefinien, kind : PatternKinds, retO: Option[Term] = None)(implicit defContext: DefinitionContext) = {
    val pattern = kind match {
      case FunctorKind() => "FuncDef"
      case PredicateKind() | AttributeKind() => "PredAttrDef"
      case ModeKind() => "ModeDef"
      case _ => throw ImplementationError("Found pattern of wrong kind in correctness condition. ")
    }
    val arguments = defContext.args map (_.tp.get)
    implicit val args = arguments map (lambdaBindArgs(_)(arguments))
    val argNum = OMI(args.length)
    val ret = retO map(List(_)) getOrElse Nil
    val caseNum = OMI(defn.caseNum)
    val cases = Sequence(defn.cases map (lambdaBindArgs(_, true)))
    val caseRes = Sequence(defn.caseRes map (lambdaBindArgs(_, true)))
    val defRes = defn.defResult map(tm => List(lambdaBindArgs(tm))) getOrElse Nil
    val claim = ApplyGeneral(correctnessCondClaim(cor_cond, defn.prefixes+pattern), argNum::Sequence(args)::ret:::caseNum::cases::caseRes::defRes)
    just map(translate_Justification(_, claim)) getOrElse uses(claim, Nil)
  }
  /**
   * translate the correctness condition of a registration
   * @param just the justification for the correctness condition
   * @param kind the kind of the registration (the name of the corresponding pattern)
   * @param m
   * @param tO
   * @param tmO
   * @param aO
   * @param bO
   * @param IdentifyPairs (optional) the pairs of terms (dependent of the arguments) to identify in an identify or reduction registration
   * @param defContext (implicit) the definition conbtext of the registartion
   * @return
   */
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

/**
 * The definition context of a subitem (empty unless on declaration-level or proof-level)
 * @param args the argument context
 * @param assumptions assumptions from the context
 * @param usedFacts proven facts from the context
 * @param corr_conds correctness condition of the subitem
 * @param props properties of the subitem in case of definitions
 * @param topLevel whether the subitem is on top-level
 */
case class DefinitionContext(var args: Context = Context.empty, var assumptions: List[Term] = Nil, var usedFacts: List[(Term, Term)] = Nil, corr_conds: List[Correctness_Condition] = Nil, props: List[MizarProperty] = Nil, topLevel: Boolean = true) {
  // nested proof level, 0 if not within a proof
  private var proofLevel = 0
  private var localDefinitions: List[List[(LocalName, Term)]] = List(Nil)
  // binding variables (of new binders) within the current proof (and its parents)
  private var localBindingVars: List[Context] = List(Context.empty)
  //arguments from the definition block only usable in the non-public part of a definition (i.e. proofs)
  //make into regular arguments when entering a proof
  private var proofArgs: Context = Context.empty
  private def addLocalDefinition(n: LocalName, defn: Term) = {
    //assert (withinProof, "trying to add local definitions outside of a proof. ")
    val toAdd = (n, defn)
    localDefinitions = localDefinitions.head.:+(toAdd) :: localDefinitions.tail
  }
  def addLocalDefinitionInContext(n: LocalName, defn: Term) = addLocalDefinition(n, lambdaBindContext(defn)(this))
  def addLocalDeclaration(c: Declaration with HasDefiniens) = addLocalDefinition(c.name, c.df.get)
  def lookupLocalDefinitionWithinSameProof(n: LocalName): Option[Term] = {
    if (withinProof) localDefinitions.flatMap(_.find(_._1 == n)).headOption.map(_._2) else None
  }
  /**
   * Add a binding variable with scope the current proof
   * @param vd the binding variable
   */
  def addLocalBindingVar(vd: VarDecl): Unit = if (!getLocalBindingVars.contains(vd)) { localBindingVars = localBindingVars.head ++ vd  :: localBindingVars.tail }
  def addLocalBindingVars(ctx: Context): Unit = ctx foreach addLocalBindingVar
  def addProofArg(vd: VarDecl): Unit = if (!proofArgs.contains(vd)) { proofArgs ++=  vd }
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
    if (proofLevel == 1) {
      args ++= proofArgs
      proofArgs = Context.empty
    }
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
  def empty()(implicit toplevel: Boolean = true) = DefinitionContext(topLevel = toplevel)
}

/**
 * A definien using case distinctions in a definition
 */
sealed abstract class CaseByCaseDefinien {
  def cases: List[Term]
  def caseRes: List[Term]
  def caseNum = cases.length
  def defResult: Option[Term]
  /**
   * Used to do inference on, to be removed once inference is no longer nevessary
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
  override def defResult = None
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
case class DirectCompleteCaseByCaseDefinien(cases: List[Term], caseRes: List[Term]) extends CompleteCaseByCaseDefinien with DirectCaseByCaseDefinien
case class IndirectCompleteCaseByCaseDefinien(cases: List[Term], caseRes: List[Term]) extends CompleteCaseByCaseDefinien with IndirectCaseByCaseDefinien

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
  private def parseFormatDesc(formatdes: String): (Int, Int, Int) = {
    val interior = formatdes.substring(formatdes.indexOf('[')+1, formatdes.lastIndexOf(']'))
    val List(pre, int, suf) = interior.split(Array('(', ')')).toList map(_.toInt)
    (pre, int, suf)
  }
  private def PrePostFixMarkers(del: String, infixArgNum: Int, suffixArgNum: Int, rightArgsBracketed: Boolean = false, implArgInds: List[Int] = Nil) = {
    assert (del.nonEmpty, "Encountered empty delimiter in PrePostFix. ")
    PrePostfix(Delim(del), infixArgNum, infixArgNum+suffixArgNum+implArgInds.length, rightArgsBracketed, implArgInds)
  }
  private def CircumfixMarkers(leftDel: String, rightDel: String, circumfixArgNr: Int, implArgInds: List[Int] = Nil) = {
    assert (leftDel.nonEmpty && rightDel.nonEmpty, "Encountered empty delimiter in CircumfixMarkers.\nThe delimiters are \""+leftDel+"\" and \""+rightDel+"\".")
    Circumfix(Delim(leftDel), Delim(rightDel), circumfixArgNr+implArgInds.length, implArgInds)
  }
  private def makeNotCont(fixity: Fixity): NotationContainer = {
    NotationContainer(TextNotation(fixity = fixity, precedence = Precedence.integer(ones = 20), meta = None))
  }
  def makeNotationCont(del: String, infixArgNum: Int, suffixArgNum: Int, rightArgsBracketed: Boolean = false) = {
    makeNotCont(PrePostFixMarkers(del, infixArgNum, suffixArgNum, rightArgsBracketed))
  }
  def globalReference(pat: GloballyReferencingObjAttrs, notMainExtDecl: Boolean = false): GlobalName = {
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
      case c if c == shortKind(FunctorKind()) => gn / longKind(FunctorKind())
      case c if c == shortKind(AttributeKind()) => gn / longKind(AttributeKind())
      case c if c == shortKind(PredicateKind()) => gn / longKind(PredicateKind())
      case c if c == shortKind(ModeKind()) => gn / longKind(ModeKind())
      case _ => gn
    }
  }

  /**
   * Translate the pattern of a new declaration into the corresponding global name and notation
   * @param pattern the pattern to translate
   * @param orgVersion whether we pick the global name of the original definition in case of a pattern of a redefinition
   * @param defContext the definition context
   * @return
   */
  def translate_Pattern(pattern:Patterns, orgVersion: Boolean = false)(implicit defContext: DefinitionContext) : (GlobalName, NotationContainer) = {
    val gn = if (orgVersion) globalReference(pattern, true) else computeGlobalName(pattern)

    val (infixArgNr, circumfixArgNr, suffixArgNr) = parseFormatDesc(pattern.patternAttrs.formatdes)
    val fstDel = pattern.patternAttrs.spelling
    assert (fstDel.nonEmpty, "Encountered empty first delimiter while trying to build notation. ")
    val explArg = pattern._locis .flatMap(translateLocisWithoutSubstitution)
    if (pattern.patKind.isInstanceOf[DeclarationKinds] && pattern.patKind == StructureKind()) if (!(explArg forall (defContext.args.map(_.toTerm).contains(_)))) {
      println ("Not all arguments mentioned in the "+longKind(pattern.patKind)+" are found in the definition context: "+defContext.args+". ")
    }
    val implArgInds = defContext.args.zipWithIndex.filterNot(argInd => explArg.map(_.name).contains(argInd._1.name)).map(_._2)
    val fixity = pattern match {
      case InfixFunctor_Pattern(rightargsbracketedO, _) =>
        val rightArgsBracketed = rightargsbracketedO.getOrElse(false)
        PrePostFixMarkers(fstDel, infixArgNr, suffixArgNr, rightArgsBracketed, implArgInds)
      case CircumfixFunctor_Pattern(orgExtPatAttr, _right_Circumflex_Symbol, _loci, _locis) =>
        val sndDel = _right_Circumflex_Symbol.spelling
        assert (sndDel.nonEmpty, "Encountered empty second delimiter while trying to build notation. ")
        CircumfixMarkers(fstDel, sndDel, circumfixArgNr, implArgInds)
      case strctPat: Strict_Pattern => PrePostFixMarkers(fstDel, 0, 1, false, implArgInds)
      case pat: Patterns =>
        PrePostFixMarkers(fstDel, infixArgNr, suffixArgNr, false, implArgInds)
    }
    (gn, makeNotCont(fixity))
  }
  /**
   * Get the name, argument types, reference to the declaration, notation and parameters
   * of the declaration referenced by the pattern
   * @param pse the pattern shaped expression to dereference
   * @return
   */
  def translate_Referencing_Pattern(pse: Pattern_Shaped_Expression)(implicit defContext: DefinitionContext): (Term, NotationContainer, List[Term]) = {
    val pat: Patterns = pse._pat
    val params = pat._locis.flatMap(_._loci map translate_Locus)
    val (gn, notC) = translate_Pattern(pat, true)
    (OMS(gn), notC, params)
  }
}

object propertyTranslator {
  def translate_JustifiedProperty(justProp: MizarProperty, definitionRef: Option[GlobalName], defRefNot: Option[String] = None)(implicit definitionContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = (justProp, definitionRef) match {
    case (Sethood(pos, _just, Some(_tp)), None) =>
      val tp = translate_Type(_tp)
      val claim = constant("sethood")(tp)
      val just = _just map (translate_Justification(_, claim))
      val backsubstitutedType = tp ^ definitionContext.args.zipWithIndex.map({case (vd, ind) => OMV(LocalName(argsVarName) / ind.toString) / vd.toTerm})
      val name = Utils.makeGlobalName(currentAid, "sethood_of_", TranslationController.presenter.asString(backsubstitutedType)).name
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
  def notToplevel(subitemKind: Option[String] = None) = ImplementationError("This kind of declaration "+subitemKind.getOrElse("")+"should not occur on top-level. ")
  def translate_Reservation(reservation: Reservation) = { Nil }
  def translate_Definition_Item(definition_Item: => Definition_Item): Unit = {
    definition_Item.blockKind() match {
      case "Definitional-Block" => translate_Definitional_Block(definition_Item._block)
      case "Registration-Block" => translate_Registration_Block(definition_Item._block)
      case "Notation-Block" => translate_Notation_Block(definition_Item._block)
    }
  }

  /**
   * Translate the beginning of a new subsection, currently not translated
   * @param pragma
   * @return
   */
  def translate_Section_Pragma(section_Pragma: Section_Pragma) = { Nil }
  /**
   * Translate the beginning of a new section, currently not translated
   * @param pragma
   * @return
   */
  def translate_Pragma(pragma: Pragma) = { Nil }
  def translate_Identify(identify: => Identify)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = { clusterTranslator.translate_Identify(identify) }

  /**
   * translate the scheme contained in a scheme block item into the declaration corresponding to the translated scheme
   * if the scheme is named additionally produce a declaration with the translated name referencing the first declaration (the translated scheme)
   * @param scheme_Block_Item
   * @param defCtx
   * @return
   */
  def translate_Scheme_Block_Item(scheme_Block_Item: => Scheme_Block_Item)(implicit defCtx: => DefinitionContext = DefinitionContext.empty()) = scheme_Block_Item match {
    case sbi @ Scheme_Block_Item(pos, _, _block) =>
      val gn = sbi.globalName
      val provenSentence = sbi.provenSentence
      val Scheme_Head(_sch, _vars, _form, _provForm) = sbi.scheme_head
      _vars._segms foreach (segm => translateBindingVariables(segm)(defCtx))
      val ass : List[Term] = _provForm.map(_._props map(translate_Claim(_)(defCtx))) getOrElse Nil
      implicit val kind = "scheme"
      implicit val notC = makeNotationCont(_sch.name, 0, defCtx.args.length, true)
      val (p, prf) = translate_Proved_Claim(provenSentence)(defCtx)
      articleData.articleStatistics.incrementNonDefinitionStatisticsCounter
      val schemeDef = SchemeDefinitionInstance(gn.name, defCtx.args map (_.tp.get), ass, p, prf)
      val localRef = sbi.notationBasedReference.map(refGn => List(makeReferencingConstant(refGn.name, gn))) getOrElse Nil
      schemeDef::localRef
  }
}

object nymTranslator {
  def translate_Nym(nym:Nyms)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = {
    val oldPatRef = nym._patRefOld
    val newPat: Patterns = nym._patNew
    val (newName, notC) = translate_Pattern(newPat)
    val (mainDecl, _, _) = translate_Referencing_Pattern(oldPatRef)
    val allArgs = defContext.args.map(_.tp.get)
    articleData.articleStatistics.incrementNonDefinitionStatisticsCounter("nym")
    SynonymicNotation(newName.name, allArgs.length, allArgs, mainDecl)(notC, nym.nymKind)
  }
}

object statementTranslator {
  /**
   * translates the statement to usually a single declaration
   *
   * only in case of a choice statement on toplevel we first generate the declaration declaring the new chosen objects,
   * then we append the translated statement applied to those declarations (with the corresponding variable substituted by the references)
   *
   * in either case, the last declaration, is the translated statement
   * @param st the statement to translate
   * @param defContext the definition context of the statement
   * @return
   */
  def translate_Statement(st:Statement with TopLevel)(implicit defContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = st match {
    case choice_Statement: Choice_Statement => translate_Choice_Statement(choice_Statement)._3
    case type_Changing_Statement: Type_Changing_Statement => List(translate_Type_Changing_Statement(type_Changing_Statement))
    case theorem_Item: Theorem_Item => List(translate_Theorem_Item(theorem_Item))
    case regular_Statement: Regular_Statement => List(translate_Regular_Statement(regular_Statement))
  }
  def translate_Conclusion(conclusion: Conclusion) = { Nil }
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement)(implicit defContext: => DefinitionContext) : Declaration with HasType with HasDefiniens with HasNotation = {
    val gn = makeNewGlobalName("Type_Changing_Statement", articleData.incrementAndGetAnonymousTheoremCount().toString)
    val (claim, proof) = translate_Proved_Claim(type_Changing_Statement.prfClaim)(defContext)
    makeConstantInContext(gn.name, Some(claim), Some(proof))(defContext = defContext)
  }
  def translate_Theorem_Item(theorem_Item: Theorem_Item)(implicit defContext: DefinitionContext) = {
    implicit val gn = theorem_Item.referenceableLabel
    val (claim, proof) = translate_Proved_Claim(theorem_Item.prfClaim)
    articleData.articleStatistics.incrementNonDefinitionStatisticsCounter("thm")
    makeConstantInContext(gn.name, Some(claim), Some(proof))
  }

  /**
   * translate the choice statement to 1) an anonymous statement declaration
   * also (if it occurs on top-level) produce 2) declaration declaring the variables whoose existence satisfying certain conditions
   * is shown in the statement
   * in that case the statement references those declarations, otherwise the variables will be added to the definition context
   * of subsequent declarations in the block containing this, in the translation function for said block, the corresponding variables
   * are therefore returned here so they can be added there
   * @param choice_Statement
   * @param defContext
   * @return
   */
  private[translator] def translate_Choice_Statement(choice_Statement: Choice_Statement)(implicit defContext: DefinitionContext): (Context, (Term, Term), List[Constant]) = {
    val vars: Context = choice_Statement._qual._children.flatMap(translate_Context(_))
    defContext.addLocalBindingVars(vars)
    val (claimFreeVars, proofFreeVars) = translate_Proved_Claim(choice_Statement.prfClaim)

    val chosenObjectDecls = vars.map {(vd: VarDecl) =>
      val name = makeNewSimpleGlobalName(vd.name.toString).name
      implicit val notC = makeNotationCont(vd.name.head.toString, 0, 0)
      makeConstantInContext(name, vd.tp, None)
    }

    val gn = makeNewGlobalName("Choice_Statement", articleData.incrementAndGetAnonymousTheoremCount().toString)
    val sub = if (defContext.topLevel) vars zip chosenObjectDecls map {
      case (vd, df) => vd.toTerm / df.toTerm
    } else Nil
    val List(claim, proof) = List(claimFreeVars, proofFreeVars) map (_ ^ sub)
    val theoremDecl = makeConstantInContext(gn.name, Some(claim), Some(proof))
    (vars, (claim, proof), chosenObjectDecls:+theoremDecl)
  }
  def translate_Regular_Statement(regular_Statement: Regular_Statement)(implicit defContext: DefinitionContext) = {
    val gn = makeNewGlobalName("Regular-Statement", articleData.incrementAndGetAnonymousTheoremCount().toString)
    val (claim, proof) = translate_Proved_Claim(regular_Statement.prfClaim)
    val theoremDecl = makeConstantInContext(gn.name, Some(claim), Some(proof))
    theoremDecl
  }
}

object definitionTranslator {
  def translate_Definition(defn: => (Definition, Option[SourceRegion]))(implicit defContext: => DefinitionContext): Unit = {
    val translatedDecls: List[Declaration with HasType with HasDefiniens with HasNotation] = defn._1 match {
      case d: Structure_Definition => translate_Structure_Definition(d)(defContext)
      case rld: RedefinableLabeledDefinition => translate_Redefinable_Labelled_Definition(rld)(defContext)
      case md: Mode_Definition => List(translate_Mode_Definition(md)(defContext))
      case pd: PrivateDefinition => translate_Private_Definition(pd)(defContext)
    }
    val declRef = Some(translatedDecls.head.path)
    lazy val justProps = defContext.props.map(translate_JustifiedProperty(_, declRef, defn._1.pat map (_.patternAttrs.spelling))(defContext))
    if (defContext.withinProof) justProps map (d => defContext.addUsedFact(d.tp.get, d.df.get))
    (translatedDecls:::(if (!defContext.withinProof) justProps else Nil)) foreach (itemTranslator.add(_, defn._2)(defContext))
  }
  def translate_Private_Definition(pd: => PrivateDefinition)(implicit defContext: => DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = pd match {
    case cd: Constant_Definition => translate_Constant_Definition(cd)(defContext)
    case d: Private_Functor_Definition => List(translate_Private_Functor_Definition(d)(defContext))
    case d: Private_Predicate_Definition => List(translate_Private_Predicate_Definition(d)(defContext))
  }
  private def getCorrCond(cc: CorrectnessConditions)(implicit defContext: DefinitionContext, kind: DeclarationKinds, defn: CaseByCaseDefinien) = {
    val corrConds = defContext.corr_conds.map(jcc => (jcc._cond, translate_def_correctness_condition(jcc._cond, jcc._just, defn, kind)))
    corrConds.find(_._1 == cc).map(_._2) getOrElse translate_def_correctness_condition(cc, None, defn, kind, None)
  }
  private def translate_Redefinable_Labelled_Definition(redefinableLabeledDefinition: => RedefinableLabeledDefinition)(implicit defContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    redefinableLabeledDefinition.check
    val (pat, _defn, label) = (redefinableLabeledDefinition._pat, redefinableLabeledDefinition._def, redefinableLabeledDefinition.mmlIdO)
    val (gn, notC) = translate_Pattern(pat)
    val name = gn.name
    implicit val notCon = notC
    val defnO = _defn.map(translate_Definiens(_))
    lazy val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
    implicit val kind = redefinableLabeledDefinition._pat.patKind
    implicit lazy val ret: Option[Term] = redefinableLabeledDefinition match {
      case Functor_Definition(_, _, _, tpSpec, _) =>
        (tpSpec.map(tpSpec => translate_Type(tpSpec._types)) orElse defnO.map(d => inferType(d.someCase)))
        .map (lambdaBindContext(_))
      case _ => None
    }
    val firstRes = if (redefinableLabeledDefinition.redefinition && defnO.isEmpty) {
        //In this case we have a type redefinition
        translate_Redefine(pat, name, ret, None, argNum, argTps)
    } else {
      //We either don't have a redefinition
      //or we define both a new type and definien, i.e. we give a completely new definition
      implicit val defn = defnO.get
      lazy val consistencyProof = getCorrCond (consistency())
      lazy val coherenceProof = getCorrCond (coherence())
      lazy val existenceProof = getCorrCond (existence())
      lazy val uniquenessProof = getCorrCond (uniqueness())
      articleData.articleStatistics.incrementDefinitionStatisticsCounter
      kind match {
        case AttributeKind() =>
          val motherTp = argTps.last
          defn match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.caseNum, cases, caseRes, defRes, consistencyProof)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              IndirectPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.caseNum, cases, caseRes, defRes, consistencyProof)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes) =>
              DirectCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.caseNum, cases, caseRes, consistencyProof)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes) =>
              IndirectCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.caseNum, cases, caseRes, consistencyProof)
          }
        case FunctorKind() =>
          defn match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectPartialFunctorDefinition(name, argNum, argTps, ret.get, defn.caseNum, cases, caseRes, defRes, consistencyProof, coherenceProof)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              IndirectPartialFunctorDefinition(name, argNum, argTps, ret.get, defn.caseNum, cases, caseRes, defRes, consistencyProof, existenceProof, uniquenessProof)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes) =>
              DirectCompleteFunctorDefinition(name, argNum, argTps, ret.get, defn.caseNum, cases, caseRes, consistencyProof, coherenceProof)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes) =>
              IndirectCompleteFunctorDefinition(name, argNum, argTps, ret.get, defn.caseNum, cases, caseRes, existenceProof, consistencyProof, uniquenessProof)
          }
        case PredicateKind() =>
          defn match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectPartialPredicateDef(name, argNum, argTps, defn.caseNum, cases, caseRes, defRes, consistencyProof)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes) =>
              DirectCompletePredicateDef(name, argNum, argTps, defn.caseNum, cases, caseRes, consistencyProof)
            case _ => throw ImplementationError ("Predicate definition can't be indirect. ")
          }
      }
    }
    firstRes :: (label map(_ => makeReferencingConstant(redefinableLabeledDefinition.globalName.name, gn)(longKind(kind), notC)) map(List(_)) getOrElse Nil)
  }
  def translate_Structure_Definition(strDef: =>Structure_Definition)(implicit defContext: DefinitionContext): List[Declaration with HasType with HasDefiniens with HasNotation] = {
    val l = defContext.args.length
    implicit var selectors: List[(Int, VarDecl)] = Nil
    val ancestorTps: List[Term] = strDef._ancestors._structTypes.map(translate_Type)
    val Structure_Patterns_Rendering(_aggrFuncPat, _forgetfulFuncPat, _strFuncPat, Selectors_List(_selectorFuncPat)) = strDef._rendering
    val n = ancestorTps.length
    var substitutions : List[Sub] = Nil
    implicit val declarationPath = strDef._pat.globalPatternName
    val nots@strNot::aggrNot::forgNot::strictNot::selNots = strDef._pat::_aggrFuncPat::_forgetfulFuncPat::_strFuncPat::_selectorFuncPat map(translate_Pattern(_)) map(t=> (t._1.name, t._2))
    def translate_Field_Segments(field_Segments: Field_Segments)(implicit defContext: => DefinitionContext) : List[VarDecl] = field_Segments._fieldSegments flatMap {
      (field_Segment: Field_Segment) =>
      // we pass a definition context with no variables, since we don't want them to be translated to terms of the form x..i, as is required for instances,
      // but instead want them to remain as they are, since the original argument context will be passed on and used instead here
			val tp = translate_Type(field_Segment._tp)(DefinitionContext.empty(), selectors)
      field_Segment._selectors._loci.reverse map { (selector: Selector) =>
        val selName = OMV(selector.spelling)//translate_Locus(selector._loci)
        val sel = (selector.nr, selName % tp)
        selectors ::= sel
        substitutions ::= selName / OMS(structureSelectorPath(selName.name))
        (sel._2 ^ substitutions).copy(not = selNots.find(_._1 == sel._2.name).map(_._2.getAllNotations.head))
      }
    }
    val fieldDecls = translate_Field_Segments(strDef._fieldSegms)
    val m = fieldDecls.length

    articleData.articleStatistics.incrementDefinitionStatisticsCounter(strDef._pat.patKind)
    StructureInstance(l, defContext.args, n, ancestorTps, m, fieldDecls, nots.map(_._2))
  }
  def translate_Mode_Definition(mode_Definition: =>Mode_Definition)(implicit defContext: DefinitionContext) = {
    val (declarationPath, notC) = translate_Pattern(mode_Definition._pat)
    implicit val kind = ModeKind()
    val ln = declarationPath.name
    val name = ln / longKind(kind)
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
            assert(mode_Definition._redef.occurs, "No type specification is given in mode definition, but not a redefinition. ")
            translate_Redefine(mode_Definition._pat, name, None, None, argNum, argTps)
          }
        } else {
          implicit val defn = defnO.get
          articleData.articleStatistics.incrementDefinitionStatisticsCounter
          val (consistencyProof, existenceProof) = (getCorrCond(consistency()), getCorrCond(existence()))
          defn match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              DirectPartialModeDefinition(ln, argNum, argTps, defn.caseNum, cases, caseRes, defRes, consistencyProof, existenceProof)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              IndirectPartialModeDefinition(ln, argNum, argTps, defn.caseNum, cases, caseRes, defRes, consistencyProof, existenceProof)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes) =>
              DirectCompleteModeDefinition(ln, argNum, argTps, defn.caseNum, cases, caseRes, consistencyProof, existenceProof)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes) =>
              IndirectCompleteModeDefinition(ln, argNum, argTps, defn.caseNum, cases, caseRes, consistencyProof, existenceProof)
          }
        }
    }
  }
  private def translate_Redefine(p: Patterns, ln: LocalName, ret: Option[Term], defn: Option[CaseByCaseDefinien], argNum: Int, argTps: List[Term])(implicit kind: DeclarationKinds, notC: NotationContainer) = {
    val origGn = globalReference(p, true)
    val numSuperfluous = p match {
      case pattern: RedefinablePatterns => pattern.globalReDefAttrs.globalOrgAttrs.superfluous getOrElse 0
      case _ => 0
    }
    lazy val tp = kind match {
      case FunctorKind() => ret map (r => Pi(LocalName(argsVarName), nTerms(argNum), r))
      case AttributeKind() | ModeKind() => Some(Pi(LocalName(argsVarName), nTerms(argNum), Arrow(any, prop)))
      case PredicateKind() => Some(Pi(LocalName(argsVarName), nTerms(argNum), prop))
    }
    val origLength = argNum - numSuperfluous
    val addArgsLength = argNum - origLength
    val df = Pi(LocalName(argsVarName), nTerms(argNum), ApplyGeneral(OMS(origGn), (addArgsLength until argNum).toList map (i => Index(OMV(argsVarName),  OMI(i)))))
    makeConstant(ln / LocalName(longKind(kind)), tp, Some(df))(notC)
  }
  private def translate_Constant_Definition(constant_Definition: =>Constant_Definition)(implicit defContext: => DefinitionContext): List[Constant] = {
    constant_Definition._children map { eq =>
      val name = makeNewSimpleGlobalName(eq._var.toIdentifier.toString).name
      val dfU = translate_Term(eq._tm)(defContext)
      val argsContext = defContext.getLocalBindingVars filter (dfU.freeVars contains _.name)
      val df = LambdaOrEmpty(defContext.args++argsContext, dfU ^ implicitNamedDefArgsSubstition()(defContext))
      if (defContext.withinProof) {
        defContext.addLocalDefinitionInContext(name, df)
      }
      val notC = makeNotationCont(eq._var.spelling, 0, 0)
      makeConstantInContext(name, None, Some(df))(notC, defContext)
    }
  }
  /**
   * translates a private functor definition
   * @param private_Functor_Definition the private functor definition to translate
   * @param defContext the definition context
   * @effect if within a proof add the translated functor definition to the lists of local definitions
   * @return the translated functor definition
   */
  def translate_Private_Functor_Definition(private_Functor_Definition: => Private_Functor_Definition)(implicit defContext: => DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = {
    val v = translate_new_Variable(private_Functor_Definition._var)
    val gn = makeNewGlobalName("Private-Functor", private_Functor_Definition._var.serialnr.toString)
    //placeholder terms are numbered starting at 1
    val args: Context = private_Functor_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map {case (tp, i) => OMV("placeholder_"+(i+1)) % tp}
    val tp = PiOrEmpty(args, any)
    val dfBody = translate_Term(private_Functor_Definition._tm)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    if (defContext.withinProof) defContext.addLocalDefinitionInContext(gn.name, df)
    val name = makeSimpleGlobalName(currentAid, v.name.toString).name
    makeConstantInContext(name, Some(tp), Some(df))(defContext = defContext)
  }
  /**
   * translates a private predicate definition
   * @param private_Predicate_Definition the private predicate definition to translate
   * @param defContext the definition context
   * @effect if within a proof add the translated predicate definition to the lists of local definitions
   * @return the translated predicate definition
   */
  def translate_Private_Predicate_Definition(private_Predicate_Definition: => Private_Predicate_Definition)(implicit defContext: => DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = {
    val v = translate_new_Variable(private_Predicate_Definition._var)
    val gn = makeNewGlobalName("Private-Predicate", private_Predicate_Definition._var.serialnr.toString)
    //placeholder terms are numbered starting at 1
    val args: Context = private_Predicate_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map {case (tp, i) => OMV("placeholder_"+(i+1)) % tp}
    val tp = PiOrEmpty(args, prop)
    val dfBody = translate_Formula(private_Predicate_Definition._form)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    if (defContext.withinProof) defContext.addLocalDefinitionInContext(gn.name, df)
    val name = makeSimpleGlobalName(currentAid, v.name.toString).name
    makeConstantInContext(name, Some(tp), Some(df))(defContext = defContext)
  }
}

object clusterTranslator {
  /**
   * translate a registration subitem and add it to the current theory
   * @param reg
   * @param definitionContext
   */
  def translate_Registration_Subitem(reg: RegistrationSubitems, sourceReg: Option[SourceRegion])(implicit definitionContext: DefinitionContext): Unit = reg match {
    case cluster: Cluster => translate_Cluster(cluster)
    case rg: Registrations => translate_Registration(rg, sourceReg)
  }
  def translate_Registration(reg: Registrations, sourceReg: Option[SourceRegion])(implicit definitionContext: DefinitionContext): Unit = {
    articleData.articleStatistics.incrementNonDefinitionStatisticsCounter("registr")
    reg match {
      case Conditional_Registration(pos, _attrs, _at, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_attrs)
        val ats = attributeTranslator.translateAttributes(_at)
        val name = makeNewGlobalName("condReg", articleData.articleStatistics.numRegistrs.toString).name
        val coherenceCond = definitionContext.corr_conds.find(_._cond == syntax.coherence()) getOrElse Correctness_Condition(coherence(), None)
        val coherenceProof = translate_reg_correctness_condition(coherenceCond._just, "condRegistration", adjs.length, Some(tp), None, Some(adjs), Some(ats), None)
        add (ConditionalRegistration(name, definitionContext.args map(_.tp.get), tp, adjs, ats, coherenceProof), Some(pos.parsePosition().toRegion))
      case Existential_Registration(pos, _adjClust, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_adjClust)
        //TODO:
        val name = makeNewGlobalName("existReg", articleData.articleStatistics.numRegistrs.toString).name
        val existenceCond = definitionContext.corr_conds.find(_._cond == syntax.existence()) getOrElse Correctness_Condition(existence(), None)
        val existenceProof = translate_reg_correctness_condition(existenceCond._just, "existRegistration", adjs.length, Some(tp), None, Some(adjs), None, None)
        add (ExistentialRegistration(name, definitionContext.args map(_.tp.get), tp, adjs, existenceProof), Some(pos.parsePosition().toRegion))
      case Functorial_Registration(pos, _aggrTerm, _adjCl, _tp) =>
        val tm = translate_Term(_aggrTerm)
        val adjs = attributeTranslator.translateAttributes(_adjCl)
        val isQualified = _tp.isDefined
        val tp = _tp map translate_Type getOrElse inferType(tm)
        val name = makeNewGlobalName("funcReg", articleData.articleStatistics.numRegistrs.toString).name
        val coherenceCond = definitionContext.corr_conds.find(_._cond == syntax.coherence()) getOrElse Correctness_Condition(coherence(), None)
        def coherenceProof(kind: String) = translate_reg_correctness_condition(coherenceCond._just, kind+"FuncRegistration", adjs.length, Some(tp), Some(tm), Some(adjs), None, None)
        if (isQualified) {
          add (QualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs, coherenceProof("qual")), Some(pos.parsePosition().toRegion))
        } else {
          add (UnqualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs, coherenceProof("unqual")), Some(pos.parsePosition().toRegion))
        }
      case Property_Registration(_props, _just) => add (translate_JustifiedProperty(_props.matchProperty(sourceReg.get.start, Some(_just)), None)(definitionContext), Some(sourceReg.get.start.toRegion))
      case id: Identify => translate_Identify(id)
      case red: Reduction => translate_Reduction(red)
    }
  }
  def translate_Identify(identify: syntax.Identify)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = identify match {
    case syntax.Identify(_firstPat, _sndPat, _lociEqns) =>
      val translatedLociEqns = _lociEqns._lociEqns map {
        case Loci_Equality(_fstLoc, _sndLoc) => (translate_Locus(_fstLoc)(defContext), translate_Locus(_sndLoc)(defContext))
      }
      val num = articleData.incrementAndGetIdentifyCount()
      val name = makeNewGlobalName("identify", num.toString).name
      val (f, _, fparams) = translate_Referencing_Pattern(_firstPat)
      val (g, _, gparams) = translate_Referencing_Pattern(_sndPat)
      val (c, d) = (ApplyGeneral(f, fparams), ApplyGeneral(g, gparams))
      val compatibility: Term = defContext.corr_conds.find(_._cond.kind == syntax.compatibility().kind).map({(comp: Correctness_Condition) =>
        val (aL, bL) = translatedLociEqns.unzip
        val List(a, b) = List(aL, bL) map(Sequence(_))
        translate_reg_correctness_condition(comp._just, "identify", translatedLociEqns.length, None, None, None, None, Some(List(a, b, c, d)))
      }).get
      mmtwrapper.Identify(name, defContext.args map(_.tp.get), translatedLociEqns, c, d, compatibility)
  }
  def translate_Reduction(reduction: syntax.Reduction)(implicit defContext: DefinitionContext): Declaration with HasType with HasDefiniens with HasNotation = reduction match {
    case syntax.Reduction(_predecessor, _successor) =>
      val num = articleData.incrementAndGetReduceCount()
      val name = makeNewGlobalName("reduce", num.toString).name
      val predecessor = translate_Term(_predecessor)
      val successor = translate_Term(_successor)
      val reducibility: Term = defContext.corr_conds.find(_._cond.kind == syntax.reducibility().kind).map({(comp: Correctness_Condition) =>
        translate_reg_correctness_condition(comp._just, "reduce", 0, None, None, None, None, Some(List(predecessor, successor)))
      }).get
      mmtwrapper.Reduction(name, defContext.args map(_.tp.get), predecessor, successor, reducibility)
  }
  def translate_Cluster(cl:Cluster)(implicit definitionContext: DefinitionContext): Unit = {
    cl._registrs foreach (translate_Registration(_, None))
  }
}
object blockTranslator {
  def collectSubitems[mainSort <: BlockSubitem](cls: Class[mainSort], block: => Block) : List[(mainSort, SourceRegion, DefinitionContext)] = {
    val items = block._items
    implicit var defContext = DefinitionContext.empty()(false)

    def recurse(remainingItems: => List[(Subitem, Positions)]):List[(mainSort, SourceRegion, DefinitionContext)] = remainingItems match {
      case Nil => Nil
      case (declIt: DeclarationLevel, pos) :: tl => declIt match {
        case Loci_Declaration(_qualSegms, _conds) =>
          _qualSegms._children foreach { segm =>
            defContext.addArguments(translate_Context(segm)(defContext))
            _conds map (translate_Claim(_)(defContext)) map defContext.addAssumption
          }
          recurse(tl)
        case ass: Assumptions => defContext.addAssumption(translate_Assumption_Claim(ass))
          recurse(tl)
        case Assumption(ass) => defContext.addAssumption(translate_Assumption_Claim(ass))
          recurse(tl)
        case choice_Statement: Choice_Statement =>
          val (addArgs, addFacts, _) = translate_Choice_Statement(choice_Statement)
          addArgs foreach defContext.addProofArg
          defContext.addUsedFact(addFacts)
          recurse(tl)
        case tcs: Type_Changing_Statement =>
          //add the new variables or change the variables with changed type
          translate_Type_Changing_Statement(tcs)(defContext)
          recurse(tl)
        case statement: Regular_Statement =>
          defContext.addUsedFact(translate_Proved_Claim(statement.prfClaim))
          recurse(tl)
        case Reservation(_segm) => recurse(tl)
        case defin if cls.isInstance(defin) =>
          val defn = defin.asInstanceOf[mainSort]
          implicit var corr_conds: List[Correctness_Condition] = Nil
          implicit var props: List[MizarProperty] = Nil
          val (corProps, remaining) = tl.span { case (cc: Property_or_Correctness_Condition, pos) => true case _ => false }
          corProps.map(_._1.asInstanceOf[Property_or_Correctness_Condition]) foreach {
            case cc: Correctness_Condition => corr_conds :+= cc
            case Correctness(_cor, _just) => corr_conds :::= _cor._cond map (Correctness_Condition(_, Some(_just)))
            case Correctness_Conditions(_cond) => //since there is no proof there is also nothing useful for us in here
            case prop: Property => props :+= prop.matchProperty(pos.position)
          }
          val correspondingDefContext = defContext.copy(corr_conds = corr_conds, props = props)
          (defn, pos.sourceRegion(), correspondingDefContext) :: recurse(remaining)
        case pd: PrivateDefinition =>
          translate_Private_Definition(pd)(defContext) foreach (defContext.addLocalDeclaration)
          recurse (tl)
        case prag: Pragma => recurse(tl)
        case defIt =>
          throw DeclarationTranslationError("Unexpected item of type " + defIt.shortKind+" found, in "+block.kind+" in file "+currentAid+".miz", defIt)
          recurse(tl)
      }
      case it::tl =>
        val mes = "Unexpected item of type " + it._1.shortKind+" found, in "+block.kind+" in article "+currentAid+"\nInstead expected a declaration item. "
        throw ImplementationError (mes)
    }
    recurse (items map (it => (it._subitem, it.pos)))
  }
  def translate_Definitional_Block(block: => Block): Unit = {
    val definitionItems = collectSubitems[Definition](classOf[Definition], block)
    definitionItems foreach  (dd => (translate_Definition(dd._1, Some(dd._2))(dd._3), dd._1))
  }
  def translate_Registration_Block(block: => Block): Unit = {
    val clusterItems = collectSubitems[RegistrationSubitems](classOf[RegistrationSubitems], block)
    clusterItems foreach (rd => translate_Registration_Subitem(rd._1, Some(rd._2))(rd._3))
  }
  def translate_Notation_Block(block: => Block): Unit = {
    val notationItems = collectSubitems[Nyms](classOf[Nyms], block)
    notationItems foreach (nd => add (translate_Nym(nd._1)(nd._3), Some(nd._2))(nd._3))
  }
}