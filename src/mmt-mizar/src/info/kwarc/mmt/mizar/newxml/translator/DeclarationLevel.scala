package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api._
import notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mizar.newxml._
import mmtwrapper._
import MMTUtils._
import Mizar._
import syntax._
import expressionTranslator._
import termTranslator._
import typeTranslator._
import contextTranslator._
import formulaTranslator._
import TranslatorUtils._
import claimTranslator._
import clusterTranslator._
import definitionTranslator._
import blockTranslator._
import info.kwarc.mmt.api.symbols.{Constant, Declaration}
import info.kwarc.mmt.mizar.newxml.mmtwrapper.PatternUtils.{LambdaOrEmpty, PiOrEmpty}
import info.kwarc.mmt.mizar.newxml.syntax.Utils._
import clusterTranslator._
import definiensTranslator._
import info.kwarc.mmt.mizar.newxml.mmtwrapper.MizSeq.OMI
import info.kwarc.mmt.mizar.newxml.translator.JustifiedCorrectnessConditions.translate_consistency
import info.kwarc.mmt.mizar.newxml.translator.TranslationController.{addUnresolvedDependency, controller, currentAid, getAnonymousTheoremCount, getIdentifyCount, getUnresolvedDependencies, incrementAnonymousTheoremCount, incrementIdentifyCount, inferType, localPath, makeConstant}
import info.kwarc.mmt.mizar.newxml.translator.statementTranslator.translate_Choice_Statement
import justificationTranslator._
import propertyTranslator._
import nymTranslator._
import patternTranslator._

case class JustifiedCorrectnessConditions(correctness_Condition: CorrectnessConditions, just: Option[Justification])
object JustifiedCorrectnessConditions {
  def translate(corConds: List[CorrectnessConditions], just: Option[Justification])(implicit defContext: DefinitionContext = DefinitionContext.empty()): List[JustifiedCorrectnessConditions] = {
    corConds map (JustifiedCorrectnessConditions(_, just))
  }
  def translate_consistency(just: Option[Justification], argTps: List[Term], cases: List[Term], caseRes: List[Term], dir: Boolean, kind : String)(implicit defContext: DefinitionContext = DefinitionContext.empty()) = {
    //TODO: Fix this up: Right now we supply type as the claim passed to the justification translator,
    //which obviously makes no sense
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

private[translator] class ThesisTerm(tm: Term) {
  private def matchTerm(tm: Term): (List[VarDecl], List[Term], List[VarDecl], List[VarDecl], List[Term]) = tm match {
    case PiOrEmpty(ctx, tm) =>
      val args = ctx map (vd => vd.copy(tp = vd.tp))
    def matchConds(tm: Term): (List[Term], Term) = tm match {
      case Mizar.implies(cond, rest) =>
        val (conds, body) = matchConds(rest)
        (cond::conds, body)
      case _ => (Nil, tm)
    }
      val (conds, actualClaim) = matchConds(tm)
    def existentialQuantifier(tm: Term): (Context, Term) = tm match {
        case Mizar.exists(vname, vtype, body) =>
          val (ctx, tm) = existentialQuantifier(body)
          (vname % TranslationController.simplifyTerm(vtype) :: ctx, tm)
        case _ => (Context.empty, tm)
      }
      val (existQuants, body) = existentialQuantifier(actualClaim)

      def univQuantifier(tm: Term): (Context, Term) = tm match {
        case Mizar.forall(vname, vtype, body) =>
          val (ctx, tm) = univQuantifier(body)
          (vname % TranslationController.simplifyTerm(vtype) :: ctx, tm)
        case _ => (Context.empty, tm)
      }
      val (univQuants, conjunction) = univQuantifier(body)
      val conjuncts = tm match  {
        case And(conjuncts) => conjuncts// map TranslationController.simplifyTerm
        case tm => List(tm)//TranslationController.simplifyTerm(tm))
      }
      (args, conds, existQuants, univQuants, conjuncts)
    case _ => throw ImplementationError("This can't ever happen.")
  }
  var args: List[VarDecl] = matchTerm(tm)._1
  var conditions: List[Term] = matchTerm(tm)._2
  var existQuants: Context = matchTerm(tm)._3
  var univQuants: Context = matchTerm(tm)._4
  var conjuncts: List[Term] = matchTerm(tm)._5
  private def killOne(cond: VarDecl, ctx: => List[VarDecl]): Unit = cond match {
    case VarDecl(OMV.anonymous, _, tp, _, _) => ctx.filterNot {
      case VarDecl(_, _, vtp, _, _) => vtp.get == TranslationController.simplifyTerm(tp.get)
    }
    case VarDecl(name, _, tp, _, _) => ctx filterNot {
      case VarDecl(vname, _, vtp, _, _) => if (name == vname) true else if (vname != OMV.anonymous) false else vtp.get == TranslationController.simplifyTerm(tp.get)
    }
  }
  def killArg(arg: VarDecl) = killOne(arg, args)
  def killExistVar(v: VarDecl) = killOne(v, existQuants)
  def killUnivVar(v: VarDecl) = killOne(v, univQuants)
  def killConjunct(tm: Term) = {
    assert(tm == conjuncts.head)
    conjuncts = conjuncts.tail
  }
  def killCond(tm: Term) = {
    assert(tm == conditions.head)
    conditions = conditions.tail
  }
  def doneProving(): Boolean = (conditions ++ existQuants ++ univQuants).isEmpty && conjuncts == Nil
  def toTerm(implicit defContext: DefinitionContext): Term = {
    val allConjunctions = Mizar.And(conjuncts)
    val univQuantified = translate_Universal_Quantifier_Formula(univQuants, allConjunctions, None)(defContext.args)
    val existQuantified = translate_Existential_Quantifier_Formula(existQuants, univQuantified, None)(defContext.args)
    val furtherArgs = defContext.args.filter(existQuantified.freeVars contains _)
    PiOrEmpty(furtherArgs++args, conditions.foldRight(existQuantified)(Mizar.implies(_, _)))
  }
  def subobjects: List[(Context, Obj)] = (conditions ++ existQuants ++ univQuants).flatMap(_.subobjects) ++ conjuncts.flatMap(_.subobjects)
}
private[translator] object ThesisTerm {
  def empty() : ThesisTerm = {
    var ths = new ThesisTerm(Mizar.trueCon)
    ths.conjuncts = Nil
    ths
  }
}
case class DefinitionContext(var args: Context = Context.empty, assumptions: List[Term] = Nil, corr_conds: List[JustifiedCorrectnessConditions] = Nil, props: List[Property] = Nil, usedFacts: List[(Term, Option[Term])] = Nil, var thesis: ThesisTerm = ThesisTerm.empty()) {
  def addArguments(arguments: Context) = this.copy(args = this.args ++ arguments)
  def addAssumptions(assumptions: List[Term]) = this.copy(assumptions = this.assumptions ++ assumptions)
  def killArg(arg: VarDecl): Unit = thesis.killArg(arg)
  def killConjunct(conjunct: Term): Unit = thesis.killConjunct(conjunct)
  def killAssumption(assumption: Term): Unit = thesis.killCond(assumption)
  def killExistQuant(existQuant: VarDecl): Unit = thesis.killExistVar(existQuant)
  def killUnivQuant(univQuant: VarDecl): Unit = thesis.killUnivVar(univQuant)
  private[translator] def setThesis(claim: Term): Unit = {
    assert (thesis.doneProving())
    thesis = new ThesisTerm(claim)
  }
  private[translator] def setThesis(claim: ThesisTerm): Unit = {
    thesis = claim
  }
}
object DefinitionContext {
  def empty() = DefinitionContext()
}

case class JustifiedProperty(conds: List[MizarProperty], prop: MizarProperty, tp: Option[Term], decl: Option[Declaration])
object JustifiedProperty {
  def apply(property: Property, decls: List[Declaration])(implicit definitionContext: DefinitionContext) : JustifiedProperty = apply(property, decls.headOption)
  def apply(property: Property, decl: Option[Declaration])(implicit definitionContext: DefinitionContext) : JustifiedProperty = apply(property._props, decl, property._just)
  def apply(properties: Properties, decl: Option[Declaration], justO: Option[Justification] = None)(implicit definitionContext: DefinitionContext = DefinitionContext.empty()) : JustifiedProperty = {
    val prop = properties.matchProperty(justO)
    val conds = properties._cond.map(_.matchProperty())
    val tp = properties._tp map translate_Type
    JustifiedProperty(conds, prop, tp, decl)
  }
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
  def translate_Definiens(defs:Definiens, just: Option[Justification] = None)(implicit defContext: DefinitionContext = DefinitionContext.empty()): CaseByCaseDefinien = {
    translate_CaseBasedExpr(defs._expr)
  }
  def translate_CaseBasedExpr(defn:CaseBasedExpr)(implicit defContext: DefinitionContext = DefinitionContext.empty()): CaseByCaseDefinien = {
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
  def translate_Cased_Expression(partDef:PartialDef)(implicit defContext: DefinitionContext = DefinitionContext.empty()): CaseByCaseDefinien = {
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
    PrePostfix(Delim(del), infixArgNum, infixArgNum+suffixArgNum)
  }
  private def CircumfixMarkers(leftDel: String, rightDel: String, circumfixArgNr: Int) = {
    Circumfix(Delim(leftDel), Delim(rightDel), circumfixArgNr)
  }
  private def makeNotCont(fixity: Fixity): NotationContainer = {
    def reasonableChar: Char => Boolean = {c => !("#" contains c)}//c.isLetterOrDigit || c.isWhitespace || "|.<>=+-*/()&^%$@!~,".contains(c)}
    val reasonable = fixity match {
      case Circumfix(lDelim, rDelim, num) => (lDelim++rDelim).forall(reasonableChar)
      case fixity: SimpleFixity => fixity.delim.text.forall(reasonableChar)
      case _ => false
    }
    if (reasonable) {
      NotationContainer(TextNotation(fixity = fixity, precedence = Precedence.integer(ones = 20), meta = None))
    } else {
      NotationContainer.empty()
    }
  }

  def makeNotationCont(del: String, infixArgNum: Int, suffixArgNum: Int, rightArgsBracketed: Boolean = false) = {
    makeNotCont(PrePostFixMarkers(del, infixArgNum, suffixArgNum, rightArgsBracketed))
  }
  def translate_Pattern(pattern:Patterns, orgVersion: Boolean = false) : (LocalName, GlobalName, NotationContainer) = {
    val referencedGn = pattern match {
      case pat: RedefinablePatterns =>
        computeGlobalName(pat, orgVersion)//computeGlobalOrgPatConstrName(pat)
      case pat: ConstrPattern => computeGlobalName(pat)//computeGlobalPatConstrName(pat)
      case pat => computeGlobalName(pat)
    }
    val gn = if (referencedGn.toString contains "hidden") {
      resolveHiddenReferences(referencedGn) match {
        case Some(OMS(p)) => p
        case _ => referencedGn
      }
    } else referencedGn
    val name = gn.name

    val (infixArgNr, circumfixArgNr, suffixArgNr) = parseFormatDesc(pattern.patternAttrs.formatdes)
    val notC = pattern match {
      case InfixFunctor_Pattern(rightargsbracketedO, orgExtPatAttr, _loci, _locis) =>
        val rightArgsBracketed = rightargsbracketedO.getOrElse(false)
        makeNotationCont(orgExtPatAttr.extPatAttr.patAttr.spelling, infixArgNr, suffixArgNr, rightArgsBracketed)
      case CircumfixFunctor_Pattern(orgExtPatAttr, _right_Circumflex_Symbol, _loci, _locis) =>
        val fixity = CircumfixMarkers(orgExtPatAttr.extPatAttr.patAttr.spelling, _right_Circumflex_Symbol.spelling, circumfixArgNr)
        makeNotCont(fixity)
      case pat: Patterns =>
        makeNotationCont(pat.patternAttrs.spelling, infixArgNr, suffixArgNr)
    }
    (name, gn, notC)
  }

  /**
   * Get the name, argument types, reference to the declaration, notation and parameters
   * of the declaration referenced by the pattern
   * @param pat
   * @return
   */
  def translate_Referencing_Pattern(pat: Patterns): (LocalName, List[Term], Term, NotationContainer, List[Term]) = {
    val params = pat._locis.flatMap(_._loci map translate_Locus)
    val (name, gn, notC) = translate_Pattern(pat, true)
    val referencedDeclSE = controller.getO(gn) getOrElse ({
      throw PatternTranslationError("Error looking up the declaration at "+gn.toString+" referenced by pattern: "+pat+", but no such Declaration found. "
        +"Probably this is because we require the dependency theory "+gn.module+" of the article currently being translated "+localPath+" to be already translated: \n"
        +"Please make sure the theory is translated (build with mizarxml-omdoc build target) and try again. ", pat)
      if (! (getUnresolvedDependencies() contains gn.module)) {
        addUnresolvedDependency(gn.module)
      }
      throw new ObjectLevelTranslationError("Trying to lookup declaration (presumably) at "+gn.toString+" referenced by pattern: "+pat+", but no such Declaration found. ", pat)})
    val (mainDecl, addArgsTps): (Term, List[Term]) = referencedDeclSE match {
      case c: Constant =>
        val FunType(addArgsTps, prop) = c.tp.getOrElse(
          throw new ObjectLevelTranslationError("Trying to retrieve type of looked up declaration " + c.name + " referenced by pattern: " + pat + ", but declaration has not type defined. ", pat))
        (c.toTerm, addArgsTps.map(_._2))
      case dd: symbols.DerivedDeclaration =>
        val addArgTps = dd match {
          case directPartialAttributeDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case indirectPartialAttributeDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case directCompleteAttributeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case indirectCompleteAttributeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case directPartialFunctorDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case indirectPartialFunctorDefinition(_, _, addArgTps, _, _, _, _, _, _) => addArgTps
          case directCompleteFunctorDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case indirectCompleteFunctorDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case directPartialModeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case indirectPartialModeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case directCompleteModeDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
          case indirectCompleteModeDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
          case directPartialPredicateDef(_, _, addArgTps, _, _, _, _, _) => addArgTps
          case directCompletePredicateDef(_, _, addArgTps, _, _, _, _) => addArgTps
          case other => throw PatternTranslationError("Expected reference to original declaration of same kind in redefinition, but instead found "+other.feature+" at referenced location "+other.path+"\nReferenced by the pattern: "+pat+". ", pat)
        }
        (dd.toTerm, addArgTps)
    }
    (name, addArgsTps, mainDecl, notC, params)
  }
}

object propertyTranslator {
  def translate_JustifiedProperty(justProp: JustifiedProperty)(implicit definitionContext: DefinitionContext = DefinitionContext.empty()): Declaration = justProp match {
    case JustifiedProperty(conds, Sethood(_just), Some(tp), _) =>
      val claim = Apply(Mizar.constant("sethood"), tp)
      val just = _just map (translate_Justification(_, claim)) getOrElse None
      val name = LocalName("sethood_of_"+tp.toStr(true))
      makeConstant(name, Some(Univ(1)), just)
    case _ => ???
  }
}

object subitemTranslator {
  def translate_Reservation(reservation: Reservation) = { Nil }
  def translate_Definition_Item(definition_Item: Definition_Item) = {
    definition_Item.check() match {
      case "Definitional-Block" => translate_Definitional_Block(definition_Item._block)
      case "Registration-Block" => translate_Registration_Block(definition_Item._block)
      case "Notation-Block" => translate_Notation_Block(definition_Item._block)
    }
  }
  def translate_Section_Pragma(section_Pragma: Section_Pragma) = { Nil }
  def translate_Pragma(pragma: Pragma) = { Nil }
  def translate_Loci_Declaration(loci_Declaration: Loci_Declaration): Context = {
    loci_Declaration._qualSegms._children flatMap(translate_Context(_))
  }
  def translate_Correctness(correctness: Correctness) = { ??? }
  def translate_Correctness_Condition(correctness_Condition: Correctness_Condition) = { ??? }
  def translate_Exemplification(exemplification: Exemplification) = { ??? }
  def translate_Assumption(assumption: Assumption) = { ??? }
  def translate_Identify(identify: Identify) = { clusterTranslator.translate_Identify(identify._firstPat, identify._sndPat) }
  def translate_Generalization(generalization: Generalization) = { ??? }
  def translate_Reduction(reduction: Reduction) = { ??? }
  def translate_Scheme_Block_Item(scheme_Block_Item: Scheme_Block_Item) = scheme_Block_Item match {
    case sbi @ Scheme_Block_Item(_, _block) =>
      val gn = mMLIdtoGlobalName(sbi.mizarGlobalName())
      val provenSentence = sbi.provenSentence()
      val Scheme_Head(_sch, _vars, _form, _provForm) = sbi.scheme_head()
      val args : Context = _vars._segms.flatMap(translateVariables(_))
      implicit val defContext = DefinitionContext(args)
      val ass : List[Term] = _provForm.map(_._props map(translate_Claim)) getOrElse Nil
      implicit val notC = makeNotationCont(_sch.spelling, 0, args.length, true)
      val (p, prf) = translate_Proved_Claim(provenSentence)
      val tr = namedDefArgsTranslator()
      List(schemeDefinitionInstance(gn.name, args.map(_.tp.get), ass, p, prf)) map tr
  }
  def translate_Property(property: Property, decl: Option[Definition]) : List[Declaration] = {
    val justProp = JustifiedProperty(property._props, decl.map( d=> translate_Definition(d).head), property._just)
    List(translate_JustifiedProperty(justProp))
  }
}

object headTranslator {
  // this may never be called in practise
  def translate_Head(head:Heads) = { ??? }
  def translate_Scheme_Head(reservation: Scheme_Head) = {}
  def translate_Suppose_Head(reservation: Suppose_Head) = {}
  def translate_Case_Head(reservation: Case_Head) = {}
}

object nymTranslator {
  def translate_Nym(nym:Nyms)(implicit defContext: DefinitionContext = DefinitionContext.empty()): List[Declaration] = {
    val oldPat = nym._patOld
    val newPat: Patterns = nym._patNew
    val (name, newName, notC) = translate_Pattern(newPat)
    try {
      val (_, addArgsTps, mainDecl, _, _) = translate_Referencing_Pattern(oldPat)
      val allArgs = defContext.args.map(_.tp.get) ++ addArgsTps
      val tr = namedDefArgsTranslator()
      List(tr(synonymicNotation(newName.name, allArgs.length, allArgs, mainDecl)))
    } catch {
      case e: ObjectLevelTranslationError => Nil
    }
  }
}

object statementTranslator {
  def translate_Statement(st:Statement) = st match {
    case conclusion: Conclusion => translate_Conclusion(conclusion)
    case type_Changing_Statement: Type_Changing_Statement => translate_Type_Changing_Statement(type_Changing_Statement)
    case theorem_Item: Theorem_Item => translate_Theorem_Item(theorem_Item)
    case choice_Statement: Choice_Statement =>
      val (args, (claim, proof)) = translate_Choice_Statement(choice_Statement)
      incrementAnonymousTheoremCount()
      val gn = makeNewGlobalName("Choice_Statement", getAnonymousTheoremCount())
      implicit val defCtx = DefinitionContext(args, List(claim))
      implicit val notC = NotationContainer.empty()
      val tr = namedDefArgsTranslator()
      val theoremDecl = makeConstant(gn.name, Some(claim), proof)
      List(theoremDecl) map tr
    case regular_Statement: Regular_Statement => translate_Regular_Statement(regular_Statement)
  }
  def translate_Conclusion(conclusion: Conclusion) = { ??? }
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement) = { ??? }
  def translate_Theorem_Item(theorem_Item: Theorem_Item)(implicit defContext: DefinitionContext = DefinitionContext.empty()) = {
    val prfedClaim = theorem_Item.prfClaim
    implicit val gn = theorem_Item.referencedLabel()//mMLIdtoGlobalName(theorem_Item.mizarGlobalName())
    val (claim, proof) = translate_Proved_Claim(theorem_Item.prfClaim)
    val tr = namedDefArgsTranslator()
    val theoremDecl = makeConstant(gn.name, Some(claim), proof)
    List(theoremDecl) map tr
  }
  def translate_Choice_Statement(choice_Statement: Choice_Statement)(implicit defContext: DefinitionContext = DefinitionContext.empty()): (Context, (Term, Option[Term])) = {
    val vars: Context = choice_Statement._qual._children.flatMap(translate_Context(_))
    val facts = translate_Proved_Claim(choice_Statement.prfClaim)
    (vars, facts)
  }
  def translate_Regular_Statement(regular_Statement: Regular_Statement) = {
    incrementAnonymousTheoremCount()
    val gn = makeNewGlobalName("Regular-Statement", getAnonymousTheoremCount())
    implicit val defCtx = DefinitionContext.empty()
    implicit val notC = NotationContainer.empty()
    val (claim, proof) = translate_Proved_Claim(regular_Statement.prfClaim)
    val tr = namedDefArgsTranslator()
    val theoremDecl = makeConstant(gn.name, Some(claim), proof)
    List(theoremDecl) map tr
  }
}

object definitionTranslator {
  def translate_Definition(defn:Definition)(implicit defContext: DefinitionContext = DefinitionContext.empty()) : List[Declaration] = {
    val translatedDecls: List[Declaration] = defn match {
      case at: Attribute_Definition => translate_Attribute_Definition(at)
      case cd: Constant_Definition => translate_Constant_Definition(cd)
      case funcDef: Functor_Definition => translate_Functor_Definition(funcDef)
      case md: Mode_Definition => translate_Mode_Definition(md)
      case pd: Predicate_Definition => translate_Predicate_Definition(pd)
      case d: Private_Functor_Definition  => translate_Private_Functor_Definition(d)
      case d: Private_Predicate_Definition => translate_Private_Predicate_Definition(d)
      case d: Structure_Definition => translate_Structure_Definition(d)
    }
    val tr = namedDefArgsTranslator()
    val preResDecl = translatedDecls.map(tr)
    val justProps = defContext.props.map(p => JustifiedProperty(p, preResDecl))
    preResDecl ++ justProps.map(translate_JustifiedProperty(_))
  }
  def translate_Structure_Definition(strDef: Structure_Definition)(implicit defContext: DefinitionContext = DefinitionContext.empty()): List[Declaration] = {
    val l = defContext.args.length
    implicit var selectors: List[(Int, VarDecl)] = Nil
    val substr: List[Term] = strDef._ancestors._structTypes.map(translate_Type) map {
      case ApplyGeneral(typeDecl, args) => typeDecl
      case _ => throw ImplementationError("This should be impossible.")
    }
    val n = substr.length
    var substitutions : List[Sub] = Nil
    //TODO: this shouldn't be neccesary ideally
    val declarationPath = mMLIdtoGlobalName(strDef._strPat.globalPatternName().copy(kind = "L"))//TranslatorUtils.computeGlobalPatternName(strDef._strPat)
    val Structure_Patterns_Rendering(_aggrFuncPat, _, _strFuncPat, Selectors_List(_selectorFuncPat)) = strDef._rendering
    val aggrNot::strNot::selNots  = _aggrFuncPat::_strFuncPat::_selectorFuncPat map(translate_Pattern(_)) map(t=> (t._1, t._3))

    def translate_Field_Segments(field_Segments: Field_Segments)(implicit defContext: DefinitionContext = DefinitionContext.empty()) : List[VarDecl] = field_Segments._fieldSegments flatMap {
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
    val trS = namedDefArgsSubstition(defContext.args)

    StructureInstance(declarationPath, l, defContext.args, n, substr, m, Context.list2context(fieldDecls) ^ trS)
  }
  def translate_Attribute_Definition(attribute_Definition: Attribute_Definition)(implicit defContext: DefinitionContext = DefinitionContext.empty()) = attribute_Definition match {
    case atd @ Attribute_Definition(a, _redefine, _attrPat, _def) =>
      val (name, gn, notC) = translate_Pattern(_attrPat)
      implicit val notCon = notC
      val defn = _def.map(translate_Definiens(_))
      if (defn.isEmpty) {
        if(_redefine.occurs) {
          List(translate_Redefine(_attrPat))
        } else {
          ???
        }
      } else {
        val motherTp = inferType(defn.get.someCase)
        val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
        val atrDef = defn.get match {
          case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
            val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, true, "attr")
              directPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes, consistency)
          case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
            val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, false, "attr")
            indirectPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes, consistency)
          case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
            val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, true, "attr")
            directCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, consistency)
          case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
            val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, false, "attr")
            indirectCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, consistency)
        }
        List(atrDef)
      }
  }
  def translate_Constant_Definition(constant_Definition: Constant_Definition)(implicit defContext: DefinitionContext = DefinitionContext.empty()): List[Constant] = {
    constant_Definition._children map {eq =>
      val name = LocalName(eq._var.varAttr.copy(kind = "DefConstant").toIdentifier(true))
      val df = translate_Term(eq._tm)
      val notC = makeNotationCont(eq._var.varAttr.spelling, 0, 0)
      makeConstant(name, None, Some(df))(notC)
    }
  }
  def translate_Functor_Definition(functor_Definition: Functor_Definition)(implicit defContext: DefinitionContext = DefinitionContext.empty()) = functor_Definition match {
    case fd @ Functor_Definition(_, _redefine, _pat, _tpSpec, _def) =>
    val (name, gn, notC) = translate_Pattern(_pat)
    implicit val notCon = notC
    val specType = _tpSpec map (tpSpec => translate_Type(tpSpec._types))
    val defn = _def.map(translate_Definiens(_))

    val ret = if (specType.isDefined) {specType} else { defn.map(d => inferType(d.someCase)) }
    if (defn.isEmpty) {
      if(_redefine.occurs) {
        List(translate_Redefine(_pat))
      } else {
        ???
      }
    } else {
      val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
      val funcDef = defn.get match {
        case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
          val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, true, "func")
          directPartialFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, defRes, consistency)
        case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
          val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, false, "func")
          indirectPartialFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, defRes, consistency)
        case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
          val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, true, "func")
          directCompleteFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, consistency)
        case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
          val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, false, "func")
          indirectCompleteFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, consistency)
      }
      List(funcDef)
    }
  }
  def translate_Mode_Definition(mode_Definition: Mode_Definition)(implicit defContext: DefinitionContext = DefinitionContext.empty()) = {
    val (name, declarationPath, notC) = translate_Pattern(mode_Definition._pat)
    implicit val notCon = notC
    mode_Definition._expMode match {
      case Expandable_Mode(_tp) =>
        val tp = translate_Type(_tp)
        List(makeConstant(declarationPath.name, Some(Mizar.tp),Some(tp)))
      case stm @ Standard_Mode(_tpSpec, _def) =>
        val name = declarationPath.name
        val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
        val defnO = _def map(translate_Definiens(_))

        if (defnO.isEmpty) {
          if (_tpSpec.isDefined) {
            List(makeConstant(declarationPath.name, translate_Type(_tpSpec.get._types)))
          } else {
            //Only here we may have content that doesn't typecheck
            assert(hiddenArts contains declarationPath.module)
            Nil
          }
        } else {
          val defn = defnO.get
          val consistency = defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap(_.just)
          val modeDef = defn match {
            case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, true, "mode")
              directPartialModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, defRes, consistency)
            case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
              val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, false, "mode")
              indirectPartialModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, defRes, consistency)
            case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, true, "mode")
              directCompleteModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, consistency)
            case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
              val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, false, "mode")
              indirectCompleteModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, consistency)
          }
          List(modeDef)
        }
    }
  }
  def translate_Private_Functor_Definition(private_Functor_Definition: Private_Functor_Definition)(implicit defContext: DefinitionContext = DefinitionContext.empty()) = {
    val v = translate_Variable(private_Functor_Definition._var, true)
    val gn = makeNewGlobalName("Private-Functor", private_Functor_Definition._var.varAttr.locVarAttr.serialNrIdNr.idnr)
    val args: Context = private_Functor_Definition._tpList._tps.map(translate_Type(_)).zipWithIndex map({case (tp, i) => OMV("placeholder_"+i.toString) % tp})
    val tp = PiOrEmpty(args, any)
    val dfBody = translate_Term(private_Functor_Definition._tm)
    val df = LambdaOrEmpty(args, dfBody)
    val res = makeConstant(gn.name, Some(tp), Some(df))
    val tr = namedDefArgsTranslator()
    List(tr(res))
  }
  def translate_Private_Predicate_Definition(private_Predicate_Definition: Private_Predicate_Definition)(implicit defContext: DefinitionContext = DefinitionContext.empty()) = {
    val v = translate_Variable(private_Predicate_Definition._var, true)
    val gn = makeNewGlobalName("Private-Predicate", private_Predicate_Definition._var.varAttr.locVarAttr.serialNrIdNr.idnr)
    val args: Context = private_Predicate_Definition._tpList._tps.map(translate_Type(_)).zipWithIndex map({case (tp, i) => OMV("placeholder_"+i.toString) % tp})
    val tp = PiOrEmpty(args, prop)
    val dfBody = translate_Formula(private_Predicate_Definition._form)
    val df = LambdaOrEmpty(args, dfBody)
    val res = makeConstant(gn.name, Some(tp), Some(df))
    val tr = namedDefArgsTranslator()
    List(tr(res))
  }
  def translate_Predicate_Definition(predicate_Definition: Predicate_Definition)(implicit defContext: DefinitionContext = DefinitionContext.empty()) = predicate_Definition match {
    case prd@Predicate_Definition(a, _redefine, _predPat, _def) =>
      val (name, gn, notC) = translate_Pattern(_predPat)
      implicit val notCon = notC
      val defn = _def.map(translate_Definiens(_))
      if (defn.isEmpty) {
        if (_redefine.occurs) {
          List(translate_Redefine(_predPat))
        } else {
          ???
        }
      } else {
        val (argNum, argTps) = (defContext.args.length, defContext.args.map(_.tp.get))
        val predDef = defn.get match {
          case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) =>
            val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, true, "pred")
            directPartialPredicateDef(name, argNum, argTps, defn.get.caseNum, cases, caseRes, defRes, consistency)
          case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) =>
            val consistency = translate_consistency(defContext.corr_conds.find(_.correctness_Condition == syntax.consistency()) flatMap (_.just), argTps, cases, caseRes, true, "pred")
            directCompletePredicateDef(name, argNum, argTps, defn.get.caseNum, cases, caseRes, consistency)
          case _ => throw DeclarationTranslationError("Predicate definition can't be indirect. ", prd)
        }
        List(predDef)
      }
  }
  def translate_Redefine(pat: RedefinablePatterns, ret: Option[Term] = None) = {
    val (name, origDecl, notC) = translate_Pattern(pat, true)
    implicit val notCon = notC
    val gn = TranslatorUtils.computeGlobalName(pat)
    val mod = controller.getModule(origDecl.module)
    //try to match both constrnr and patternnr
    mod.getO(origDecl.name) orElse {
      //use only the constrnr to match
      val constrNrPart = pat.globalKind+pat.globalConstrNr
      val constrNumBasedName = mod.domain.filter(_.toString.endsWith(constrNrPart)) match {
        case List(name) => Some(name)
        case _ => None
      }
      constrNumBasedName map mod.get
    } match {
      case Some(c: Constant) => makeConstant(gn.name, Some(ret getOrElse c.tp.get), c.df)
      case Some(MizarPatternInstance(ln, pat, params)) => MizarPatternInstance(gn.name, pat, params)
      case _ => throw PatternTranslationError("Failure to look up the referenced definition to redefine at "+origDecl.module.last+"?"+origDecl.name+"(full path "+origDecl+"). ", pat)
    }
  }
}

object clusterTranslator {
  def translate_Registration(reg: Registrations)(implicit definitionContext: DefinitionContext = DefinitionContext.empty()): List[Declaration] = {
    val tr = namedDefArgsTranslator()
    val resDecl: Option[Declaration] = reg match {
      case Conditional_Registration(pos, _attrs, _at, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_attrs)
        val ats = attributeTranslator.translateAttributes(_at)
        val name = LocalName("condReg:"+pos.position)
        Some(conditionalRegistration(name, definitionContext.args map(_.tp.get), tp, adjs, ats))
      case Existential_Registration(pos, _adjClust, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_adjClust)
        //TODO:
        val name = LocalName("existReg:"+pos.position)
        Some(existentialRegistration(name, definitionContext.args map(_.tp.get), tp, adjs))
      case Functorial_Registration(pos, _aggrTerm, _adjCl, _tp) =>
        val tm = translate_Term(_aggrTerm)
        val adjs = attributeTranslator.translateAttributes(_adjCl)
        val isQualified = _tp.isDefined
        val tp = _tp map translate_Type getOrElse({
          inferType(tm)})
        val name = LocalName("funcReg:"+pos.position)
        Some(if (isQualified) {
          qualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs)
        } else {
          unqualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs)(NotationContainer.empty())
        })
      case Property_Registration(_props, _just) => _props.property map {_ =>
        translate_JustifiedProperty(JustifiedProperty(_props, None, Some(_just)))
      }
    }
    (resDecl.map(tr)).map(List(_)).getOrElse(Nil)
  }
  def translate_Identify(_fstPat:Patterns, _sndPat:Patterns)(implicit definitionContext: DefinitionContext = DefinitionContext.empty()): List[Declaration] = {
    val tr = namedDefArgsTranslator()
    /*val num = getIdentifyCount()
    incrementIdentifyCount()
    val name = LocalName("identify"+num)
    val (_, _, f, _, fparams) = translate_Referencing_Pattern(_fstPat)
    val (_, _, g, _, gparams) = translate_Referencing_Pattern(_sndPat)
    val tpO = Some(Mizar.eq(ApplyGeneral(f, fparams), ApplyGeneral(g, gparams)))
    val resDecls = List(makeConstant(name, tpO, None))*/
    //Since the patterns don't contains global Ids, it is currently impossible to translate them
    val resDecls : List[Declaration] = Nil
    resDecls map tr
  }
  def translate_Cluster(cl:Cluster)(implicit definitionContext: DefinitionContext = DefinitionContext.empty()): List[Declaration] = {
    //TODO: Also translate the proofs of the correctness conditions
    cl._registrs flatMap translate_Registration
  }
}
object blockTranslator {
  def collectSubitems[mainSort <: BlockSubitem](block: Block) : (Context, List[Term], List[(Term, Option[Term])], List[(mainSort, List[JustifiedCorrectnessConditions], List[Property])]) = {
    val items = block._items
    implicit var args: Context = Context.empty
    implicit var assumptions: List[Term] = Nil
    implicit var usedFacts: List[(Term, Option[Term])] = Nil
    var resDecls: List[(mainSort, List[JustifiedCorrectnessConditions], List[Property])] = Nil

    items.zipWithIndex foreach { case (it: Item, ind: Int) =>
      it._subitem match {
        case loci_Declaration: Loci_Declaration =>
          args = args ++ subitemTranslator.translate_Loci_Declaration(loci_Declaration)
        case ass: Claim => assumptions +:= translate_Claim(ass)
        case Assumption(ass) => assumptions +:= translate_Claim(ass)
        //We put the guard, since the type pattern is eliminated by the compiler, it is for better error messages only, as we shouldn't ever encounter any other subitems at this point
        case defn : mainSort if (defn.isInstanceOf[BlockSubitem]) =>
          implicit var corr_conds: List[JustifiedCorrectnessConditions] = Nil
          implicit var props: List[Property] = Nil
          items.drop(ind).map(_._subitem match {
            case Correctness_Condition(_cor, _justO) => (true, Some(JustifiedCorrectnessConditions.translate(List(_cor), _justO)))
            case Correctness(_cor, _just) => (true, Some(JustifiedCorrectnessConditions.translate(_cor._cond, Some(_just))))
            case prop: Property => (true, Some(prop))
            case _ => (false, None)
          }).takeWhile(_._1) foreach(_._2.get match {
            case just: JustifiedCorrectnessConditions => corr_conds +:= just
            case prop : Property => props +:= prop
          })
          resDecls +:= (defn, corr_conds, props)
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
        case choice_Statement: Choice_Statement =>
          val (addArgs, addFacts) = translate_Choice_Statement(choice_Statement)
          args ++= addArgs
          usedFacts +:= addFacts
        case statement: Regular_Statement =>
          usedFacts +:= translate_Proved_Claim(statement.prfClaim)
        case defIt => throw DeclarationTranslationError("Unexpected item of type " + defIt.kind+" found, in "+block.kind+" at line "+(it.pos.startPosition().line+1).toString+" in file "+currentAid+".miz", defIt)
      }
    }
    (args, assumptions, usedFacts, resDecls)
  }
  def translate_Definitional_Block(block:Block):List[Declaration] = {
    val (arguments, assumptions, usedFacts, definitionItems) = collectSubitems[Definition](block)
    definitionItems flatMap {
      case (defn: Definition, corConds: List[JustifiedCorrectnessConditions], props: List[Property]) =>
        implicit val defContext = DefinitionContext(arguments, assumptions, corConds, props, usedFacts)
        translate_Definition(defn)
    }
  }
  def translate_Registration_Block(block: Block) : List[Declaration] = {
    val (arguments, assumptions, usedFacts, clusterItems) = collectSubitems[RegistrationSubitems](block)
    clusterItems flatMap {
      case (cl: Cluster, corConds: List[JustifiedCorrectnessConditions], props: List[Property]) =>
        implicit val defContext = DefinitionContext(arguments, assumptions, corConds, props, usedFacts)
        translate_Cluster(cl)
      case (reg: Registrations, corConds: List[JustifiedCorrectnessConditions], props: List[Property]) =>
        implicit val defContext = DefinitionContext(arguments, assumptions, corConds, props, usedFacts)
        translate_Registration(reg)
      case (Identify(_firstPat, _sndPat, _lociEqns), corConds: List[JustifiedCorrectnessConditions], props: List[Property]) =>
        val defContextPrev = DefinitionContext(arguments, assumptions, corConds, props, usedFacts)
        val translatedLociEqns = translate_Loci_Equalities(_lociEqns)(defContextPrev)
        implicit val defContext = defContextPrev.addAssumptions(translatedLociEqns)
        translate_Identify(_firstPat, _sndPat)
    }
  }
  def translate_Notation_Block(block: Block): List[Declaration] = {
    val (arguments, assumptions, usedFacts, notationItems) = collectSubitems[Nyms](block)
    notationItems flatMap {
      case (nym: Nyms, corConds: List[JustifiedCorrectnessConditions], props: List[Property]) =>
        implicit val defContext = DefinitionContext(arguments, assumptions, corConds, props)
        translate_Nym(nym)
    }
  }
}