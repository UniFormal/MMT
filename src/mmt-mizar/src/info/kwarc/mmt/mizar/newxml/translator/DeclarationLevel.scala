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
import info.kwarc.mmt.mizar.newxml.translator.TranslationController.{addUnresolvedDependency, controller, currentAid, getAnonymousTheoremCount, getIdentifyCount, getUnresolvedDependencies, incrementAnonymousTheoremCount, incrementIdentifyCount, inferType, localPath, makeConstant, makeConstantInContext}
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

private[translator] class ThesisTerm(tm: Term) {
  private def matchTerm(tm: Term): (List[VarDecl], List[Term], List[VarDecl], List[VarDecl], List[Term]) = tm match {
    case PiOrEmpty(ctx, tm) =>
      val args = ctx map (vd => vd.copy(tp = vd.tp))
    def matchConds(tm: Term): (List[Term], Term) = tm match {
      case implies(cond, rest) =>
        val (conds, body) = matchConds(rest)
        (cond::conds, body)
      case iff(a, b) => matchConds(And(List(implies(a, b), implies(b, a))))
      case _ => (Nil, tm)
    }
      val (conds, actualClaim) = matchConds(tm)
    def existentialQuantifier(tm: Term): (Context, Term) = tm match {
        case exists(vname, vtype, body) =>
          val (ctx, tm) = existentialQuantifier(body)
          (vname % TranslationController.simplifyTerm(vtype) :: ctx, tm)
        case _ => (Context.empty, tm)
      }
      val (existQuants, body) = existentialQuantifier(actualClaim)

      def univQuantifier(tm: Term): (Context, Term) = tm match {
        case forall(vname, vtype, body) =>
          val (ctx, tm) = univQuantifier(body)
          (vname % TranslationController.simplifyTerm(vtype) :: ctx, tm)
        case _ => (Context.empty, tm)
      }
      val (univQuants, conjunction) = univQuantifier(body)
      val conjuncts = conjunction match  {
        case And(conjuncts) => conjuncts// map TranslationController.simplifyTerm
        case tm => List(tm)//TranslationController.simplifyTerm(tm))
      }
      (args, conds, existQuants, univQuants, conjuncts)
    case _ => throw ImplementationError("This can't ever happen.")
  }
  private var args: List[VarDecl] = matchTerm(tm)._1
  private var conditions: List[Term] = matchTerm(tm)._2
  private var existQuants: Context = matchTerm(tm)._3
  private var univQuants: Context = matchTerm(tm)._4
  private var conjuncts: List[Term] = matchTerm(tm)._5
  def getConjuncts: List[Term] = conjuncts
  def getDisjuncts: List[Term] = conjuncts.headOption match {
    case Some(Or(disjs)) => disjs
    case Some(other) => List(other)
    case None => List(trueCon)
  }
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
  def nextExistVar(vO: Option[OMV] = None): Option[VarDecl] = vO flatMap(v => existQuants find(_.name == v.name)) orElse(existQuants.variables.headOption orElse({
    conjuncts.headOption match {
      case Some(hd) => val subThesis = new ThesisTerm(hd)
        if (subThesis.conjuncts.head == conjuncts.head) None else subThesis.nextExistVar(vO)
      case None => None
    }
  }))
  def nextUnivVar(vO: Option[OMV] = None): VarDecl = vO flatMap (v => univQuants.find (_.name == v.name)) getOrElse univQuants.variables.head
  def killConjunct(tm: Term) = conjuncts match {
    case Nil => Nil
    case hd::tl => if (tm == hd) conjuncts = conjuncts.drop(1)
  }
  def killDisjunct(tm: Term) = {
    val disjuncts = getDisjuncts
    assert(tm == disjuncts.head)
    conjuncts = conjuncts match {
      case hd::tl => Or(disjuncts.tail)::conjuncts.tail
      case Nil => List(trueCon)
    }
  }
  def killCond(tm: Term) = conditions match {
    case Nil =>
    case hd::tl =>
      if (tm == conditions.head) conditions = conditions.drop(1)
  }
  def doneProving(): Boolean = (conditions ++ existQuants ++ univQuants).isEmpty && conjuncts == Nil
  def toTerm(implicit defContext: DefinitionContext): Term = {
    val allConjunctions = And(conjuncts)
    val univQuantified = translate_Universal_Quantifier_Formula(univQuants, allConjunctions, None)(defContext)
    val existQuantified = translate_Existential_Quantifier_Formula(existQuants, univQuantified, None)(defContext)
    val furtherArgs = defContext.args.filter(existQuantified.freeVars contains _)
    PiOrEmpty(furtherArgs, conditions.foldRight(existQuantified)(implies(_, _)))
  }
  def subobjects: List[(Context, Obj)] = (conditions ++ existQuants ++ univQuants).flatMap(_.subobjects) ++ conjuncts.flatMap(_.subobjects)
}
private[translator] object ThesisTerm {
  def trivial() : ThesisTerm = {
    var ths = new ThesisTerm(trueCon)
    ths.conjuncts = Nil
    ths
  }
}
case class DefinitionContext(var args: Context = Context.empty, var assumptions: List[Term] = Nil, var usedFacts: List[(Term, Option[Term])] = Nil, corr_conds: List[JustifiedCorrectnessConditions] = Nil, props: List[Property] = Nil, private var thesis: List[ThesisTerm] = Nil) {
  private var localDefinitions: Option[List[(LocalName, Term)]] = None
  def addLocalDefinition(n: LocalName, defn: Term) = {
    assert (withinProof)
    val toAdd = (n, defn)
    localDefinitions = Some(localDefinitions.getOrElse(Nil):+toAdd)
  }
  def lookupLocalDefinitionWithinSameProof(n: LocalName): Option[Term] = {
    if (withinProof) localDefinitions.flatMap(_.find(_._1 == n)).map(_._2) else None
  }
  def addArguments(arguments: Context): Unit = { this.args ++= arguments }
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
  private def notWithinProofError = new TranslatingError("Trying to access thesis (seemingly) outside of a proof. ")
  def killArg(arg: VarDecl): Unit = thesis.headOption foreach (_.killArg(arg))
  def killArguments(ctx: Context): Unit = ctx foreach (killArg(_))
  def killConjunct(conjunct: Term): Unit = thesis.headOption foreach (_.killConjunct(conjunct))
  def getConjuncts(): List[Term] = thesis.headOption map (_.getConjuncts) getOrElse (throw notWithinProofError)
  def killDisjunct(disjunct: Term): Unit = thesis.headOption foreach (_.killDisjunct(disjunct))
  def getDisjuncts(): List[Term] = thesis.headOption map (_.getDisjuncts) getOrElse (throw notWithinProofError)
  def killAssumption(assumption: Term): Unit = thesis.headOption foreach (_.killCond(assumption))
  def killExistQuant(existQuant: VarDecl): Unit = thesis.headOption foreach (_.killExistVar(existQuant))
  def popExistQuant(v: Option[OMV] = None): Option[VarDecl] = thesis match {
    case thesisTm::_ =>
    val existQuant = thesis.find(_.nextExistVar(v).isDefined).flatMap(_.nextExistVar(v))
      existQuant foreach killExistQuant
    existQuant
    case Nil => throw notWithinProofError
  }
  def killUnivQuant(univQuant: VarDecl): Unit = thesis.headOption foreach (_.killUnivVar(univQuant))
  def popUnivQuant(v: Option[OMV] = None): VarDecl = thesis match {
    case thesisTm::_ =>
      val univQuant = thesisTm.nextUnivVar(v)
      killUnivQuant(univQuant)
      univQuant
    case Nil => throw notWithinProofError
  }
  private[translator] def pushThesis(claim: Term): Unit = {
    thesis ::= new ThesisTerm(claim)
  }
  private[translator] def pushThesis(claim: ThesisTerm): Unit = {
    thesis ::= claim
  }
  def getThesisAsTerm: Term = thesis match {
    case Nil => throw notWithinProofError
    case thesisTerm::_ => thesisTerm.toTerm(this)
  }
  def popThesis: ThesisTerm = thesis match {
    case hd::tl =>
      thesis = tl
      hd
    case Nil => throw notWithinProofError
  }
  def withinProof: Boolean = ! thesis.isEmpty
  def trivialThesis = thesis.map(_.doneProving()).headOption getOrElse false
}
object DefinitionContext {
  def empty() = DefinitionContext()
}

case class JustifiedProperty(conds: List[MizarProperty], prop: MizarProperty, tp: Option[Term], decl: Option[Declaration])
object JustifiedProperty {
  def apply(property: Property, decls: List[Declaration])(implicit definitionContext: DefinitionContext) : JustifiedProperty = apply(property, decls.headOption)
  def apply(property: Property, decl: Option[Declaration])(implicit definitionContext: DefinitionContext) : JustifiedProperty = apply(property._props, decl, property._just)
  def apply(properties: Properties, decl: Option[Declaration], justO: Option[Justification] = None)(implicit definitionContext: DefinitionContext) : JustifiedProperty = {
    val prop = properties.matchProperty(justO)
    val conds = properties._cond.flatMap(_.matchProperty())
    val tp = properties._tp map translate_Type
    JustifiedProperty(conds, prop.get, tp, decl)
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
    PrePostfix(Delim(del), infixArgNum, infixArgNum+suffixArgNum)
  }
  private def CircumfixMarkers(leftDel: String, rightDel: String, circumfixArgNr: Int) = {
    Circumfix(Delim(leftDel), Delim(rightDel), circumfixArgNr)
  }
  private def makeNotCont(fixity: Fixity): NotationContainer = {
    /*def reasonableChar: Char => Boolean = {c => !("#" == c)}//c.isLetterOrDigit || c.isWhitespace || "|.<>=*+-()/&^%$@!~,".contains(c)}
    val reasonable = fixity match {
      case Circumfix(lDelim, rDelim, num) => (lDelim++rDelim).forall(reasonableChar)
      case fixity: SimpleFixity => fixity.delim.text.forall(reasonableChar)
      case _ => false
    }*/
    def admissable(fixity: Fixity): Boolean = ! (fixity.toString.drop(2) exists("#" contains _))
    NotationContainer(TextNotation(fixity = fixity, precedence = Precedence.integer(ones = 20), meta = None))
  }

  def makeNotationCont(del: String, infixArgNum: Int, suffixArgNum: Int, rightArgsBracketed: Boolean = false) = {
    makeNotCont(PrePostFixMarkers(del, infixArgNum, suffixArgNum, rightArgsBracketed))
  }
  def translate_Pattern(pattern:Patterns, orgVersion: Boolean = false) : (LocalName, GlobalName, NotationContainer) = {
    val referencedGn = computeGlobalName(pattern, orgVersion)
    val gn = if (referencedGn.toString contains "hidden") {
      resolveHiddenReferences(referencedGn) match {
        case Some(OMS(p)) => p
        case _ => referencedGn
      }
    } else referencedGn
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
    val pat: Patterns = pse._pat match {
      case Mode_Pattern(patternAttrs, _locis) => Mode_Pattern(patternAttrs.copy(globalObjAttrs = patternAttrs.globalObjAttrs.copy(globalKind = "M")), _locis)
      case pattern: ConstrPattern =>
        val kind = pattern.extPatDef.extPatAttr.constr.head.toString
        def extPatAttrNew = pattern.extPatAttr.copy(patAttr = pattern.extPatAttr.patAttr.copy(globalObjAttrs = pattern.extPatAttr.patAttr.globalObjAttrs.copy(globalKind = kind)))
        pattern match {
        case patterns: RedefinablePatterns =>
          def orgExtPatAttrNew = patterns.orgExtPatAttr.copy(extPatAttr = extPatAttrNew)
          patterns match {
          case p:Attribute_Pattern => p.copy(orgExtPatAttr = orgExtPatAttrNew)//"V"
          case p:Predicate_Pattern => p.copy(orgExtPatAttr = orgExtPatAttrNew)//"R"
          case p:Strict_Pattern => p.copy(orgExtPatAttr = orgExtPatAttrNew)//"V"
          case p:InfixFunctor_Pattern => p.copy(orgExtPatAttr = orgExtPatAttrNew)//"K"
          case p:CircumfixFunctor_Pattern => p.copy(orgExtPatAttr = orgExtPatAttrNew)//"K"
        }
        case sp:Structure_Pattern => sp.copy(extPatAttr = extPatAttrNew)//"L"
        case p:AggregateFunctor_Pattern => p.copy(extPatAttr = extPatAttrNew)//"G"
        case p:ForgetfulFunctor_Pattern => p.copy(extPatAttr = extPatAttrNew)//"U"
        case p:SelectorFunctor_Pattern => p.copy(extPatAttr = extPatAttrNew)//"J"
      }
    }
    val params = pat._locis.flatMap(_._loci map translate_Locus)
    val (name, gn, notC) = translate_Pattern(pat)
    val referencedDeclSE = controller.getO(gn) getOrElse ({
      throw PatternTranslationError("Error looking up the declaration at "+gn.toString+" referenced by pattern: "+pat+", but no such Declaration found. "
        +"Probably this is because we require the dependency theory "+gn.module+" of the article currently being translated "+localPath+" to be already translated: \n"
        +"Please make sure the theory is translated (build with mizarxml-omdoc build target) and try again. ", pat)
      if (! (getUnresolvedDependencies() contains gn.module)) {
        addUnresolvedDependency(gn.module)
      }
      throw new ObjectLevelTranslationError("Trying to lookup declaration (presumably) at "+gn.toString+" referenced by pattern-shaped-expression: "+pse+", but no such Declaration found. ", pat)})
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
  def translate_JustifiedProperty(justProp: JustifiedProperty)(implicit definitionContext: DefinitionContext): Declaration = justProp match {
    case JustifiedProperty(conds, Sethood(_just), Some(tp), _) =>
      val claim = Apply(constant("sethood"), tp)
      val just = _just map (translate_Justification(_, claim)) getOrElse None
      val name = LocalName("sethood_of_"+tp.toStr(true))
      makeConstantInContext(name, Some(Univ(1)), just)
    case _ => ???
  }
}

object subitemTranslator {
  private def notToplevel = new TranslatingError("This kind of declaration should not occur on toplevel. ")
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
  def translate_Correctness(correctness: Correctness) = { throw notToplevel }
  def translate_Correctness_Condition(correctness_Condition: Correctness_Condition) = { throw notToplevel }
  def translate_Exemplification(exemplification: Exemplification) = { throw notToplevel }
  def translate_Assumption(assumption: Assumption) = { throw notToplevel }
  def translate_Identify(identify: Identify)(implicit defContext: DefinitionContext) = { clusterTranslator.translate_Identify(identify._firstPat, identify._sndPat) }
  def translate_Generalization(generalization: Generalization) = { throw notToplevel }
  def translate_Reduction(reduction: Reduction) = { ??? }
  def translate_Scheme_Block_Item(scheme_Block_Item: Scheme_Block_Item)(implicit defContext: => DefinitionContext = DefinitionContext.empty()) = scheme_Block_Item match {
    case sbi @ Scheme_Block_Item(_, _block) =>
      val gn = sbi.globalName()
      val provenSentence = sbi.provenSentence()
      val Scheme_Head(_sch, _vars, _form, _provForm) = sbi.scheme_head()
      _vars._segms foreach (segm => translateBindingVariables(segm)(defContext))
      val ass : List[Term] = _provForm.map(_._props map(translate_Claim(_)(defContext))) getOrElse Nil
      implicit val notC = makeNotationCont(_sch.spelling, 0, defContext.args.length, true)
      val (p, prf) = translate_Proved_Claim(provenSentence)(defContext)
      TranslationController.articleStatistics.incrementStatisticsCounter("scheme")
      List(schemeDefinitionInstance(gn.name, defContext.args map (_.tp.get), ass, p, prf))
  }
  def translate_Property(property: Property, decl: Option[Definition])(implicit defContext: DefinitionContext) : List[Declaration] = {
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
  def translate_Nym(nym:Nyms)(implicit defContext: DefinitionContext): List[Declaration] = {
    val oldPatRef = nym._patRefOld
    val newPat: Patterns = nym._patNew
    val (name, newName, notC) = translate_Pattern(newPat)
    try {
      val (_, addArgsTps, mainDecl, _, _) = translate_Referencing_Pattern(oldPatRef)
      val allArgs = defContext.args.map(_.tp.get) ++ addArgsTps
      val res  = synonymicNotation(newName.name, allArgs.length, allArgs, mainDecl)(notC)
      TranslationController.articleStatistics.incrementStatisticsCounter("nym")
      List(res)
    } catch {
      case e: ObjectLevelTranslationError => Nil
    }
  }
}

object statementTranslator {
  def translate_Statement(st:Statement)(implicit defContext: DefinitionContext) = st match {
    case conclusion: Conclusion => translate_Conclusion(conclusion)
    case type_Changing_Statement: Type_Changing_Statement => translate_Type_Changing_Statement(type_Changing_Statement)
    case theorem_Item: Theorem_Item => translate_Theorem_Item(theorem_Item)
    case choice_Statement: Choice_Statement =>
      val (addArgs, (claim, proof)) = translate_Choice_Statement(choice_Statement)
      incrementAnonymousTheoremCount()
      val gn = makeNewGlobalName("Choice_Statement", getAnonymousTheoremCount())
      implicit val defCtx = defContext.copy(args = defContext.args ++ addArgs, assumptions = defContext.assumptions :+ claim)
      val theoremDecl = makeConstantInContext(gn.name, Some(claim), proof)(defContext = defCtx)
      List(theoremDecl)
    case regular_Statement: Regular_Statement => translate_Regular_Statement(regular_Statement)
  }
  def translate_Conclusion(conclusion: Conclusion) = { Nil }
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement)(implicit defContext: => DefinitionContext) : List[Declaration] = {
    incrementAnonymousTheoremCount()
    val gn = makeNewGlobalName("Type_Changing_Statement", getAnonymousTheoremCount())
    val (claim, proof) = translate_Proved_Claim(type_Changing_Statement.prfClaim)(defContext)
    List(makeConstantInContext(gn.name, Some(claim), proof))
  }
  def translate_Theorem_Item(theorem_Item: Theorem_Item)(implicit defContext: DefinitionContext) = {
    val prfedClaim = theorem_Item.prfClaim
    implicit val gn = theorem_Item.referencedLabel()//mMLIdtoGlobalName(theorem_Item.mizarGlobalName())
    val (claim, proof) = translate_Proved_Claim(theorem_Item.prfClaim)
    TranslationController.articleStatistics.incrementStatisticsCounter("thm")
    List(makeConstantInContext(gn.name, Some(claim), proof))
  }
  def translate_Choice_Statement(choice_Statement: Choice_Statement)(implicit defContext: DefinitionContext): (Context, (Term, Option[Term])) = {
    val vars: Context = choice_Statement._qual._children.flatMap(translate_Context(_))
    val facts = translate_Proved_Claim(choice_Statement.prfClaim)
    (vars, facts)
  }
  def translate_Regular_Statement(regular_Statement: Regular_Statement)(implicit defContext: DefinitionContext) = {
    incrementAnonymousTheoremCount()
    val gn = makeNewGlobalName("Regular-Statement", getAnonymousTheoremCount())
    val (claim, proof) = translate_Proved_Claim(regular_Statement.prfClaim)
    val theoremDecl = makeConstantInContext(gn.name, Some(claim), proof)
    List(theoremDecl)
  }
}

object definitionTranslator {
  def translate_Definition(defn:Definition)(implicit defContext: => DefinitionContext) : List[Declaration] = {
    val translatedDecls: List[Declaration] = defn match {
      case d: Private_Functor_Definition  => translate_Private_Functor_Definition(d)(defContext)
      case d: Private_Predicate_Definition => translate_Private_Predicate_Definition(d)(defContext)
      case at: Attribute_Definition => translate_Attribute_Definition(at)(defContext)
      case cd: Constant_Definition => translate_Constant_Definition(cd)(defContext)
      case funcDef: Functor_Definition => translate_Functor_Definition(funcDef)(defContext)
      case md: Mode_Definition => translate_Mode_Definition(md)(defContext)
      case pd: Predicate_Definition => translate_Predicate_Definition(pd)(defContext)
      case d: Structure_Definition => translate_Structure_Definition(d)(defContext)
    }
    val justProps = defContext.props.filter(_._props.property.isDefined).map(p => JustifiedProperty(p, translatedDecls)(defContext))
    translatedDecls ++ justProps.map(translate_JustifiedProperty(_)(defContext))
  }
  def translate_Structure_Definition(strDef: Structure_Definition)(implicit defContext: DefinitionContext): List[Declaration] = {
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
  def translate_Attribute_Definition(attribute_Definition: Attribute_Definition)(implicit defContext: DefinitionContext) = attribute_Definition match {
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
        TranslationController.articleStatistics.incrementStatisticsCounter("attr")
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
  def translate_Constant_Definition(constant_Definition: Constant_Definition)(implicit defContext: DefinitionContext): List[Constant] = {
    constant_Definition._children map {eq =>
      val name = LocalName(eq._var.varAttr.copy(kind = "DefConstant").toIdentifier)
      val df = translate_Term(eq._tm)
      val notC = makeNotationCont(eq._var.varAttr.spelling, 0, 0)
      makeConstantInContext(name, None, Some(df))(notC)
    }
  }
  def translate_Functor_Definition(functor_Definition: Functor_Definition)(implicit defContext: DefinitionContext) = functor_Definition match {
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
      TranslationController.articleStatistics.incrementStatisticsCounter("funct")
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
  def translate_Mode_Definition(mode_Definition: Mode_Definition)(implicit defContext: DefinitionContext) = {
    val (name, declarationPath, notC) = translate_Pattern(mode_Definition._pat)
    implicit val notCon = notC
    mode_Definition._expMode match {
      case Expandable_Mode(_tp) =>
        val tp = translate_Type(_tp)
        List(makeConstantInContext(declarationPath.name, Some(tp), Some(tp)))
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
          TranslationController.articleStatistics.incrementStatisticsCounter("mode")
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
  def translate_Private_Functor_Definition(private_Functor_Definition: Private_Functor_Definition)(implicit defContext: => DefinitionContext) = {
    val v = translate_Variable(private_Functor_Definition._var)(defContext)
    val gn = makeNewGlobalName("Private-Functor", private_Functor_Definition._var.varAttr.locVarAttr.serialNrIdNr.idnr)
    val args: Context = private_Functor_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map({case (tp, i) => OMV("placeholder_"+i.toString) % tp})
    val tp = PiOrEmpty(args, any)
    val dfBody = translate_Term(private_Functor_Definition._tm)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    val res = makeConstantInContext(gn.name, Some(tp), Some(df))
    if (! defContext.withinProof) List(res) else {
      defContext.addLocalDefinition(gn.name, df)
      Nil
    }
  }
  def translate_Private_Predicate_Definition(private_Predicate_Definition: Private_Predicate_Definition)(implicit defContext: => DefinitionContext) = {
    val v = translate_Variable(private_Predicate_Definition._var)(defContext)
    val gn = makeNewGlobalName("Private-Predicate", private_Predicate_Definition._var.varAttr.locVarAttr.serialNrIdNr.idnr)
    val args: Context = private_Predicate_Definition._tpList._tps.map(translate_Type(_)(defContext)).zipWithIndex map({case (tp, i) => OMV("placeholder_"+i.toString) % tp})
    val tp = PiOrEmpty(args, prop)
    val dfBody = translate_Formula(private_Predicate_Definition._form)(defContext)
    val df = LambdaOrEmpty(args, dfBody)
    val res = makeConstantInContext(gn.name, Some(tp), Some(df))
    if (! defContext.withinProof) List(res) else {
      defContext.addLocalDefinition(gn.name, df)
      Nil
    }
  }
  def translate_Predicate_Definition(predicate_Definition: Predicate_Definition)(implicit defContext: DefinitionContext) = predicate_Definition match {
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
        TranslationController.articleStatistics.incrementStatisticsCounter("pred")
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
      val constrNrPart = origDecl.name.toString.tail.dropWhile(_ != 'K')
      val constrNumBasedName = mod.domain.filter(_.toString.endsWith(constrNrPart)) match {
        case List(name) => Some(name)
        case hd::_ =>
          //TODO: Talk with Artur and figure out how to avoid this situation
          Some(hd)
        case Nil => None
      }
      constrNumBasedName map mod.get
    } match {
      case Some(c: Constant) => makeConstant(gn.name, Some(ret getOrElse c.tp.get), c.df)(notCon)
      case Some(MizarPatternInstance(ln, pat, params)) => MizarPatternInstance(gn.name, pat, params)
      case _ =>
        throw PatternTranslationError("Failure to look up the referenced definition to redefine at "+origDecl.module.last+"?"+origDecl.name+"(full path "+origDecl+"). ", pat)
    }
  }
}

object clusterTranslator {
  def translate_Registration(reg: Registrations)(implicit definitionContext: DefinitionContext): List[Declaration] = {
    TranslationController.articleStatistics.incrementStatisticsCounter("registr")
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
          unqualifiedFunctorRegistration(name, definitionContext.args map(_.tp.get), tp, tm, adjs)
        })
      case Property_Registration(_props, _just) => _props.property map {_ =>
        translate_JustifiedProperty(JustifiedProperty(_props, None, Some(_just)))
      }
    }
    resDecl.map(List(_)).getOrElse(Nil)
  }
  def translate_Identify(_fstPat:Pattern_Shaped_Expression, _sndPat:Pattern_Shaped_Expression)(implicit definitionContext: DefinitionContext): List[Declaration] = {
    incrementIdentifyCount()
    val num = getIdentifyCount()
    val name = LocalName("identify"+num)
    val (_, _, f, _, fparams) = translate_Referencing_Pattern(_fstPat)
    val (_, _, g, _, gparams) = translate_Referencing_Pattern(_sndPat)
    val tpO = Some(MizarPrimitiveConcepts.eq(ApplyGeneral(f, fparams), ApplyGeneral(g, gparams)))
    val resDecls = List(makeConstantInContext(name, tpO, None))
    resDecls
  }
  def translate_Cluster(cl:Cluster)(implicit definitionContext: DefinitionContext): List[Declaration] = {
    //TODO: Also translate the proofs of the correctness conditions
    cl._registrs flatMap translate_Registration
  }
}
object blockTranslator {
  def collectSubitems[mainSort <: BlockSubitem](block: Block) : List[(mainSort, DefinitionContext)] = {
    val items = block._items
    implicit var defContext = DefinitionContext.empty()
    var resDecls: List[(mainSort, DefinitionContext)] = Nil

    items.zipWithIndex foreach { case (it: Item, ind: Int) =>
      it._subitem match {
        case loci_Declaration: Loci_Declaration =>
          loci_Declaration._qualSegms._children foreach {segm =>
            defContext.addArguments(translate_Context(segm)(defContext))
          }
        case ass: Claim => defContext.addAssumption(translate_Claim(ass))
        case Assumption(ass) => defContext.addAssumption(translate_Claim(ass))
        case choice_Statement: Choice_Statement =>
          val (addArgs, addFacts) = translate_Choice_Statement(choice_Statement)
          defContext.addArguments(addArgs)
          defContext.addUsedFact(addFacts)
        case statement: Regular_Statement =>
          defContext.addUsedFact(translate_Proved_Claim(statement.prfClaim))
        //We put the guard, since the type pattern is eliminated by the compiler, it is for better error messages only, as we shouldn't ever encounter any other subitems (not to be ignored) at this point
        case defn : mainSort if (defn.isInstanceOf[BlockSubitem]) =>
          implicit var corr_conds: List[JustifiedCorrectnessConditions] = Nil
          implicit var props: List[Property] = Nil
          items.drop(ind).foreach(_._subitem match {
            case Correctness_Condition(_cor, _justO) => corr_conds ++:= JustifiedCorrectnessConditions.translate(List(_cor), _justO).reverse
            case Correctness(_cor, _just) => corr_conds ++:= JustifiedCorrectnessConditions.translate(_cor._cond, Some(_just)).reverse
            case prop: Property => props ::= prop
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
        case defIt => throw DeclarationTranslationError("Unexpected item of type " + defIt.kind+" found, in "+block.kind+" at line "+(it.pos.startPosition().line+1).toString+" in file "+currentAid+".miz", defIt)
      }
    }
    resDecls
  }
  def translate_Definitional_Block(block:Block):List[Declaration] = {
    val definitionItems = collectSubitems[Definition](block)
    definitionItems flatMap {
      case (defn: Definition, defContext: DefinitionContext) =>
        translate_Definition(defn)(defContext)
    }
  }
  def translate_Registration_Block(block: Block) : List[Declaration] = {
    val clusterItems = collectSubitems[RegistrationSubitems](block)
    clusterItems flatMap {
      case (cl: Cluster, defContext: DefinitionContext) =>
        translate_Cluster(cl)(defContext)
      case (reg: Registrations, defContext: DefinitionContext) =>
        translate_Registration(reg)(defContext)
      case (Identify(_firstPat, _sndPat, _lociEqns), defContext: DefinitionContext) =>
        val translatedLociEqns = translate_Loci_Equalities(_lociEqns)(defContext)
        defContext.addAssumptions(translatedLociEqns)
        translate_Identify(_firstPat, _sndPat)(defContext)
    }
  }
  def translate_Notation_Block(block: Block): List[Declaration] = {
    val notationItems = collectSubitems[Nyms](block)
    notationItems flatMap {
      case (nym: Nyms, defContext: DefinitionContext) =>
        translate_Nym(nym)(defContext)
    }
  }
}
