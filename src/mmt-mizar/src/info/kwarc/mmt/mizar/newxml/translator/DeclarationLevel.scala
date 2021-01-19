package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api._
import notations._
import info.kwarc.mmt.api.objects.{Context, VarDecl}
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mizar.newxml.mmtwrapper._
import info.kwarc.mmt.mizar.newxml.syntax._
import expressionTranslator._
import termTranslator._
import typeTranslator._
import contextTranslator._
import formulaTranslator._
import TranslatorUtils._
import definiensTranslator._
import info.kwarc.mmt.mizar.newxml.mmtwrapper.MMTUtils.Lam
import info.kwarc.mmt.mizar.newxml.mmtwrapper.Mizar.any
import info.kwarc.mmt.mizar.newxml.translator.claimTranslator.translate_Claim
import info.kwarc.mmt.mizar.newxml.translator.clusterTranslator.translate_Cluster
import info.kwarc.mmt.mizar.newxml.translator.definitionTranslator.translate_Definition
import info.kwarc.mmt.mizar.newxml.translator.nymTranslator.translate_Nym
import patternTranslator._

object subitemTranslator {
  def translate_Reservation(reservation: Reservation) = { Nil }
  def translate_Definition_Item(definition_Item: Definition_Item) = {
    definition_Item.check() match {
      case "Definitional-Block" => blockTranslator.translate_Definitional_Block(definition_Item._block)
      case "Registration-Block" => blockTranslator.translate_Registration_Block(definition_Item._block)
      case "Notation-Block" => blockTranslator.translate_Notation_Block(definition_Item._block)
    }
  }
  def translate_Section_Pragma(section_Pragma: Section_Pragma) = { Nil }
  def translate_Pragma(pragma: Pragma) = { Nil }
  def translate_Loci_Declaration(loci_Declaration: Loci_Declaration): Context = {
    loci_Declaration._qualSegms._children flatMap(translate_Context(_))
  }
  def translate_Cluster(cluster: Cluster) = { ??? }
  def translate_Correctness(correctness: Correctness) = { ??? }
  def translate_Correctness_Condition(correctness_Condition: Correctness_Condition) = { ??? }
  def translate_Exemplification(exemplification: Exemplification) = { ??? }
  def translate_Assumption(assumption: Assumption) = { ??? }
  def translate_Identify(identify: Identify) = { ??? }
  def translate_Generalization(generalization: Generalization) = { ??? }
  def translate_Reduction(reduction: Reduction) = { ??? }
  def translate_Scheme_Block_Item(scheme_Block_Item: Scheme_Block_Item) = { ??? }
  def translate_Property(property: Property) = { ??? }
  def translate_Per_Cases(per_Cases: Per_Cases) = { ??? }
  def translate_Case_Block(case_block: Case_Block) = { ??? }
}

object headTranslator {
  def translate_Head(head:Heads) = { ??? }
  def translate_Scheme_Head(reservation: Scheme_Head) = {}
  def translate_Suppose_Head(reservation: Suppose_Head) = {}
  def translate_Case_Head(reservation: Case_Head) = {}
}

object nymTranslator {
  def translate_Nym(nym:Nyms)(implicit cor_conds: List[JustifiedCorrectnessConditions] = Nil, args: Context = Context.empty): List[symbols.Declaration] = {
    val oldPat = nym._patOld
    val newPat: Patterns = nym._patNew
    val (name, newName, notC) = translate_Pattern(newPat)
    val (_, addArgsTps, mainDecl, _) = patternTranslator.translate_Referencing_Pattern(oldPat)
    val allArgs = args.map(_.tp.get) ++ addArgsTps
    val tr = namedDefArgsTranslator()
    List(tr(synonymicNotation(newName.name, allArgs.length, allArgs, mainDecl)))
  }
}

object statementTranslator {
  def translate_Statement(st:Statement) = st match {
    case conclusion: Conclusion => translate_Conclusion(conclusion)
    case type_Changing_Statement: Type_Changing_Statement => translate_Type_Changing_Statement(type_Changing_Statement)
    case theorem_Item: Theorem_Item => translate_Theorem_Item(theorem_Item)
    case choice_Statement: Choice_Statement => translate_Choice_Statement(choice_Statement)
    case regular_Statement: Regular_Statement => translate_Regular_Statement(regular_Statement)
  }
  def translate_Conclusion(conclusion: Conclusion) = { ??? }
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement) = { ??? }
  def translate_Theorem_Item(theorem_Item: Theorem_Item)(implicit args: Context=Context.empty) = {
    val prfedClaim = theorem_Item.prfClaim
    prfedClaim.check()
    implicit val gn = TranslatorUtils.MMLIdtoGlobalName(theorem_Item.mizarGlobalName())
    val (_claim, _just) = (prfedClaim._claim, prfedClaim._just)
    val claim = translate_Claim(_claim)
    val proof = _just map {
      case justification: Justification => justificationTranslator.translate_Justification(justification, claim)
    } getOrElse((_claim match {case it:Iterative_Equality => Some(it) case _ => None}).map(justificationTranslator.translate_Iterative_Equality_Proof))

    val theoremDecl = TranslationController.makeConstant(gn.name, Some(claim), proof)
    List(theoremDecl)
  }
  def translate_Choice_Statement(reservation: Choice_Statement) = { ??? }
  def translate_Regular_Statement(regular_Statement: Regular_Statement) = { ??? }
}

object definitionTranslator {
  def translate_Definition(defn:Definition)(implicit args: Context= Context.empty, assumptions: List[objects.Term] = Nil, corr_conds: List[JustifiedCorrectnessConditions] = Nil, props: List[Property] = Nil) : List[info.kwarc.mmt.api.symbols.Declaration] = {
    val resDecls = defn match {
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
    resDecls map tr
  }
  def translate_Structure_Definition(strDef: Structure_Definition)(implicit args: Context= Context.empty): List[symbols.Declaration] = {
    val l = args.length
    implicit var selectors: List[(Int, VarDecl)] = Nil
    val substr: List[objects.Term] = strDef._ancestors._structTypes.map(translate_Type) map {
      case ApplyGeneral(typeDecl, args) => typeDecl
    }
    val n = substr.length
    var substitutions : List[objects.Sub] = Nil
    val declarationPath = MMLIdtoGlobalName(strDef._strPat.globalPatternName().copy(kind = "L"))//TranslatorUtils.computeGlobalPatternName(strDef._strPat)

    def translate_Field_Segments(field_Segments: Field_Segments)(implicit args: List[(Option[LocalName], objects.Term)]= Nil) : List[VarDecl] = field_Segments._fieldSegments flatMap {
      case field_Segment: Field_Segment =>
			val tp = translate_Type(field_Segment._tp)
      field_Segment._selectors._loci.reverse map { case selector =>

        val selName = objects.OMV(selector.spelling)//translate_Locus(selector._loci)
        val sel = (selector.posNr.nr, selName % tp)
        selectors ::= sel
        substitutions ::= selName / PatternUtils.referenceExtDecl(declarationPath, selName.name.toString)
        sel._2 ^ substitutions
      }
    }
    val fieldDecls = translate_Field_Segments(strDef._fieldSegms)
    val m = fieldDecls.length
    val trS = namedDefArgsSubstition()

    StructureInstance(declarationPath, l, args, n, substr, m, objects.Context.list2context(fieldDecls) ^ trS)
  }
  def translate_Attribute_Definition(attribute_Definition: Attribute_Definition)(implicit args: Context= Context.empty) = attribute_Definition match {
    case atd @ Attribute_Definition(a, _redef, _attrPat, _def) =>
      val (name, gn, notC) = translate_Pattern(_attrPat)
      implicit val notCon = notC
      val defn = _def.map(translate_Definiens(_))
      if (defn.isEmpty) {
        if(_redef.occurs) {
          val origDecl = TranslatorUtils.MMLIdtoGlobalName(_attrPat.globalPatternName())
          val oldDef = TranslationController.controller.get(origDecl) match {case decl: symbols.Constant => decl}
          val redef = TranslationController.makeConstant(name, oldDef.tp, oldDef.df)
          List(redef)
        } else {
          ???
        }
      }
      val motherTp = TranslationController.inferType(defn.get.someCase)
      val (argNum, argTps) = (args.length, args.map(_.tp.get))
      val atrDef = defn.get match {
        case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) => directPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes)
        case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) => indirectPartialAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes, defRes)
        case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) => directCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes)
        case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) => indirectCompleteAttributeDefinition(name, argNum, argTps, motherTp, defn.get.caseNum, cases, caseRes)
      }
      List(atrDef)
  }
  def translate_Constant_Definition(constant_Definition: Constant_Definition)(implicit args: Context= Context.empty) = { ??? }
  def translate_Functor_Definition(functor_Definition: Functor_Definition)(implicit args: Context= Context.empty) = functor_Definition match {
    case fd @ Functor_Definition(_, _redefine, _pat, _tpSpec, _def) =>
    val (name, gn, notC) = translate_Pattern(_pat)
    implicit val notCon = notC
    val specType = _tpSpec map (tpSpec => translate_Type(tpSpec._types))
    val defn = _def.map(translate_Definiens(_))

    val ret = if (specType.isDefined) {specType} else { defn.map(d => TranslationController.inferType(d.someCase)) }
    if (defn.isEmpty) {
      if(_redefine.occurs) {
        //Redefinitions are only possible for Infix or Circumfix Functors
        val orgExtPatAttrs = _pat.patDef
        val origDecl = TranslatorUtils.computeGlobalOrgPatternName(orgExtPatAttrs)
        val oldDef = TranslationController.controller.get(origDecl) match {case decl: symbols.Constant => decl}
        val redef = TranslationController.makeConstant(gn.name, Some(ret getOrElse oldDef.tp.get), oldDef.df)
        List(redef)
      } else {
        ???
      }
    }
    val (argNum, argTps) = (args.length, args.map(_.tp.get))
    val funcDef = defn.get match {
      case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) => directPartialFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, defRes)
      case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) => indirectPartialFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes, defRes)
      case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) => directCompleteFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes)
      case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) => indirectCompleteFunctorDefinition(name, argNum, argTps, ret.get, defn.get.caseNum, cases, caseRes)
    }
    List(funcDef)
  }
  def translate_Mode_Definition(mode_Definition: Mode_Definition)(implicit args: Context = Context.empty) = {
    val (name, declarationPath, notC) = translate_Pattern(mode_Definition._pat)
    implicit val notCon = notC
    mode_Definition._expMode match {
      case Expandable_Mode(_tp) =>
        val tp = translate_Type(_tp)
        List(TranslationController.makeConstant(declarationPath.name, Some(Mizar.tp),Some(tp)))
      case stm @ Standard_Mode(_tpSpec, _def) =>
        val name = declarationPath.name
        val (argNum, argTps) = (args.length, args.map(_.tp.get))
        val defnO = _def map(translate_Definiens(_))

        if (defnO.isEmpty) {
          assert(_tpSpec.isDefined)
          List(TranslationController.makeConstant(declarationPath.name, translate_Type(_tpSpec.get._types)))
        }
        val defn = defnO.get
        val modeDef = defn match {
          case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) => directPartialModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, defRes)
          case IndirectPartialCaseByCaseDefinien(cases, caseRes, defRes) => indirectPartialModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes, defRes)
          case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) => directCompleteModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes)
          case IndirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) => indirectCompleteModeDefinition(name, argNum, argTps, defn.caseNum, cases, caseRes)
        }
        List(modeDef)
    }
  }
  def translate_Private_Functor_Definition(private_Functor_Definition: Private_Functor_Definition)(implicit args: Context= Context.empty) = { ??? }
  def translate_Private_Predicate_Definition(private_Predicate_Definition: Private_Predicate_Definition)(implicit args: Context= Context.empty) = { ??? }
  def translate_Predicate_Definition(predicate_Definition: Predicate_Definition)(implicit args: Context= Context.empty) = predicate_Definition match {
    case prd@Predicate_Definition(a, _redef, _predPat, _def) =>
      val (name, gn, notC) = translate_Pattern(_predPat)
      implicit val notCon = notC
      val defn = _def.map(translate_Definiens(_))
      if (defn.isEmpty) {
        if (_redef.occurs) {
          val origDecl = TranslatorUtils.MMLIdtoGlobalName(_predPat.globalPatternName())
          val oldDef = TranslationController.controller.get(origDecl) match {
            case decl: symbols.Constant => decl
          }
          val redef = TranslationController.makeConstant(name, oldDef.tp, oldDef.df)
          List(redef)
        } else {
          ???
        }
      }
      val (argNum, argTps) = (args.length, args.map(_.tp.get))
      val predDef = defn.get match {
        case DirectPartialCaseByCaseDefinien(cases, caseRes, defRes) => directPartialPredicateDef(name, argNum, argTps, defn.get.caseNum, cases, caseRes, defRes)
        case DirectCompleteCaseByCaseDefinien(cases, caseRes, completenessProof) => directCompletePredicateDef(name, argNum, argTps, defn.get.caseNum, cases, caseRes)
        case _ => throw DeclarationTranslationError("Predicate definition can't be indirect. ", prd)
      }
      List(predDef)
  }
}

object clusterTranslator {
  def translate_Cluster(cl:Cluster, cor_conds: List[JustifiedCorrectnessConditions] = Nil)(implicit args: Context = Context.empty): List[info.kwarc.mmt.api.symbols.Declaration] = {
    val tr = namedDefArgsTranslator()
    //TODO: Also translate the proofs of the correctness conditions
    val resDecls = cl._registrs map {
      case Conditional_Registration(pos, _attrs, _at, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_attrs)
        val ats = attributeTranslator.translateAttributes(_at)
        val name = LocalName("existReg:"+pos.position)
        conditionalRegistration(name, args map(_.tp.get), tp, adjs, ats)
      case Existential_Registration(pos, _adjClust, _tp) =>
        val tp = translate_Type(_tp)
        val adjs = attributeTranslator.translateAttributes(_adjClust)
        val name = LocalName("existReg:"+pos.position)
        existentialRegistration(name, args map(_.tp.get), tp, adjs)
      case Functorial_Registration(pos, _aggrTerm, _adjCl, _tp) =>
        val tm = translate_Term(_aggrTerm)
        val adjs = attributeTranslator.translateAttributes(_adjCl)
        val isQualified = _tp.isDefined
        val tp = _tp map translate_Type getOrElse({
          TranslationController.inferType(tm)})
        val name = LocalName("funcReg:"+pos.position)
        if (isQualified) {
          qualifiedFunctorRegistration(name, args map(_.tp.get), tp, tm, adjs)
        } else {
          unqualifiedFunctorRegistration(name, args map(_.tp.get), tp, tm, adjs)
        }
      case Property_Registration(_props, _just) =>
        val Properties(Some(sort), None, Nil, Some(_tp)) = _props
        sort match {
          case "sethood" =>
            val tp = translate_Type(_tp)
            val claim = Apply(Mizar.constant("sethood"), tp)
            val just = justificationTranslator.translate_Justification(_just, claim)
            val name = LocalName("sethood_of_"+tp.toStr(true))
            TranslationController.makeConstant(name, Some(Univ(1)), just)
        }
    }
    resDecls map tr
  }
}
object patternTranslator {
  def parseFormatDesc(formatdes: String): (Int, Int, Int) = {
    val interior = formatdes.substring(formatdes.indexOf('[')+1, formatdes.lastIndexOf(']'))
    val List(pre, int, suf) = interior.split(Array('(', ')')).toList map(_.toInt)
    (pre, int, suf)
  }
  def translate_Pattern(pat:Patterns) : (LocalName, GlobalName, NotationContainer) = {
    val gn = TranslatorUtils.MMLIdtoGlobalName(pat.globalPatternName())
    val name = gn.name

    val (infixNr, circumfixNr, suffixNr) = parseFormatDesc(pat.patternAttrs.formatdes)
    val notC = pat match {
      case InfixFunctor_Pattern(rightargsbracketedO, orgExtPatAttr, _loci, _locis) =>
        val rightArgsBracketed = rightargsbracketedO.getOrElse(false)
        val infixedArgMarkers = (1 to infixNr).map(i=>SimpArg(i)).toList
        val functorMarker = Delim(orgExtPatAttr.extPatAttr.patAttr.spelling)
        val suffixedArgsMarkers = (infixNr+1 to infixNr+suffixNr).map(i=>SimpArg(i)).toList
        val suffMarkers: List[Marker] = if (rightArgsBracketed) {Delim("(")::suffixedArgsMarkers.+:(Delim(")"))} else {suffixedArgsMarkers}
        val fixity = Mixfix(infixedArgMarkers++(functorMarker::suffMarkers))
        NotationContainer(TextNotation(fixity, Precedence.integer(0), None, false))
      case CircumfixFunctor_Pattern(orgExtPatAttr, _right_Circumflex_Symbol, _loci, _locis) =>
        val leftDelim = Delim(orgExtPatAttr.extPatAttr.patAttr.spelling)
        val rightDelim = Delim(_right_Circumflex_Symbol.spelling)
        val circumfixedArgsMarkers = (1 to circumfixNr).map(i=>SimpArg(i)).toList
        val fixity = Mixfix(rightDelim::(circumfixedArgsMarkers.+:(rightDelim)))
        NotationContainer(TextNotation(fixity, Precedence.integer(0), None, false))
      case pat: Patterns =>
        val functorMarker = Delim(pat.patternAttrs.spelling)
        val suffixedArgsMarkers = (infixNr+1 to infixNr+suffixNr).map(i=>SimpArg(i)).toList
        val fixity = Mixfix(functorMarker::suffixedArgsMarkers)
        NotationContainer(TextNotation(fixity, Precedence.integer(0), None, false))
    }
    (name, gn, notC)
  }
  def translate_Referencing_Pattern(pat: Patterns): (LocalName, List[objects.Term], objects.Term, NotationContainer) = {
    val (name, gn, notC) = translate_Pattern(pat)
    val referencedDeclSE = TranslationController.controller.getO(gn).getOrElse(
      throw new ObjectLevelTranslationError("Trying to lookup declaration referenced by pattern: "+pat+", but no such Declaration found. ", pat))
    val (mainDecl, addArgsTps): (objects.Term, List[objects.Term]) = referencedDeclSE match {
      case c: symbols.Constant =>
        val FunType(addArgsTps, prop) = c.tp.getOrElse(
          throw new ObjectLevelTranslationError("Trying to retrieve type of looked up declaration "+c.name+" referenced by pattern: "+pat+", but declaration has not type defined. ", pat))
        (c.toTerm, addArgsTps.map(_._2))
      case dd: symbols.DerivedDeclaration =>
        val addArgTps = dd match {
        case directPartialAttributeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
        case indirectPartialAttributeDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
        case directCompleteAttributeDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
        case indirectCompleteAttributeDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
        case directPartialFunctorDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
        case indirectPartialFunctorDefinition(_, _, addArgTps, _, _, _, _, _) => addArgTps
        case directCompleteFunctorDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
        case indirectCompleteFunctorDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
        case directPartialModeDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
        case indirectPartialModeDefinition(_, _, addArgTps, _, _, _, _) => addArgTps
        case directCompleteModeDefinition(_, _, addArgTps, _, _, _) => addArgTps
        case indirectCompleteModeDefinition(_, _, addArgTps, _, _, _) => addArgTps
        case directPartialPredicateDef(_, _, addArgTps, _, _, _, _) => addArgTps
        case directCompletePredicateDef(_, _, addArgTps, _, _, _) => addArgTps
      }
        (dd.toTerm, addArgTps)
    }
    (name, addArgsTps, mainDecl, notC)
  }
}

object blockTranslator {
  def collectSubitems[mainSort <: Subitem](block: Block) : (Context, List[objects.Term], List[(mainSort, List[JustifiedCorrectnessConditions], List[Property])]) = {
    val items = block._items
    implicit var args: Context = objects.Context.empty
    implicit var assumptions: List[objects.Term] = Nil
    var resDecls: List[(mainSort, List[JustifiedCorrectnessConditions], List[Property])] = Nil

    items.zipWithIndex foreach { case (it: Item, ind: Int) =>
      it._subitem match {
        case loci_Declaration: Loci_Declaration =>
          args = args ++ subitemTranslator.translate_Loci_Declaration(loci_Declaration)
        case ass: Assumption => assumptions +:= translate_Claim(ass)
        case defn : mainSort if (defn.isInstanceOf[BlockSubitem]) =>
          implicit var corr_conds: List[JustifiedCorrectnessConditions] = Nil
          implicit var props: List[Property] = Nil
          items.drop(ind).map(_._subitem match {
            case corCond: Correctness_Condition => (true, Some(corCond))
            case cor: Correctness => (true, Some(cor))
            case prop: Property => (true, Some(prop))
            case _ => (false, None)
          }) filter(_._1) foreach(_._2.get match {
            case corCond @ Correctness_Condition(_cor, _justO) =>
              corr_conds +:= JustifiedCorrectnessConditions(List(_cor), _justO)
            case Correctness(_cor, _just) =>
              corr_conds +:= JustifiedCorrectnessConditions(_cor._cond, Some(_just))
            case prop : Property => props +:= prop
          })
          resDecls +:= (defn, corr_conds, props)
        case corr: Correctness =>
        case correctness_Condition: Correctness_Condition =>
        case prop:Property =>
        case defIt => throw DeclarationTranslationError("Unexpected item of type " + defIt.kind+" found, in "+block.kind, defIt)
      }
    }
    (args, assumptions, resDecls)
  }
  def translate_Definitional_Block(block:Block):List[symbols.Declaration] = {
    val (arguments, ass, definitionItems) = collectSubitems[Definition](block)
    implicit var args: Context = arguments
    implicit var assumptions: List[objects.Term] = ass
    definitionItems flatMap {
      case (defn: Definition, corConds: List[JustifiedCorrectnessConditions], props: List[Property]) =>
        implicit val corrConds = corConds
        implicit val properties = props
        translate_Definition(defn)
    }
  }
  def translate_Registration_Block(block: Block) : List[symbols.Declaration] = {
    val (arguments, ass, definitionItems) = collectSubitems[Cluster](block)
    implicit var args: Context = arguments
    implicit var assumptions: List[objects.Term] = ass
    definitionItems flatMap {
      case (cl: Cluster, corConds: List[JustifiedCorrectnessConditions], props: List[Property]) =>
        implicit val corrConds = corConds
        implicit val properties = props
        translate_Cluster(cl)
    }
  }
  def translate_Notation_Block(block: Block): List[symbols.Declaration] = {
    val (arguments, ass, definitionItems) = collectSubitems[Nyms](block)
    implicit var args: Context = arguments
    implicit var assumptions: List[objects.Term] = ass
    definitionItems flatMap {
      case (nym: Nyms, corConds: List[JustifiedCorrectnessConditions], props: List[Property]) =>
        implicit val corrConds = corConds
        implicit val properties = props
        translate_Nym(nym)
    }
  }
}

sealed abstract class CaseByCaseDefinien {
  /**
   * Used to do inference on, mainly
   * @return
   */
  def someCase: objects.Term
  def cases: List[objects.Term]
  def caseRes: List[objects.Term]
  def caseNum = cases.length
}
case class DirectPartialCaseByCaseDefinien(cases: List[objects.Term], caseRes: List[objects.Term], defRes: objects.Term) extends CaseByCaseDefinien {
  override def someCase: objects.Term = defRes
}
object DirectPartialCaseByCaseDefinien {
  def apply(tm: objects.Term): DirectPartialCaseByCaseDefinien = DirectPartialCaseByCaseDefinien(Nil, Nil, tm)
}
case class IndirectPartialCaseByCaseDefinien(cases: List[objects.Term], caseRes: List[objects.Term], defRes: objects.Term) extends CaseByCaseDefinien {
  override def someCase: objects.Term = defRes
}
object IndirectPartialCaseByCaseDefinien {
  def apply(tm: objects.Term): IndirectPartialCaseByCaseDefinien = IndirectPartialCaseByCaseDefinien(Nil, Nil, Lam("it", any, tm))
}
case class DirectCompleteCaseByCaseDefinien(cases: List[objects.Term], caseRes: List[objects.Term], completenessProof: Option[objects.Term] = None) extends CaseByCaseDefinien {
  override def someCase: objects.Term = caseRes.head
}
case class IndirectCompleteCaseByCaseDefinien(cases: List[objects.Term], caseRes: List[objects.Term], completenessProof: Option[objects.Term] = None) extends CaseByCaseDefinien {
  override def someCase: objects.Term = caseRes.head
}

case class JustifiedCorrectnessConditions(correctness_Condition: List[CorrectnessConditions], just: Option[Justification])

object definiensTranslator {
  def translate_Definiens(defs:Definiens, just: Option[Justification] = None)(implicit args: Context): CaseByCaseDefinien = {
    translate_CaseBasedExpr(defs._expr)
  }
  def translate_CaseBasedExpr(defn:CaseBasedExpr)(implicit args: Context): CaseByCaseDefinien = {
    defn.check()
    if (defn.isSingleCase()) {
      val defRes = translate_Expression(defn.singleCasedExpr._expr.get)
      if (defRes.freeVars.contains(LocalName("it"))) {
        println("Indirect single case definien: "+defRes.toStr(true))
        IndirectPartialCaseByCaseDefinien(defRes)
      } else {
        DirectPartialCaseByCaseDefinien(defRes)
      }
    } else {
      translate_Cased_Expression(defn.partialCasedExpr)
    }
  }
  def translate_Cased_Expression(partDef:PartialDef)(implicit args: Context): CaseByCaseDefinien = {
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

  object assumptionTranslator {
    def translateAssumption(ass:Assumption) = translateAssumptions(ass._ass)
      def translateAssumptions(ass:Assumptions) = { ??? }
  }
}

object registrationTranslator {
  def translateRegistration(reg:Registrations) = { ??? }
}