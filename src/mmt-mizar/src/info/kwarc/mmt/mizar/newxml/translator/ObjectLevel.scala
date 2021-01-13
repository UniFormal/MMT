package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.{OMV, VarDecl}
import info.kwarc.mmt.lf._
import info.kwarc.mmt.lf.structuralfeatures.{RecordUtil, StructuralFeatureUtils}
import info.kwarc.mmt.mizar.newxml.mmtwrapper
import info.kwarc.mmt.mizar.newxml.mmtwrapper.{MMTUtils, Mizar, PatternUtils, StructureInstance}
import info.kwarc.mmt.mizar.newxml.syntax._
import info.kwarc.mmt.mizar.newxml.translator.TranslatorUtils
import org.omdoc.latin.foundations.mizar.MizarPatterns

object expressionTranslator {
  def translate_Expression(expr:Expression): objects.Term = expr match {
    case tm: Term => termTranslator.translate_Term(tm)
    case tp: Type => typeTranslator.translate_Type(tp)
    case formula: Formula => formulaTranslator.translate_Formula(formula)
  }
}

object termTranslator {
  def translate_Term(tm:Term)(implicit selectors: List[(Int, VarDecl)] = Nil) : objects.Term = tm match {
    case Simple_Term(locVarAttr) => OMV(locVarAttr.toIdentifier())
    case Aggregate_Term(tpAttrs, _args) =>
      val gn = TranslatorUtils.computeGlobalPatternName(tpAttrs)
      val aggrDecl = PatternUtils.referenceExtDecl(gn,RecordUtil.makeName)
      val args = TranslatorUtils.translateArguments(_args)
      ApplyGeneral(aggrDecl, args)
    case Selector_Term(tpAttrs, _args) =>
      val strGn = TranslatorUtils.computeStrGlobalName(tpAttrs)
      val sel = PatternUtils.referenceExtDecl(strGn, tpAttrs.spelling)
      val args = _args map(translate_Term)
      ApplyGeneral(sel, args)
    case Circumfix_Term(tpAttrs, _symbol, _args) =>
      assert(tpAttrs.sort == "Functor-Term")
      val gn = TranslatorUtils.computeGlobalPatternName(tpAttrs)
      val args = TranslatorUtils.translateArguments(_args)
      ApplyGeneral(objects.OMS(gn), args)
    case Numeral_Term(redObjAttr, nr, varnr) => mmtwrapper.Mizar.num(nr)
    case itt @ it_Term(redObjSubAttrs) =>
      OMV("it")
      //throw new ObjectLevelTranslationError("Unresolved implicit reference in term.", itt)
    case ist @ Internal_Selector_Term(redObjAttr, varnr) =>
      val nr = redObjAttr.posNr.nr
      val referencedSelector = utils.listmap(selectors, nr).getOrElse(
        throw new ObjectLevelTranslationError("The referenced selector with number "+nr+" is unknown, hence the internal selector term can't be translated. "+
          "\nThe only known selectors are: \n"+selectors.toString(), ist))
      referencedSelector.toTerm
    case Infix_Term(tpAttrs, infixedArgs) =>
      assert(tpAttrs.sort == "Functor-Term", "Expected Infix-Term to have sort Functor-Term, but instead found sort "+tpAttrs.sort)
      val gn = TranslatorUtils.computeGlobalPatternName(tpAttrs)
      val args = TranslatorUtils.translateArguments(infixedArgs._args)
      ApplyGeneral(objects.OMS(gn), args)
    case Global_Choice_Term(redObjSubAttrs, _tp) =>
      val tp = typeTranslator.translate_Type(_tp)
      Apply(Mizar.constant("choice"), tp)
    case Placeholder_Term(redObjAttr, varnr) => throw new java.lang.Error("Unresolved argument reference in term.")
    case Private_Functor_Term(redObjAttr, serialnr, _args) => ???
    case Fraenkel_Term(redObjSubAttrs, _varSegms, _tm, _form) =>
      val tp : Type = TranslatorUtils.firstVariableUniverse(_varSegms)
      val universe = translate_Term(TranslatorUtils.getUniverse(tp))
      val args : List[objects.OMV] = _varSegms._vars map {
        case explSegm: Explicitly_Qualified_Segment =>
          assert(TranslatorUtils.conforms(explSegm._tp, tp))
          explSegm._vars match { case List(v) => variableTranslator.translate_Variable(v) }
        case segm => segm._vars() match { case List(v) => variableTranslator.translate_Variable(v) }
      }
      val cond = formulaTranslator.translate_Formula(_form)
      val expr = translate_Term(_tm)
      mmtwrapper.Mizar.fraenkelTerm(expr, args, universe, cond)
    case Simple_Fraenkel_Term(redObjSubAttrs, _varSegms, _tm) =>
      val tp : Type = TranslatorUtils.firstVariableUniverse(_varSegms)
      val universe = translate_Term(TranslatorUtils.getUniverse(tp))
      val args : List[objects.OMV] = _varSegms._vars map {
        case explSegm: Explicitly_Qualified_Segment =>
          assert(TranslatorUtils.conforms(explSegm._tp, tp))
          explSegm._vars match { case List(v) => variableTranslator.translate_Variable(v) }
        case segm => segm._vars() match { case List(v) => variableTranslator.translate_Variable(v) }
      }
      val expr = translate_Term(_tm)
      mmtwrapper.Mizar.simpleFraenkelTerm(expr, args, universe)
    case Qualification_Term(redObjSubAttrs, _tm, _tp) => ???
    case Forgetful_Functor_Term(constrExtObjAttrs, _tm) =>
      val gn = TranslatorUtils.computeGlobalPatternName(constrExtObjAttrs)
      val substr = objects.OMS(gn)
      val struct = translate_Term(_tm)
      val structTm = TranslationController.simplifyTerm(struct)
      val ApplyGeneral(objects.OMS(strAggrPath), aggrArgs) = structTm
      val strPath = strAggrPath.module ? strAggrPath.name.steps.init
      val restr = PatternUtils.referenceExtDecl(strPath,PatternUtils.structureDefRestrName(gn.name.toString).toString)
      val args::argsTyped::_ = aggrArgs
      ApplyGeneral(restr, List(args, argsTyped, struct))
  }
}

object typeTranslator {
  def translate_Type_Specification(tp: Type_Specification) = translate_Type(tp._types)
  def translate_Type(tp:Type)(implicit selectors: List[(Int, VarDecl)] = Nil) : objects.Term = tp match {
    case ReservedDscr_Type(idnr, nr, srt, _subs, _tp) => translate_Type(_tp)
    case Clustered_Type(redObjSubAttrs, _adjClust, _tp) =>
      val tp = translate_Type(_tp)
      val adjectives = _adjClust._attrs map attributeTranslator.translate_Attribute
      Mizar.SimpleTypedAttrAppl(tp, adjectives)
    case Standard_Type(tpAttrs, noocc, origNr, _args) =>
      // Seems to roughly correspond to an OMS referencing a type, potentially applied to some arguments
      // TODO: Check this the correct semantics and take care of the noocc attribute
      val gn = TranslatorUtils.computeGlobalPatternName(tpAttrs)
      val tp : objects.Term = objects.OMS(gn)
      val args = TranslatorUtils.translateArguments(_args)
      ApplyGeneral(tp,args)
    case Struct_Type(tpAttrs, _args) =>
      val gn = TranslatorUtils.computeGlobalPatternName(tpAttrs)
      val typeDecl = PatternUtils.referenceExtDecl(gn,RecordUtil.recTypeName)
      val args = TranslatorUtils.translateArguments(_args)
      ApplyGeneral(typeDecl, args)
  }
}

object formulaTranslator {
  def translate_Formula(formula:Formula) : objects.Term = formula match {
    case Existential_Quantifier_Formula(redObjSubAttrs, _vars, _expression) =>
      val tp : Type = TranslatorUtils.firstVariableUniverse(_vars)
      val univ = termTranslator.translate_Term(TranslatorUtils.getUniverse(tp))
      val vars = TranslatorUtils.translateVariables(_vars)
      translate_Existential_Quantifier_Formula(vars,univ,_expression)
    case Relation_Formula(objectAttrs, antonymic, infixedArgs) =>
      if (antonymic.isDefined && antonymic.get) {
        translate_Formula(TranslatorUtils.negatedFormula(Relation_Formula(objectAttrs, None, infixedArgs)))
      } else {
        val rel = TranslatorUtils.translateObjRef(objectAttrs)
        val args = TranslatorUtils.translateArguments(infixedArgs._args)
        ApplyGeneral(rel, args)
      }
    case Universal_Quantifier_Formula(redObjSubAttrs, _vars, _restrict, _expression) =>
      val tp : Type = TranslatorUtils.firstVariableUniverse(_vars)
      val univ = termTranslator.translate_Term(TranslatorUtils.getUniverse(tp))
      val vars = TranslatorUtils.translateVariables(_vars)
      translate_Universal_Quantifier_Formula(vars,univ,_expression)
    case Multi_Attributive_Formula(redObjSubAttrs, _tm, _cluster) =>
      val tm = termTranslator.translate_Term(_tm)
      val attrs = _cluster._attrs map attributeTranslator.translate_Attribute
/*
      val attrTps = attrs map(TranslationController.inferType(_))
      val atrTps = attrTps map(TranslationController.simplifyTerm)
      val tp = atrTps.head
      atrTps.tail foreach {case tp2 =>
        if (!TranslationController.conforms(tp, tp2)) {
          throw new ObjectTranslationError("mother types of attributes don't match for multi-attributive formula. ", formula)
        }
      }
*/
        /*tp match {
        // we have dependent typed attributes
        case ApplyGeneral(Pi(nName, nTp, Pi(tpName, motherTp, atrTp)), List(n, tp)) if (nTp == uom.StandardInt) =>
          assert (atrTp == Arrow(Mizar.any, Mizar.prop))
          val mmtwrapper.MizSeq.OMI(nNum) = n
          Mizar.is(tm, Mizar.depTypedAttrAppl(nNum, tp, attrs))
        // simple typed attributes
        case Apply(Pi(tpName, motherTp, atrTp), tp) =>
          assert (atrTp == Arrow(Mizar.any, Mizar.prop))
          Mizar.is(tm, Mizar.SimpleTypedAttrAppl(tp, attrs))
        case Arrow(a, b) if (a == Mizar.any && b == Mizar.prop) =>*/
          Mizar.and(attrs.map(at => Apply(at, tm)))
      /*  case _ => throw new ObjectTranslationError("expected attribute to have type term -> prop, but found type: "+tp, formula)
      }*/
    case Conditional_Formula(redObjSubAttrs, _frstFormula, _sndFormula) =>
      val assumption = claimTranslator.translate_Claim(_frstFormula)
      val conclusion = claimTranslator.translate_Claim(_sndFormula)
      mmtwrapper.Mizar.implies(assumption, conclusion)
    case Conjunctive_Formula(redObjSubAttrs, _frstConjunct, _sndConjunct) =>
      val frstConjunct = claimTranslator.translate_Claim(_frstConjunct)
      val sndConjunct = claimTranslator.translate_Claim(_sndConjunct)
      mmtwrapper.Mizar.binaryAnd(frstConjunct, sndConjunct)
    case Biconditional_Formula(redObjSubAttrs, _frstFormula, _sndFormula) => ???
    case Disjunctive_Formula(redObjSubAttrs, _frstDisjunct, _sndDisjunct) =>
      val frstDisjunct = claimTranslator.translate_Claim(_frstDisjunct)
      val sndDisjunct = claimTranslator.translate_Claim(_sndDisjunct)
      mmtwrapper.Mizar.binaryOr(frstDisjunct, sndDisjunct)
    case Negated_Formula(redObjSubAttrs, _formula) =>
      Apply(mmtwrapper.Mizar.constant("not"),claimTranslator.translate_Claim(_formula))
    case Contradiction(redObjSubAttrs) => mmtwrapper.Mizar.constant("contradiction")
    case Qualifying_Formula(redObjSubAttrs, _tm, _tp) => ???
    case Private_Predicate_Formula(redObjAttr, serialNr, constrNr, _args) => ???
    case FlexaryDisjunctive_Formula(redObjSubAttrs, _formulae) =>
      val formulae = _formulae map claimTranslator.translate_Claim
      mmtwrapper.Mizar.or(formulae)
    case FlexaryConjunctive_Formula(redObjSubAttrs, _formulae) =>
      val formulae = _formulae map claimTranslator.translate_Claim
      mmtwrapper.Mizar.and(formulae)
    case Multi_Relation_Formula(redObjSubAttrs, _relForm, _rhsOfRFs) => ???
  }
  def translate_Existential_Quantifier_Formula(vars:List[OMV], univ:objects.Term, expression:Claim) = vars match {
    case Nil => claimTranslator.translate_Claim(expression)
    case v::vs =>
      val expr = claimTranslator.translate_Claim(expression)
      mmtwrapper.Mizar.exists(v,univ,expr)
  }
  def translate_Universal_Quantifier_Formula(vars:List[OMV], univ:objects.Term, expression:Claim) = vars match {
    case Nil => claimTranslator.translate_Claim(expression)
    case v::vs =>
      val expr = claimTranslator.translate_Claim(expression)
      mmtwrapper.Mizar.forall(v,univ,expr)
  }
}

object contextTranslator {
  private def translateSingleTypedVariable(_var : Variable, _tp: Type)(implicit selectors: List[(Int, objects.VarDecl)] = Nil) = {
    val variable = variableTranslator.translate_Variable(_var)
    val tp = typeTranslator.translate_Type(_tp)
    variable % tp
  }
  def translate_Context(varSegm: VariableSegments)(implicit selectors: List[(Int, objects.VarDecl)] = Nil) : objects.Context= varSegm match {
    case Free_Variable_Segment(pos, _var, _tp) => translateSingleTypedVariable(_var, _tp)
    case Implicitly_Qualified_Segment(pos, _var, _tp) =>translateSingleTypedVariable(_var, _tp)
    case Explicitly_Qualified_Segment(pos, _variables, _tp) => _variables._vars.map(v => translateSingleTypedVariable(v,_tp))
  }
	def translate_Locus(loc:Locus) : objects.OMV = {
		OMV(loc.varAttr.toIdentifier())
	}
}

object variableTranslator {
  def translate_Variable(variable:Variable) : objects.OMV = {
    OMV(variable.varAttr.toIdentifier())
  }
}

object claimTranslator {
  def translate_Claim(claim:Claim) : objects.Term = claim match {
    case Assumption(_ass) => ???
    case formula: Formula => formulaTranslator.translate_Formula(formula)
    case Proposition(pos, _label, _thesis) => ???
    case Thesis(redObjSubAttrs) => ???
    case Diffuse_Statement(spell, serialnr, labelnr, _label) => ???
    case Conditions(_props) => ???
    case Iterative_Equality(_label, _formula, _just, _iterSteps) => ???
  }
}

object attributeTranslator {
  def translateAttributes(adjective_Cluster: Adjective_Cluster) = adjective_Cluster._attrs map translate_Attribute
  def translate_Attribute(attr: Attribute): objects.Term = {
    val gn = TranslatorUtils.computeGlobalPatternName(attr.orgnlExtObjAttrs)
    val args = TranslatorUtils.translateArguments(attr._args)
    ApplyGeneral(objects.OMS(gn), args)
  }
}
