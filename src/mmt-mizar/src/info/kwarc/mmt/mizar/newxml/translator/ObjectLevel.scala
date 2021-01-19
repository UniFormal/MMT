package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.{Context, OMV, VarDecl}
import info.kwarc.mmt.lf._
import info.kwarc.mmt.lf.structuralfeatures.{RecordUtil, StructuralFeatureUtils}
import info.kwarc.mmt.mizar.newxml.mmtwrapper
import info.kwarc.mmt.mizar.newxml.mmtwrapper.{MMTUtils, Mizar, PatternUtils, StructureInstance}
import info.kwarc.mmt.mizar.newxml.syntax._
import info.kwarc.mmt.mizar.newxml.translator.TranslatorUtils._
import info.kwarc.mmt.mizar.newxml.translator.contextTranslator.translate_Context
import info.kwarc.mmt.mizar.newxml.translator.formulaTranslator.{translate_Existential_Quantifier_Formula, translate_Formula}
import org.omdoc.latin.foundations.mizar.MizarPatterns

object expressionTranslator {
  def translate_Expression(expr:Expression)(implicit args: Context = Context.empty): objects.Term = expr match {
    case tm: Term => termTranslator.translate_Term(tm)
    case tp: Type => typeTranslator.translate_Type(tp)
    case formula: Formula => translate_Formula(formula)
  }
}

object termTranslator {
  def translate_Term(tm:Term)(implicit args: Context, selectors: List[(Int, VarDecl)] = Nil) : objects.Term = tm match {
    case Simple_Term(locVarAttr) =>
      val tr = TranslatorUtils.namedDefArgsSubstition()
      val refTm = LocalName(locVarAttr.toIdentifier())
      tr(refTm).getOrElse(OMV(refTm))
    case Aggregate_Term(tpAttrs, _args) =>
      val gn = TranslatorUtils.computeGlobalPatternName(tpAttrs)
      val aggrDecl = PatternUtils.referenceExtDecl(gn,RecordUtil.makeName)
      val args = TranslatorUtils.translateArguments(_args)
      ApplyGeneral(aggrDecl, args)
    case Selector_Term(tpAttrs, _args) =>
      val strGn = MMLIdtoGlobalName(tpAttrs.globalPatternName().copy(kind = "L"))
      val sel = PatternUtils.referenceExtDecl(strGn, tpAttrs.spelling)
      val arguments = _args map(translate_Term)
      ApplyGeneral(sel, arguments)
    case Circumfix_Term(tpAttrs, _symbol, _args) =>
      assert(tpAttrs.sort == "Functor-Term")
      val gn = TranslatorUtils.computeGlobalPatternName(tpAttrs)
      val arguments = TranslatorUtils.translateArguments(_args)
      ApplyGeneral(objects.OMS(gn), arguments)
    case Numeral_Term(redObjAttr, nr, varnr) => mmtwrapper.Mizar.num(nr)
    case itt @ it_Term(redObjSubAttrs) => OMV("it")
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
      val arguments : List[objects.OMV] = _varSegms._vars flatMap(translate_Context(_)) map(_.toTerm)
      val cond = translate_Formula(_form)
      val expr = translate_Term(_tm)
      mmtwrapper.Mizar.fraenkelTerm(expr, arguments, universe, cond)
    case Simple_Fraenkel_Term(redObjSubAttrs, _varSegms, _tm) =>
      val tp : Type = TranslatorUtils.firstVariableUniverse(_varSegms)
      val universe = translate_Term(TranslatorUtils.getUniverse(tp))
      val arguments : List[objects.OMV] = _varSegms._vars flatMap(translate_Context(_)) map(_.toTerm)
      val expr = translate_Term(_tm)
      mmtwrapper.Mizar.simpleFraenkelTerm(expr, arguments, universe)
    case Qualification_Term(redObjSubAttrs, _tm, _tp) => ???
    case Forgetful_Functor_Term(constrExtObjAttrs, _tm) =>
      val gn = TranslatorUtils.computeGlobalPatternName(constrExtObjAttrs)
      val substr = objects.OMS(gn)
      val struct = translate_Term(_tm)
      val structTm = TranslationController.simplifyTerm(struct)
      val ApplyGeneral(objects.OMS(strAggrPath), aggrArgs) = structTm
      val strPath = strAggrPath.module ? strAggrPath.name.steps.init
      val restr = PatternUtils.referenceExtDecl(strPath,PatternUtils.structureDefRestrName(gn.name.toString)(strPath).toString)
      val arguments::argsTyped::_ = aggrArgs
      ApplyGeneral(restr, List(arguments, argsTyped, struct))
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
      // TODO: Check this is the correct semantics and take care of the noocc attribute
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
  def translate_Formula(formula:Formula)(implicit args: Context) : objects.Term = formula match {
    case Existential_Quantifier_Formula(redObjSubAttrs, _vars, _expression) =>
      val tp : Type = TranslatorUtils.firstVariableUniverse(_vars)
      val univ = termTranslator.translate_Term(TranslatorUtils.getUniverse(tp))
      val vars = TranslatorUtils.translateVariables(_vars)
      val expr = claimTranslator.translate_Claim(_expression)
      translate_Existential_Quantifier_Formula(vars, univ, expr)
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
      val univ = typeTranslator.translate_Type(tp)
      val vars = TranslatorUtils.translateVariables(_vars)
      translate_Universal_Quantifier_Formula(vars,univ,_expression)
    case Multi_Attributive_Formula(redObjSubAttrs, _tm, _cluster) =>
      val tm = termTranslator.translate_Term(_tm)
      val attrs = _cluster._attrs map attributeTranslator.translate_Attribute
          Mizar.and(attrs.map(at => Apply(at, tm)))
    case Conditional_Formula(redObjSubAttrs, _frstFormula, _sndFormula) =>
      val assumption = claimTranslator.translate_Claim(_frstFormula)
      val conclusion = claimTranslator.translate_Claim(_sndFormula)
      mmtwrapper.Mizar.implies(assumption, conclusion)
    case Conjunctive_Formula(redObjSubAttrs, _frstConjunct, _sndConjunct) =>
      val frstConjunct = claimTranslator.translate_Claim(_frstConjunct)
      val sndConjunct = claimTranslator.translate_Claim(_sndConjunct)
      mmtwrapper.Mizar.binaryAnd(frstConjunct, sndConjunct)
    case Biconditional_Formula(redObjSubAttrs, _frstFormula, _sndFormula) =>
      val frstForm = claimTranslator.translate_Claim(_frstFormula)
      val sndForm = claimTranslator.translate_Claim(_sndFormula)
      mmtwrapper.Mizar.iff(frstForm, sndForm)
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
  def translate_Existential_Quantifier_Formula(vars:List[OMV], univ:objects.Term, expression:objects.Term)(implicit args: Context=Context.empty): objects.Term = vars match {
    case Nil => expression
    case v::vs =>
      val expr = translate_Existential_Quantifier_Formula(vs, univ, expression)
      mmtwrapper.Mizar.exists(v,univ,expr)
  }
  def translate_Universal_Quantifier_Formula(vars:List[OMV], univ:objects.Term, expression:Claim)(implicit args: Context=Context.empty): objects.Term = vars match {
    case Nil => claimTranslator.translate_Claim(expression)
    case v::vs =>
      val expr = translate_Universal_Quantifier_Formula(vs, univ, expression)
      mmtwrapper.Mizar.forall(v,univ,expr)
  }
}

object contextTranslator {
  def translate_Variable(variable:Variable) : objects.OMV = {
    OMV(variable.varAttr.toIdentifier())
  }
  private def translateSingleTypedVariable(_var : Variable, _tp: Type)(implicit selectors: List[(Int, objects.VarDecl)] = Nil) = {
    val variable = translate_Variable(_var)
    val tp = typeTranslator.translate_Type(_tp)
    variable % tp
  }
  def translate_Context(varSegm: VariableSegments)(implicit selectors: List[(Int, objects.VarDecl)] = Nil) : Context= varSegm match {
    case Free_Variable_Segment(pos, _var, _tp) => translateSingleTypedVariable(_var, _tp)
    case Implicitly_Qualified_Segment(pos, _var, _tp) =>translateSingleTypedVariable(_var, _tp)
    case Explicitly_Qualified_Segment(pos, _variables, _tp) => _variables._vars.map(v => translateSingleTypedVariable(v,_tp))
  }
	def translate_Locus(loc:Locus) : objects.OMV = {
		OMV(loc.varAttr.toIdentifier())
	}
}

object claimTranslator {
  def translate_Claim(claim:Claim)(implicit args: Context) : objects.Term = claim match {
    case Assumption(_ass) => _ass match {
      case Single_Assumption(pos, _prop) => translate_Claim(_prop)
      case Collective_Assumption(pos, _cond) => Mizar.and(_cond._props map translate_Claim)
      case Existential_Assumption(_qualSegm, _cond) =>
        implicit var arguments = args
        def qualifySegments(vs: List[(List[OMV], objects.Term)], claim: objects.Term): objects.Term = vs match {
          case con::cons =>
            translate_Existential_Quantifier_Formula(con._1, con._2, qualifySegments(cons, claim))(arguments)
          case Nil => claim
        }
        val vars = _qualSegm._children map(translate_Context) map(ctx => (ctx.variables.toList map(_.toTerm), ctx.variables.head.tp.get))
        val claim = Mizar.and(_cond._props.map(translate_Claim(_)(arguments)))
        qualifySegments(vars, claim)
    }
    case form: Formula => translate_Formula(form)
    case Proposition(pos, _label, _thesis) => translate_Claim(_thesis)
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
