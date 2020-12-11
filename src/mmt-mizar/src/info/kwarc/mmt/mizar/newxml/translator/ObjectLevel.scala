package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.objects
import info.kwarc.mmt.api.objects.OMV
import info.kwarc.mmt.lf.{Apply, ApplyGeneral}
import info.kwarc.mmt.mizar.newxml.mmtwrapper
import info.kwarc.mmt.mizar.newxml.mmtwrapper.{PatternUtils, StructureInstance}
import info.kwarc.mmt.mizar.newxml.syntax.{Aggregate_Term, Assumption, Biconditional_Formula, Circumfix_Term, Claim, Clustered_Type, Conditional_Formula, Conditions, Conjunctive_Formula, Contradiction, Definition, Diffuse_Statement, Disjunctive_Formula, Existential_Quantifier_Formula, Explicitly_Qualified_Segment, Expression, FlexaryConjunctive_Formula, FlexaryDisjunctive_Formula, Forgetful_Functor_Term, Formula, Fraenkel_Term, Global_Choice_Term, Infix_Term, Internal_Selector_Term, Iterative_Equality, Multi_Attributive_Formula, Multi_Relation_Formula, Negated_Formula, Numeral_Term, Placeholder_Term, Private_Functor_Term, Private_Predicate_Formula, Proposition, Qualification_Term, Qualifying_Formula, Redefine, Relation_Formula, ReservedDscr_Type, Selector_Term, Simple_Fraenkel_Term, Simple_Term, Standard_Type, Struct_Type, Term, Thesis, Type, Universal_Quantifier_Formula, Variable, it_Term}
import info.kwarc.mmt.mizar.newxml.translator.Utils
import org.omdoc.latin.foundations.mizar.MizarPatterns

object expressionTranslator {
  def translate_Expression(expr:Expression): objects.Term = expr match {
    case tm: Term => termTranslator.translate_Term(tm)
    case tp: Type => typeTranslator.translate_Type(tp)
    case formula: Formula => formulaTranslator.translate_Formula(formula)
  }
}

object termTranslator {
  def translate_Term(tm:Term) : objects.Term = tm match {
    case Simple_Term(varAttr, srt) => ???
    case Aggregate_Term(tpAttrs, _args) =>
      val gn = Utils.MMLIdtoGlobalName(tpAttrs.globalName())
      val aggrDecl = PatternUtils.referenceExtDecl(gn,"aggr")
      val args = Utils.translateArguments(_args)
      ApplyGeneral(aggrDecl, args)
    case Selector_Term(tpAttrs, _args) =>
      val gn = Utils.MMLIdtoGlobalName(tpAttrs.globalName())
      val struct = translate_Term(_args)
      val structTm = TranslationController.simplifyTerm(struct)
      val ApplyGeneral(structAggrDecl, aggrArgs) = structTm
      val args::argsTyped::_ = aggrArgs
      ApplyGeneral(objects.OMS(gn), List(args, argsTyped, struct))
    case Circumfix_Term(tpAttrs, _symbol, _args) =>
      assert(tpAttrs.sort == "Functor-Term")
      val gn = Utils.MMLIdtoGlobalName(tpAttrs.globalName())
      val args = Utils.translateArguments(_args)
      ApplyGeneral(objects.OMS(gn), args)
    case Numeral_Term(redObjAttr, nr, varnr) => mmtwrapper.Mizar.num(nr)
    case it_Term(redObjSubAttrs) => throw new java.lang.Error("Unresolved implicit reference in term.")
    case Internal_Selector_Term(redObjAttr, varnr) => ???
    case Infix_Term(tpAttrs, infixedArgs) =>
      assert(tpAttrs.sort == "Functor-Term")
      val gn = Utils.MMLIdtoGlobalName(tpAttrs.globalName())
      val args = Utils.translateArguments(infixedArgs._args)
      ApplyGeneral(objects.OMS(gn), args)
    case Global_Choice_Term(redObjSubAttrs, _tp) => ???
    case Placeholder_Term(redObjAttr, varnr) => throw new java.lang.Error("Unresolved argument reference in term.")
    case Private_Functor_Term(redObjAttr, serialnr, _args) => ???
    case Fraenkel_Term(redObjSubAttrs, _varSegms, _tm, _form) =>
      val tp : Type = Utils.firstVariableUniverse(_varSegms)
      val universe = translate_Term(Utils.getUniverse(tp))
      val args : List[objects.OMV] = _varSegms._vars map {
        case explSegm: Explicitly_Qualified_Segment =>
          assert(Utils.conforms(explSegm._tp, tp))
          explSegm._vars match { case List(v) => variableTranslator.translate_Variable(v) }
        case segm => segm._vars() match { case List(v) => variableTranslator.translate_Variable(v) }
      }
      val cond = formulaTranslator.translate_Formula(_form)
      val expr = translate_Term(_tm)
      mmtwrapper.Mizar.fraenkelTerm(expr, args, universe, cond)
    case Simple_Fraenkel_Term(redObjSubAttrs, _varSegms, _tm) =>
      val tp : Type = Utils.firstVariableUniverse(_varSegms)
      val universe = translate_Term(Utils.getUniverse(tp))
      val args : List[objects.OMV] = _varSegms._vars map {
        case explSegm: Explicitly_Qualified_Segment =>
          assert(Utils.conforms(explSegm._tp, tp))
          explSegm._vars match { case List(v) => variableTranslator.translate_Variable(v) }
        case segm => segm._vars() match { case List(v) => variableTranslator.translate_Variable(v) }
      }
      val expr = translate_Term(_tm)
      mmtwrapper.Mizar.simpleFraenkelTerm(expr, args, universe)
    case Qualification_Term(redObjSubAttrs, _tm, _tp) => ???
    case Forgetful_Functor_Term(constrExtObjAttrs, _tm) =>
      val gn = Utils.MMLIdtoGlobalName(constrExtObjAttrs.globalName())
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
  def translate_Type(tp:Type) : objects.Term = tp match {
    case ReservedDscr_Type(idnr, nr, srt, _subs, _tp) => ???
    case Clustered_Type(redObjSubAttrs, _adjClust, _tp) => ???
    case Standard_Type(tpAttrs, noocc, origNr, _args) =>
      // Seems to roughly correspond to an OMS referencing a type, potentially applied to some arguments
      // TODO: Check this the correct semantics and take care of the noocc attribute
      val gn = Utils.MMLIdtoGlobalName(tpAttrs.globalName())
      val tp : objects.Term = objects.OMS(gn)
      val args = Utils.translateArguments(_args)
      ApplyGeneral(tp,args)
    case Struct_Type(tpAttrs, _args) => ???
      val formatNr = tpAttrs.formatNr
      val patternNr = tpAttrs.patternNr
      val position = tpAttrs.posNr.pos.parsePosition()
      val nr = tpAttrs.posNr.nr
      val constrnr = tpAttrs.constrNr
      ???
  }
}

object formulaTranslator {
  def translate_Formula(formula:Formula) : objects.Term = formula match {
    case Existential_Quantifier_Formula(redObjSubAttrs, _vars, _expression) =>
      val tp : Type = Utils.firstVariableUniverse(_vars)
      val univ = termTranslator.translate_Term(Utils.getUniverse(tp))
      val vars = Utils.translateVariables(_vars)
      translate_Existential_Quantifier_Formula(vars,univ,_expression)
    case Relation_Formula(objectAttrs, antonymic, infixedArgs) =>
      if (antonymic.isDefined && antonymic.get) {
        translate_Formula(Utils.negatedFormula(Relation_Formula(objectAttrs, None, infixedArgs)))
      } else {
        val rel = Utils.translateObjRef(objectAttrs)
        val args = Utils.translateArguments(infixedArgs._args)
        ApplyGeneral(rel, args)
      }
    case Universal_Quantifier_Formula(redObjSubAttrs, _vars, _restrict, _expression) =>
      val tp : Type = Utils.firstVariableUniverse(_vars)
      val univ = termTranslator.translate_Term(Utils.getUniverse(tp))
      val vars = Utils.translateVariables(_vars)
      translate_Universal_Quantifier_Formula(vars,univ,_expression)
    case Multi_Attributive_Formula(redObjSubAttrs, _tm, _clusters) => ???
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

object variableTranslator {
  def translate_Variable(variable:Variable) : objects.OMV = { ??? }
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
