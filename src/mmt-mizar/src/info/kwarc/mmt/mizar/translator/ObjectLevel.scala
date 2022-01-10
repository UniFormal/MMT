package info.kwarc.mmt.mizar.translator

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.{Context, OMS, OMV, Sub, Term, VarDecl}
import info.kwarc.mmt.lf._
import info.kwarc.mmt.mizar._
import syntax._
import mmtwrapper._
import MizarPrimitiveConcepts._
import PatternUtils._
import info.kwarc.mmt.mizar.translator.patternTranslator.globalReference
import translator.attributeTranslator.translate_Attribute
import translator.claimTranslator.translate_Claim
import translator.typeTranslator.translate_Type
import translator.termTranslator.translate_Term
import translator.TranslatorUtils._
import translator.contextTranslator._
import translator.formulaTranslator._
import TranslationController._
import info.kwarc.mmt.mizar.syntax.Utils.{makeNewSimpleGlobalName, makeSimpleGlobalName, mapKind}

object expressionTranslator {
  def translate_Expression(expr:Expression)(implicit defContent: DefinitionContext): Term = expr match {
    case tm: MizTerm => termTranslator.translate_Term(tm)
    case tp: Type => translate_Type(tp)
    case formula: Formula => translate_Formula(formula)
  }
}

object termTranslator {
  def translate_Term(tm:syntax.MizTerm)(implicit defContext: DefinitionContext, selectors: List[(Int, VarDecl)] = Nil) : Term = tm match {
    case st: Simple_Term =>
      val refTm = st.toIdentifier
      val gn = makeNewSimpleGlobalName(refTm.toString)
      lazy val defaultValue = OMV(refTm) ^ implicitNamedDefArgsSubstition()
      if (locallyDeclared(gn)) {
        OMS(gn)
      } else defContext.lookupLocalDefinitionWithinSameProof(refTm) getOrElse defaultValue
    case at@Aggregate_Term(tpAttrs, _args) =>
      val gn = computeGlobalName(at)
      val aggrDecl = structureMakePath(gn)
      val args = translateArguments(_args)
      ApplyGeneral(OMS(aggrDecl), args)
    case st@Selector_Term(tpAttrs, _arg) =>
      val strGn = computeGlobalName(st)
      val sel = structureSelectorPath(LocalName(tpAttrs.spelling))(strGn)
      val argument = translate_Term (_arg)
      Apply(OMS(sel), argument)
    case ct@Circumfix_Term(tpAttrs, _symbol, _args) =>
      assert(tpAttrs.sort == "Functor-Term")
      val gn = globalReference(ct)
      val arguments = translateArguments(_args)
      ApplyGeneral(OMS(gn), arguments)
    case Numeral_Term(nr, _) => num(nr)
    case itt @ it_Term(_) => OMV("it")
    case ist @ Internal_Selector_Term(objAttr) =>
      val referencedSelector = utils.listmap(selectors, objAttr.nr).getOrElse(
        throw new ObjectLevelTranslationError("The referenced selector with number "+objAttr.nr+" is unknown, hence the internal selector term can't be translated. "+
          "\nThe only known selectors are: \n"+selectors.toString(), ist))
      referencedSelector.toTerm
    case it@Infix_Term(tpAttrs, infixedArgs) =>
      assert(tpAttrs.sort == "Functor-Term", "Expected Infix-Term to have sort Functor-Term, but instead found sort "+tpAttrs.sort)
      val gn = globalReference(it)
      val args = translateArguments(infixedArgs._args)
      ApplyGeneral(OMS(gn), args)
    case Global_Choice_Term(sort, _tp) =>
      val tp = translate_Type(_tp)
      Apply(constant("choice"), tp)
    case Placeholder_Term(redObjAttr) => OMV("placeholder_"+redObjAttr.nr)
    case Private_Functor_Term(redObjAttr, idnr, _args) =>
      val ln = LocalName(Utils.MizarVariableName(redObjAttr.spelling, redObjAttr.sort.stripSuffix("-Term"), idnr))
      val name = makeSimpleGlobalName(currentAid, ln.toString).name
      val f = if (ln.steps.tail.head.toString == mapKind("Private-Functor")) {
        OMS(TranslationController.currentTheoryPath ? ln)
      } else if (defContext.withinProof) {
        defContext.lookupLocalDefinitionWithinSameProof(ln) getOrElse OMV(ln) ^ implicitNamedDefArgsSubstition()
      } else {
        OMV(ln) ^ implicitNamedDefArgsSubstition()
      }
      ApplyGeneral(f, translateArguments(_args))
    case Fraenkel_Term(_, _varSegms, _tm, _form) =>
      val universe = translate_Type(_varSegms._children.head._tp())
      val arguments : List[OMV] = translateVariables(_varSegms)
      val cond = translate_Formula(_form)
      val expr = translate_Term(_tm)
      fraenkelTerm(expr, arguments, universe, cond)
    case Simple_Fraenkel_Term(_, _varSegms, _tm) =>
      val universe = translate_Type(_varSegms._children.head._tp())
      val arguments : List[OMV] = translateVariables(_varSegms)
      val expr = translate_Term(_tm)
      simpleFraenkelTerm(expr, arguments, universe)
    case Qualification_Term(_, _tm, _tp) =>
      //TODO: do the required checks
      translate_Term(_tm)
    case fft@Forgetful_Functor_Term(_, _tm) =>
      val forgetfulFunctorGn = computeGlobalName(fft)
      val structTm = translate_Term(_tm)
      Apply (OMS(forgetfulFunctorGn), structTm)
  }
}

object typeTranslator {
  def translate_Type_Specification(tp: Type_Specification)(implicit defContext: DefinitionContext) = translate_Type(tp._types)
  def translate_Type(tp:Type)(implicit defContext: DefinitionContext, selectors: List[(Int, VarDecl)] = Nil) : Term = tp match {
      case ReservedDscr_Type(_subs, _tp) => translate_Type(_tp)
      case Clustered_Type(_adjClust, _tp) =>
        val tp = translate_Type(_tp)
        val adjectives = _adjClust._attrs map translate_Attribute
        SimpleTypedAttrAppl(tp, adjectives)
      case st@Standard_Type(_, _, _args) =>
        // TODO: Check this is the correct semantics and take care of the noocc attribute
        val gn = globalReference(st)
        val tp : Term = OMS(gn)
        val args = translateArguments(_args)
        ApplyGeneral(tp,args)
      case st@Struct_Type(_, _args) =>
        val gn = computeGlobalName(st)
        val typeDecl = structureTypePath(gn)
        val args = translateArguments(_args)
        ApplyGeneral(OMS(typeDecl), args)
    }
   }

object formulaTranslator {
  def translate_Formula(formula:Formula)(implicit defContext: DefinitionContext) : Term = formula match {
    case Scope(_expr) => translate_Claim(_expr)
    case Existential_Quantifier_Formula(_vars, _restrict, _expression) =>
      val vars: Context = translate_Context_Segment(_vars)
      vars foreach defContext.addLocalBindingVar
      val expr = translate_Claim(_expression)
      val assumptions = translate_Restriction(_restrict)
      translate_Existential_Quantifier_Formula(vars, expr, assumptions)
    case rf@Relation_Formula(_, antonymic, infixedArgs) =>
      val rel = globalReference(rf)
      val args = translateArguments(infixedArgs._args)
      val form = ApplyGeneral(OMS(rel), args)
      if (antonymic getOrElse false) not(form) else form
    case Universal_Quantifier_Formula(_vars, _restrict, _expression) =>
      val vars = translate_Context_Segment(_vars)
      vars foreach defContext.addLocalBindingVar
      val expr = translate_Claim(_expression)
      val assumptions = translate_Restriction(_restrict)
      translate_Universal_Quantifier_Formula(vars, expr, assumptions)
    case Multi_Attributive_Formula(_tm, _cluster) =>
      val tm = termTranslator.translate_Term(_tm)
      val attrs = _cluster._attrs map translate_Attribute
          And(attrs.map(at => Apply(at, tm)))
    case Conditional_Formula(_frstFormula, _sndFormula) =>
      val assumption = translate_Claim(_frstFormula)
      val conclusion = translate_Claim(_sndFormula)
      implies(assumption, conclusion)
    case Conjunctive_Formula(_frstConjunct, _sndConjunct) =>
      val frstConjunct = translate_Claim(_frstConjunct)
      val sndConjunct = translate_Claim(_sndConjunct)
      binaryAnd(frstConjunct, sndConjunct)
    case Biconditional_Formula(_frstFormula, _sndFormula) =>
      val frstForm = translate_Claim(_frstFormula)
      val sndForm = translate_Claim(_sndFormula)
      iff(frstForm, sndForm)
    case Disjunctive_Formula(_frstDisjunct, _sndDisjunct) =>
      val frstDisjunct = translate_Claim(_frstDisjunct)
      val sndDisjunct = translate_Claim(_sndDisjunct)
      binaryOr(frstDisjunct, sndDisjunct)
    case Negated_Formula(_formula) =>
      Apply(constant("not"),translate_Claim(_formula))
    case Contradiction() => constant("contradiction")
    case Qualifying_Formula(_tm, _tp) => is(translate_Term(_tm), translate_Type(_tp))
    case Private_Predicate_Formula(redObjAttr, idnr, _args) =>
      val ln = LocalName(Utils.MizarVariableName(redObjAttr.spelling, redObjAttr.sort.stripSuffix("-Formula"), idnr))
      val name = makeSimpleGlobalName(currentAid, ln.toString).name
      val p = if (ln.steps.tail.head.toString == mapKind("Private-Predicate")) {
        OMS(TranslationController.currentTheoryPath ? ln)
      } else if (defContext.withinProof) {
        defContext.lookupLocalDefinitionWithinSameProof(ln) getOrElse OMV(ln)
      } else {
        OMV(ln) ^ implicitNamedDefArgsSubstition()
      }
      ApplyGeneral(p, translateArguments(_args))
    case FlexaryDisjunctive_Formula(_formulae) =>
      val formulae = _formulae map translate_Claim
      Or(formulae)
    case FlexaryConjunctive_Formula(_formulae) =>
      val formulae = _formulae map translate_Claim
      And(formulae)
    case rf@RightSideOf_Relation_Formula(objAttrs, antonymic, infixedArgs) =>
      translate_Formula(Relation_Formula(objAttrs, antonymic, infixedArgs))
    case mrl@Multi_Relation_Formula(_relForm, _rhsOfRFs) =>
      And(_relForm::_rhsOfRFs map (translate_Formula))
  }
  def translate_Existential_Quantifier_Formula(vars:Context, expression:Term, restrictionO: Option[Claim])(implicit defContext: DefinitionContext): Term = vars.variables match {
    case Nil => restrictionO match {
      case Some(ass) => binaryAnd(translate_Claim(ass), expression)
      case None => expression
    }
    case v::vs =>
      defContext.addLocalBindingVar(v)
      val expr = translate_Existential_Quantifier_Formula(vs, expression, restrictionO)
      exists(v, expr)
  }
  def translate_Universal_Quantifier_Formula(vars: Context, expression:Term, restrictionO: Option[Claim])(implicit defContext: DefinitionContext): Term = vars.variables match {
    case Nil => restrictionO match {
      case Some(ass) => implies(translate_Claim(ass), expression)
      case None => expression
    }
    case v::vs =>
      defContext.addLocalBindingVar(v)
      val expr = translate_Universal_Quantifier_Formula(vs, expression, restrictionO)
      forall(v, expr)
  }
  def translate_Restriction(maybeRestriction: Option[Restriction]) = maybeRestriction map {
    case Restriction(_formula) => _formula
  }
}

object contextTranslator {
  def translate_Variable(variable:Variable)(implicit defContext: DefinitionContext) : Term = {
    translate_new_Variable(variable) ^ implicitNamedDefArgsSubstition()
  }
  /**
   * translate a new variable to the corrensponding OMV
   * @param variable
   * @return
   */
  def translate_new_Variable(variable:Variable) : OMV = OMV(variable.toIdentifier)
  /**
   * translate a new variable within a binder,
   * i.e. adds the variable to the arguments in the definition context
   * @param variable
   * @param tp the type of the new variable
   * @return
   */
  def translate_binding_Variable(variable:Variable, tp: Term)(implicit defContext: => DefinitionContext) : Unit = {
    defContext.addArguments(translate_new_Variable(variable) % tp)
  }
  private def translateSingleTypedVariable(_var : Variable, _tp: Type)(implicit defContext: DefinitionContext, selectors: List[(Int, VarDecl)] = Nil) = {
    val variable = translate_new_Variable(_var)
    val tp = translate_Type(_tp)
    variable % tp
  }
  def translate_Context(varSegm: VariableSegments)(implicit defContext: DefinitionContext, selectors: List[(Int, VarDecl)] = Nil) : Context= varSegm match {
    case Free_Variable_Segment(_var, _tp) => translateSingleTypedVariable(_var, _tp)
    case Implicitly_Qualified_Segment(_var, _tp) =>translateSingleTypedVariable(_var, _tp)
    case Explicitly_Qualified_Segment(_variables, _tp) => _variables._vars.map(v => translateSingleTypedVariable(v,_tp))
  }
  def translateVariables(varSegms: VariableSegments)(implicit defContext: DefinitionContext) : List[Term] = {varSegms._vars().map(translate_Variable(_))}
  def translateVariables(varSegms: ContextSegments) : List[OMV] = {varSegms._children.flatMap(_._vars()).map(translate_new_Variable(_))}
  def translateBindingVariables(segm: Segments)(implicit defContext: => DefinitionContext) : Unit = {
    val argTypes = segm._tpList._tps map (translate_Type(_)(defContext))
    val ret = segm match {
      case Functor_Segment(_vars, _tpList, _tpSpec) => _tpSpec map(_._types) map (translate_Type(_)(defContext)) get
      case Predicate_Segment(_vars, _tpList) => prop
    }
    val tp = Arrow(argTypes, ret)
    segm._vars._vars foreach {
      v =>
        translate_binding_Variable(v, tp)(defContext)
    }
  }
  def translateNewVariables(segm: Segments)(implicit defContext: DefinitionContext) : Context = {
      val argTypes = segm._tpList._tps map translate_Type
      val ret = segm match {
        case Functor_Segment(_vars, _tpList, _tpSpec) => _tpSpec map(_._types) map translate_Type get
        case Predicate_Segment(_vars, _tpList) => prop
      }
      val tp = Arrow(argTypes, ret)
      segm._vars._vars.map (translate_new_Variable(_) % tp)
  }
	def translate_Locus(loc:Locus)(implicit defContext: DefinitionContext) : Term = OMV(loc.toIdentifier) ^ implicitNamedDefArgsSubstition()
  def translateLocisWithoutSubstitution(loc:Loci)(implicit defContext: DefinitionContext) : List[OMV] = loc._loci map (l=>OMV(l.toIdentifier))
  def translate_Context_Segment(con: ContextSegments)(implicit defContext: DefinitionContext): Context = con._children flatMap translate_Context
}


object claimTranslator {
  def translate_Type_Unchanging_Claim(claim: TypeUnchangingClaim)(implicit defContext: DefinitionContext): Term = claim match {
    case form: Formula => translate_Formula(form)
    case Proposition(_label, _thesis) => translate_Claim(_thesis)
    case Thesis() => thesis()
    case Diffuse_Statement(_label) => throw ImplementationError("This should never get called, diffuse statement are treated separately in the justification translator.")
    case Conditions(_props) => And(_props map translate_Claim)
    case Iterative_Equality(_label, _formula, _just, _iterSteps) =>
      val fstRelat = translate_Formula(_formula)
      val ApplyGeneral(relat, List(_, sndTm)) = fstRelat match {
        case not(f) => f
        case f => f
      }
      val otherTms = _iterSteps._iterSteps.map(_._tm).map(translate_Term)
      val eqClaims = sndTm :: otherTms.init.zip(otherTms)
        .map { case (x, y) => ApplyGeneral(relat, List(x, y)) }
      And(eqClaims)
  }
  def translate_Claim(claim:Claim)(implicit defContext: => DefinitionContext) : Term =  claim match {
    //Only here we might change definContext
    case Type_Changing_Claim(_eqList, _tp) =>
      val tp = translate_Type(_tp)(defContext)
      val reconsideringVars = _eqList._eqns map { case eq =>
        val v = VarDecl(translate_new_Variable(eq._var).name, tp, translate_Term(eq._tm)(defContext))
        //adjust known variables in the context
        eq match {
          case Equality(_, _) =>
            if (defContext.topLevel || defContext.withinProof) defContext.addArguments(v) else defContext.addProofArg(v)
          case Equality_To_Itself(_, _) => defContext.replaceArguments(v)
        }
        v
      }
      val typingClaims = reconsideringVars.map(_.df.get) map(is(_, tp))
      And(typingClaims)
    case claim: TypeUnchangingClaim => translate_Type_Unchanging_Claim(claim)(defContext)
  }
  def translate_Assumption_Claim(ass: Assumptions)(implicit defContext: DefinitionContext) : Term = ass match {
    case Collective_Assumption(_cond) => translate_Claim(_cond)
    case Existential_Assumption(_qualSegm, _cond) =>
      val args = _qualSegm._children flatMap translate_Context
      val cond = translate_Claim(_cond)
      translate_Existential_Quantifier_Formula(args, cond, None)(defContext)
    case Single_Assumption(_prop) => translate_Claim(_prop)
    case Suppose_Head(_ass) => translate_Assumption_Claim(_ass)
  }
}

object attributeTranslator {
  def translateAttributes(adjective_Cluster: Adjective_Cluster)(implicit defContext: DefinitionContext) = adjective_Cluster._attrs map translate_Attribute
  def translate_Attribute(attr: Attribute)(implicit defContext: DefinitionContext): Term = {
    val gn = globalReference(attr)
    val args = translateArguments(attr._args)
    ApplyGeneral(OMS(gn), args)
  }
}