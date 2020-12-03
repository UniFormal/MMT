package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMMOD, OMV}
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.{DPath, LocalName, NarrativeElement, archives, documents, objects}
import info.kwarc.mmt.lf.{Apply, ApplyGeneral}
import info.kwarc.mmt.mizar.newxml.Main.makeParser
import info.kwarc.mmt.mizar.newxml.mmtwrapper
import info.kwarc.mmt.mizar.newxml.syntax.Utils.MizarGlobalName
import info.kwarc.mmt.mizar.newxml.syntax._

object articleTranslator {
  def translateArticle(text_Proper: Text_Proper) = {
    val items = text_Proper._items map itemTranslator.translateItem
  }
}

import subitemTranslator._
object itemTranslator {
  // Adds the corresponding content to the TranslationController
  def translateItem(item: Item) = {
    val sourceReg = item.pos.sourceRegion()
    item.checkKind()
    val translatedSubitem : info.kwarc.mmt.api.ContentElement = item match {
      case res:Reservation =>  translate_Reservation(res)
      case defIt: Definition_Item => translate_Definition_Item(defIt)
      case sectPragma: Section_Pragma=> translate_Section_Pragma(sectPragma)
      case pr:Pragma => translate_Pragma(pr)
      case lociDecl: Loci_Declaration => translate_Loci_Declaration(lociDecl)
      case cl:Cluster => translate_Cluster(cl)
      case correctness: Correctness => translate_Correctness(correctness)
      case correctness_Condition: Correctness_Condition => translate_Correctness_Condition(correctness_Condition)
      case exemplification: Exemplification => translate_Exemplification(exemplification)
      case assumption: Assumption => translate_Assumption(assumption)
      case identify: Identify => translate_Identify(identify)
      case generalization: Generalization => translate_Generalization(generalization)
      case reduction: Reduction => translate_Reduction(reduction)
      case scheme_Block_Item: Scheme_Block_Item => translate_Scheme_Block_Item(scheme_Block_Item)
      case property: Property => translate_Property(property)
      case per_Cases: Per_Cases => translate_Per_Cases(per_Cases)
      case case_block: Case_Block => translate_Case_Block(case_block)
      case head:Heads => headTranslator.translate_Head(head)
      case nym:Nyms => nymTranslator.translate_Nym(nym)
      case st:Statement => statementTranslator.translate_Statement(st)
      case defn: Definition => definitionTranslator.translate_Definition(defn)
    }
    translatedSubitem match {
      case decl: Declaration => TranslationController.add(decl)
      case mod: info.kwarc.mmt.api.modules.Module => TranslationController.add(mod)
      case nar: NarrativeElement => TranslationController.add(nar)
    }
    //TranslationController.addSourceRef(translatedSubitem, sourceReg)
  }
}

object subitemTranslator {
  def translate_Reservation(reservation: Reservation) = { ??? }
  def translate_Definition_Item(definition_Item: Definition_Item) = {
    definition_Item.check()
    blockTranslator.translate_Definition_Block(definition_Item._block)
    ???
  }
  def translate_Section_Pragma(section_PRagma: Section_Pragma) = { ??? }
  def translate_Pragma(pragma: Pragma) = { ??? }
  def translate_Loci_Declaration(loci_Declaration: Loci_Declaration) = { ??? }
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
  def translate_Nym(nym:Nyms) = { ??? }
  def translate_Pred_Antonym(pred_Antonym: Pred_Antonym) = {}
  def translate_Pred_Synonym(pred_Synonym: Pred_Synonym) = {}
  def translate_Attr_Synonym(attr_Synonym: Attr_Synonym) = {}
  def translate_Attr_Antonym(attr_Antonym: Attr_Antonym) = {}
  def translate_Func_Synonym(func_Synonym: Func_Synonym) = {}
  def translate_Func_Antonym(func_Antonym: Func_Antonym) = {}
  def translate_Mode_Synonym(mode_Synonym: Mode_Synonym) = {}
}

object statementTranslator {
  def translate_Statement(st:Statement) = st match {
    case conclusion: Conclusion => translate_Conclusion(conclusion)
    case type_Changing_Statement: Type_Changing_Statement => translate_Type_Changing_Statement(type_Changing_Statement)
    case theorem_Item: Theorem_Item => translate_Theorem_Item(theorem_Item)
    case choice_Statement: Choice_Statement => translate_Choice_Statement(choice_Statement)
  }
  def translate_Conclusion(conclusion: Conclusion) = { ??? }
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement) = { ??? }
  def translate_Theorem_Item(reservation: Theorem_Item) = { ??? }
  def translate_Choice_Statement(reservation: Choice_Statement) = { ??? }
}

object definitionTranslator {
  def translate_Definition(defn:Definition) : info.kwarc.mmt.api.symbols.Declaration = defn match {
    case at: Attribute_Definition => translate_Attribute_Definition(at)
    case cd: Constant_Definition => translate_Constant_Definition(cd)
    case funcDef: Functor_Definition => translate_Functor_Definition(funcDef)
    case md: Mode_Definition => translate_Mode_Definition(md)
    case pd: Predicate_Definition => translate_Predicate_Definition(pd)
    case d: Private_Functor_Definition  => translate_Private_Functor_Definition(d)
    case d: Private_Predicate_Definition => translate_Private_Predicate_Definition(d)
    case d: Structure_Definition => translate_Structure_Definition(d)
  }
  def translate_Structure_Definition(structure_Definition: Structure_Definition) = { ??? }
  def translate_Attribute_Definition(attribute_Definition: Attribute_Definition) = attribute_Definition match {
    case atd @ Attribute_Definition(_, _redef, _attrPat, _def) =>
      val gn = Utils.MMLIdtoGlobalName(atd.mizarGlobalName())
      val defn = _def.map(definiensTranslator.translate_Definiens(_))
      val notC = patternTranslator.translate_Attribute_Pattern(_attrPat)
      if (_redef.occurs) {
        redefinitionTranslator.translate_Redefine(_redef, atd)
      }
      TranslationController.makeConstant(gn, notC, defn, None)
  }
  def translate_Constant_Definition(constant_Definition: Constant_Definition) = { ??? }
  def translate_Functor_Definition(functor_Definition: Functor_Definition) = { ??? }
  def translate_Mode_Definition(mode_Definition: Mode_Definition) = { ??? }
  def translate_Private_Functor_Definition(private_Functor_Definition: Private_Functor_Definition) = { ??? }
  def translate_Private_Predicate_Definition(private_Predicate_Definition: Private_Predicate_Definition) = { ??? }
  def translate_Predicate_Definition(predicate_Definition: Predicate_Definition) = { ??? }
}

object patternTranslator {
  def translate_Attribute_Pattern(atp:Attribute_Pattern):NotationContainer = {
    NotationContainer.empty()
  }
}

object blockTranslator {
  def translate_Definition_Block(block:Block): Unit = {
    val definitionItems = block._items

    definitionItems foreach {
      case defn:Definition =>
        val sourceReg = defn.pos.sourceRegion()
        val translDef = definitionTranslator.translate_Definition(defn)
        //TranslationController.addSourceRef(translDef, sourceReg)
        translDef
    }
  }
  def translate_Justification_Block(block:Block) : Unit = {
    val justificationItems = block._items

    justificationItems foreach {
      case just:Justification =>
        val sourceReg = just.pos.sourceRegion()
        val translJust = justificationTranslator.translate_Justification(just)
        //TranslationController.addSourceRef(translJust, sourceReg)
        translJust
    }
  }
}

object definiensTranslator {
  def translate_Definiens(defs:Definiens):objects.Term = {
    translate_CaseBasedExpr(defs._expr)
  }
  def translate_CaseBasedExpr(defn:CaseBasedExpr): objects.Term = {
    defn.check()
    if (defn.isSingleCase()) {
      expressionTranslator.translate_Expression(defn.singleCasedExpr._expr.get)
    } else {
      translate_Cased_Expression(defn.partialCasedExpr)
    }
  }
  def translate_Cased_Expression(partDef:PartialDef):objects.Term = {
    assert(partDef._partDefs.isDefined)
    ???
  }
}

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
    case Aggregate_Term(tpAttrs, _args) => ???
    case Selector_Term(tpAttrs, _args) => ???
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
    case Forgetful_Functor_Term(constrExtObjAttrs, _tm) => ???
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

object redefinitionTranslator {
  def translate_Redefine(red:Redefine, defn:Definition) = {}
}

object justificationTranslator {
  def translate_Justification(just:Justification) = { ??? }
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

class MizarXMLImporter extends archives.Importer {
  val key = "mizarxml-omdoc"
  def inExts = List("esx")

  def importDocument(bf: archives.BuildTask, index: documents.Document => Unit): archives.BuildResult = {
    val parser = makeParser
    val text_Proper = parser.apply(bf.inFile).asInstanceOf[Text_Proper]
    val doc = translate(text_Proper, bf)

    index(doc)
    //archives.BuildResult.empty
    archives.BuildResult.fromImportedDocument(doc)
  }

  def translate(text_Proper: Text_Proper, bf:archives.BuildTask) : Document = {
    val aid = text_Proper.articleid
    TranslationController.currentAid = aid
    TranslationController.currentOutputBase = bf.narrationDPath.^!

    val doc = TranslationController.makeDocument()
    val th = TranslationController.makeTheory()

    articleTranslator.translateArticle(text_Proper)
    log("INDEXING ARTICLE: " + bf.narrationDPath.last)
    TranslationController.endMake()
    doc
  }
}

object Utils {
  def MMLIdtoGlobalName(mizarGlobalName: MizarGlobalName): info.kwarc.mmt.api.GlobalName = {
    val theoryName = LocalName(mizarGlobalName.aid)
    val ln = LocalName(mizarGlobalName.kind+":"+mizarGlobalName.nr)
    TranslationController.currentThyBase ? theoryName ? ln
  }
  def addConstant(gn:info.kwarc.mmt.api.GlobalName, notC:NotationContainer, df: Option[objects.Term], tp:Option[objects.Term] = None) = {
    val hm : Term= OMMOD(gn.module).asInstanceOf[Term]
    val const = info.kwarc.mmt.api.symbols.Constant(OMMOD(gn.module), gn.name, Nil, tp, df, None, notC)
    TranslationController.add(const)
  }
  def conforms(A:Type, B:Type) : Boolean = {A == B}
  def negatedFormula(form:Claim) = Negated_Formula(RedObjSubAttrs(emptyPosition(),Sort("Negated-Formula")),form)
  def emptyCondition() = negatedFormula(Contradiction(RedObjSubAttrs(emptyPosition(),Sort("Contradiction"))))
  def emptyPosition() = Position("translation internal")
  def getUniverse(tp:Type) : Term = tp match {
    case Standard_Type(ExtObjAttrs(_, _, _, Spelling("Element"), Sort("Mode")), _, _, args) =>
      args match { case List(Arguments(List(u))) => u }
  }
  def getVariables(varSegms: Variable_Segments) : List[Variable] = varSegms._vars.flatMap {
    case segm: VariableSegments => segm._vars()
  }
  def translateVariables(varSegms: VariableSegments) : List[OMV] = {varSegms._vars().map(variableTranslator.translate_Variable)}
  def translateVariables(varSegms: Variable_Segments) : List[OMV] = {getVariables(varSegms).map(variableTranslator.translate_Variable)}
  def firstVariableUniverse(varSegms: VariableSegments) : Type = {
    assert(! varSegms._vars().isEmpty)
    varSegms._tp()
  }
  def firstVariableUniverse(varSegm: Variable_Segments) : Type = {
    varSegm._vars.head._tp()
  }
  def translateArguments(args: Arguments) : List[objects.Term] = {args._children map termTranslator.translate_Term }
  def translateObjRef(refObjAttrs:referencingObjAttrs)  = objects.OMS(MMLIdtoGlobalName(refObjAttrs.globalName()))
}