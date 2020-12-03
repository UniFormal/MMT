package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects
import info.kwarc.mmt.mizar.newxml.syntax.{Assumption, Attr_Antonym, Attr_Synonym, Attribute_Definition, Attribute_Pattern, Block, CaseBasedExpr, Case_Block, Case_Head, Choice_Statement, Cluster, Conclusion, Constant_Definition, Correctness, Correctness_Condition, Definiens, Definition, Definition_Item, Exemplification, Func_Antonym, Func_Synonym, Functor_Definition, Generalization, Heads, Identify, Justification, Loci_Declaration, Mode_Definition, Mode_Synonym, Nyms, PartialDef, Per_Cases, Pragma, Pred_Antonym, Pred_Synonym, Predicate_Definition, Private_Functor_Definition, Private_Predicate_Definition, Property, Redefine, Reduction, Reservation, Scheme_Block_Item, Scheme_Head, Section_Pragma, Statement, Structure_Definition, Suppose_Head, Theorem_Item, Type_Changing_Statement}
import info.kwarc.mmt.mizar.newxml.translator.{TranslationController, Utils, expressionTranslator, justificationTranslator}

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
      TranslationController.makeConstant(gn, notC, defn, None)
      /*if (_redef.occurs) {
        translate_Redefine(_redef, atd, elabDefn)
      } else {
        elabDefn
      } */
  }
  def translate_Constant_Definition(constant_Definition: Constant_Definition) = { ??? }
  def translate_Functor_Definition(functor_Definition: Functor_Definition) = { ??? }
  def translate_Mode_Definition(mode_Definition: Mode_Definition) = { ??? }
  def translate_Private_Functor_Definition(private_Functor_Definition: Private_Functor_Definition) = { ??? }
  def translate_Private_Predicate_Definition(private_Predicate_Definition: Private_Predicate_Definition) = { ??? }
  def translate_Predicate_Definition(predicate_Definition: Predicate_Definition) = { ??? }
  /*def translate_Redefine(red:Redefine, defn:Definition, elabDefn: info.kwarc.mmt.api.symbols.Declaration):symbols.Declaration = {
    // TODO: translate the redefine
    elabDefn
  }*/
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
