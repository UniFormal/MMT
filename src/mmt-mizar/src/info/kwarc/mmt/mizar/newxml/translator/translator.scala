package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.modules.{ModuleOrLink, ModuleWrapper, Theory}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMID, OMMOD}
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.{DPath, LocalName, NarrativeElement, archives, documents, objects}
import info.kwarc.mmt.mizar.mmtwrappers
import info.kwarc.mmt.mizar.mmtwrappers.Mizar
import info.kwarc.mmt.mizar.newxml.Main.makeParser
import info.kwarc.mmt.mizar.newxml.syntax.Utils.MizarGlobalName
import info.kwarc.mmt.mizar.newxml.syntax._
import info.kwarc.mmt.mizar.newxml.translator.TranslationController

object articleTranslator {
  def translateArticle(text_Proper: Text_Proper, dpath:DPath) = {
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
    TranslationController.addSourceRef(translatedSubitem, sourceReg)
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
        TranslationController.addSourceRef(translDef, sourceReg)
        translDef
    }
  }
  def translate_Justification_Block(block:Block) : Unit = {
    val justificationItems = block._items

    justificationItems foreach {
      case just:Justification =>
        val sourceReg = just.pos.sourceRegion()
        val translJust = justificationTranslator.translate_Justification(just)
        TranslationController.addSourceRef(translJust, sourceReg)
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
    case Circumfix_Term(tpAttrs, _symbol, _args) => ???
    case Numeral_Term(posNr, srt, varnr) => ???
    case it_Term(pos, sort) => throw new java.lang.Error("Unresolved implicit reference in term.")
    case Internal_Selector_Term(redObjAttr, varnr) => ???
    case Infix_Term(tpAttrs, infixedArgs) => ???
    case Global_Choice_Term(srt, pos, _tp) => ???
    case Placeholder_Term(redObjAttr, varnr) => ???
    case Private_Functor_Term(redObjAttr, serialnr, _args) => ???
    case Fraenkel_Term(pos, srt, _varSegms, _tm, _form) => ???
    case Simple_Fraenkel_Term(pos, srt, _varSegms, _tm) => ???
    case Qualification_Term(pos, srt, _tm, _tp) => ???
    case Forgetful_Functor_Term(constrExtObjAttrs, _tm) => ???
  }
}

object typeTranslator {
  def translate_Type(tp:Type) : objects.Term = tp match {
    case ReservedDscr_Type(idnr, nr, srt, _subs, _tp) => ???
    case Clustered_Type(srt, pos, _adjClust, _tp) => ???
    case Standard_Type(tpAttrs, noocc, origNr, _args) =>
      // Seems to roughly correspond to an OMS referencing a type
      // TODO: Check this the correct semantics and take care of the noocc attribute
      val gn = Utils.MMLIdtoGlobalName(tpAttrs.globalName())
      val tp : objects.Term = objects.OMS(gn).asInstanceOf[objects.Term]
      tp
    case Struct_Type(tpAttrs, _args) => ???
      val formatNr = tpAttrs.extAttrs.formatNr
      val patternNr = tpAttrs.extAttrs.patNr
      val position = tpAttrs.extAttrs.posNr.pos.parsePosition()
      val nr = tpAttrs.extAttrs.posNr.nr
      val constrnr = tpAttrs.constrNr
      ???
  }
}

object formulaTranslator {
  def translate_Formula(formula:Formula) : objects.Term = formula match {
    case Existential_Quantifier_Formula(srt, pos, _vars, _expression) => ???
    case Relation_Formula(objectAttrs, infixedArgs) => ???
    case Universal_Quantifier_Formula(srt, pos, _vars, _restrict, _expression) => ???
    case Multi_Attributive_Formula(srt, pos, _tm, _clusters) => ???
    case Conditional_Formula(srt, pos, _formulae) => ???
    case Conjunctive_Formula(srt, pos, _formulae) => ???
    case Biconditional_Formula(srt, pos, _frstFormula, _sndFormula) => ???
    case Disjunctive_Formula(srt, pos, _formulae) => ???
    case Negated_Formula(srt, pos, _formula) => ???
    case Contradiction(srt, pos) => ???
    case Qualifying_Formula(srt, pos, _tm, _tp) => ???
    case Private_Predicate_Formula(redObjAttr, serialNr, constrNr, _args) => ???
    case FlexaryDisjunctive_Formula(srt, pos, _formulae) => ???
    case FlexaryConjunctive_Formula(srt, pos, _formulae) => ???
    case Multi_Relation_Formula(srt, pos, _relForm, _rhsOfRFs) => ???
  }
}

object redefinitionTranslator {
  def translate_Redefine(red:Redefine, defn:Definition) = {}
}

object justificationTranslator {
  def translate_Justification(just:Justification) = { ??? }
}

class MizarImporter extends archives.Importer {
  val key = "mizarxml-omdoc"
  def inExts = List("esx")

  def importDocument(bf: archives.BuildTask, index: documents.Document => Unit): archives.BuildResult = {
    val parser = makeParser
    val text_Proper = parser.apply(bf.inFile).asInstanceOf[Text_Proper]
    val doc = translate(text_Proper, bf)

    index(doc)
    archives.BuildResult.empty
  }

  def translate(text_Proper: Text_Proper, bf:archives.BuildTask) : Document = {
    val aid = text_Proper.articleid
    val dpath = bf.narrationDPath.^! / (aid.toString().toLowerCase() + ".omdoc")

    TranslationController.currentBase = getBase(bf.inFile)
    TranslationController.currentAid = aid

    val doc = new Document(dpath, documents.ModuleLevel, None)
    TranslationController.add(doc)

    val th = new Theory(TranslationController.currentThyBase, TranslationController.localPath, Some(Mizar.MizarPatternsTh), Theory.noParams, Theory.noBase)
    TranslationController.add(th)

    articleTranslator.translateArticle(text_Proper, dpath)
    log("INDEXING ARTICLE: " + bf.narrationDPath.last)
    controller.endAdd(th)
    doc
  }

  def getBase(f: File): String = {
    f.toJava.getParentFile.getParent match {
      case null => "./"
      case s => s + "/"
    }
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
}