package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.VarDecl
import info.kwarc.mmt.lf.ApplyGeneral
import info.kwarc.mmt.mizar.newxml.mmtwrapper.{PatternUtils, StructureInstance}
import info.kwarc.mmt.mizar.newxml.syntax._
import info.kwarc.mmt.mizar.newxml.translator.{TranslationController, TranslatorUtils, expressionTranslator, justificationTranslator}

object subitemTranslator {
  def translate_Reservation(reservation: Reservation) = { Nil }
  def translate_Definition_Item(definition_Item: Definition_Item) = {
    definition_Item.check()
    blockTranslator.translate_Definition_Block(definition_Item._block)
  }
  def translate_Section_Pragma(section_Pragma: Section_Pragma) = { Nil }
  def translate_Pragma(pragma: Pragma) = { ??? }
  def translate_Loci_Declaration(loci_Declaration: Loci_Declaration): List[(Option[LocalName], objects.Term)] = {
    val varContext = loci_Declaration._qualSegms._children flatMap contextTranslator.translate_Context
    varContext map {vd => (Some(vd.name), vd.tp.get)}
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
    case regular_Statement: Regular_Statement => translate_Regular_Statement(regular_Statement)
  }
  def translate_Conclusion(conclusion: Conclusion) = { ??? }
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement) = { ??? }
  def translate_Theorem_Item(reservation: Theorem_Item) = { ??? }
  def translate_Choice_Statement(reservation: Choice_Statement) = { ??? }
  def translate_Regular_Statement(regular_Statement: Regular_Statement) = { ??? }
}

object definitionTranslator {
  def translate_Definition(defn:Definition, args: List[(Option[LocalName], objects.Term)]= Nil) : List[info.kwarc.mmt.api.symbols.Declaration] = defn match {
    case at: Attribute_Definition => translate_Attribute_Definition(at)
    case cd: Constant_Definition => translate_Constant_Definition(cd)
    case funcDef: Functor_Definition => translate_Functor_Definition(funcDef)
    case md: Mode_Definition => translate_Mode_Definition(md)
    case pd: Predicate_Definition => translate_Predicate_Definition(pd)
    case d: Private_Functor_Definition  => translate_Private_Functor_Definition(d)
    case d: Private_Predicate_Definition => translate_Private_Predicate_Definition(d)
    case d: Structure_Definition => translate_Structure_Definition(d, args)
  }
  def translate_Structure_Definition(structure_Definition: Structure_Definition, args: List[(Option[LocalName], objects.Term)]): List[symbols.Declaration] = {
    val l = args.length
    implicit var selectors: List[(Int, VarDecl)] = Nil
    val substr: List[objects.Term] = structure_Definition._ancestors._structTypes.map(typeTranslator.translate_Type) map {
      case ApplyGeneral(typeDecl, args) => typeDecl
    }
    val n = substr.length
    var substitutions : List[objects.Sub] = Nil
    //val strName = structure_Definition._rendering._aggrFuncPat.extPatDef.extPatAttr.patAttr.spell.spelling
    val patternNr = structure_Definition._strPat.extPatDef.extPatAttr.patAttr.patternnr.patternnr
    val declarationPath = TranslatorUtils.makeGlobalName(TranslationController.currentAid, "Struct-Type", patternNr)

    def translate_Field_Segments(field_Segments: List[Field_Segment]) : List[VarDecl] = field_Segments flatMap {
      case field_Segment: Field_Segment =>
			val tp = typeTranslator.translate_Type(field_Segment._tp)
      field_Segment._selectors._loci foreach { case selector =>
        val selName = contextTranslator.translate_Locus(selector._loci)
        val sel = (selector.posNr.nr.nr, selName % tp)
        selectors ::= sel
        substitutions ::= selName / PatternUtils.referenceExtDecl(declarationPath, selName.name.toString)
      }
      selectors = selectors.reverse
      selectors map (_._2 ^ substitutions)
    }
    val fieldDecls = translate_Field_Segments(structure_Definition._fieldSegms.flatMap(_._fieldSegments).distinct).distinct
    val m = fieldDecls.length
    println(declarationPath.toString)
    StructureInstance(declarationPath, l, args, n, substr, m, fieldDecls)
  }
  def translate_Attribute_Definition(attribute_Definition: Attribute_Definition) = attribute_Definition match {
    case atd @ Attribute_Definition(_, _redef, _attrPat, _def) =>
      val gn = TranslatorUtils.MMLIdtoGlobalName(atd.mizarGlobalName())
      val defn = _def.map(definiensTranslator.translate_Definiens(_))
      val notC = patternTranslator.translate_Attribute_Pattern(_attrPat)
      List(TranslationController.makeConstant(gn, notC, defn, None))
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
  def translate_Definition_Block(block:Block):List[symbols.Declaration] = {
    val definitionItems = block._items
    var args: List[(Option[LocalName], objects.Term)] = Nil
    var resDecls:List[symbols.Declaration] = Nil

    definitionItems foreach { it: Item =>
      it._subitem match {
        case loci_Declaration: Loci_Declaration =>
          args = args ++ subitemTranslator.translate_Loci_Declaration(loci_Declaration)
        case defn: Definition =>
          val translDef = definitionTranslator.translate_Definition(defn, args)
          resDecls = resDecls ++ translDef
        case defIt => throw new TranslatingError("definition expected inside definition-item.\n Instead found " + defIt.kind)
      }
    }
    resDecls
  }
  def translate_Justification_Block(block:Block) : Unit = {
    val justificationItems = block._items

    justificationItems foreach {
      case just:Justification =>
        val sourceReg = just.pos.sourceRegion()
        val translJust = justificationTranslator.translate_Justification(just)
        //TranslationController.addSourceRef(translJust, sourceReg)
        translJust
      case _ => throw new java.lang.Error("justification expected inside justification-item.")
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

  object assumptionTranslator {
    def translateAssumption(ass:Assumption) = translateAssumptions(ass._ass)
      def translateAssumptions(ass:Assumptions) = { ??? }
  }
}

object registrationTranslator {
  def translateRegistration(reg:Registrations) = { ??? }
}
