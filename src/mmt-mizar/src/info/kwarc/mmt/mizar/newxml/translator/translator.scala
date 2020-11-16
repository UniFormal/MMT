package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.{DPath, archives, documents}
import info.kwarc.mmt.mizar.mmtwrappers.Mizar
import info.kwarc.mmt.mizar.newxml.Main.makeParser
import info.kwarc.mmt.mizar.newxml.syntax._
import info.kwarc.mmt.mizar.translator.TranslationController

object articleTranslator {
  def translateArticle(text_Proper: Text_Proper, dpath:DPath) = {
    val items = text_Proper._items map itemTranslator.translateItem
  }
}

import subitemTranslator._
object itemTranslator {
  // Adds the corresponding content to the TranslationController
  def translateItem(item: Item) = {
    item.checkKind()
    val translatedSubitem = item match {
      case res:Reservation =>  translate_Reservation(res)
      case defIt: Definition_Item => translate_Definition_Item(defIt)
      case sectPragma: Section_Pragma=> translate_Section_Pragma(sectPragma)
      case pr:Pragma => translate_Pragma(pr)
      case lociDecl: Loci_Declaration => translate_Loci_Declaration(lociDecl)
      case cl:Cluster => translate_Cluster(cl)
      case _ =>
    }
  }
}

object subitemTranslator {
  def translate_Reservation(reservation: Reservation) = {}
  def translate_Definition_Item(definition_Item: Definition_Item) = {
    definition_Item.check()
    blockTranslator.translate_Definition_Block(definition_Item._block)
  }
  def translate_Section_Pragma(section_PRagma: Section_Pragma) = {

  }
  def translate_Pragma(pragma: Pragma) = {

  }
  def translate_Loci_Declaration(loci_Declaration: Loci_Declaration) = {}
  def translate_Cluster(cluster: Cluster) = {}
  def translate_Correctness(correctness: Correctness) = {}
  def translate_Correctness_Condition(correctness_Condition: Correctness_Condition) = {}
  def translate_Exemplification(exemplification: Exemplification) = {}
  def translate_Assumption(assumption: Assumption) = {}
  def translate_Identify(identify: Identify) = {}
  def translate_Generalization(generalization: Generalization) = {}
  def translate_Reduction(reduction: Reduction) = {}
  def translate_Scheme_Block_Item(scheme_Block_Item: Scheme_Block_Item) = {}
  def translate_Property(property: Property) = {}
  def translate_Per_Cases(per_Cases: Per_Cases) = {}
  def translate_Case_Block(case_block: Case_Block) = {}
  //def translate_Scheme_Head(reservation: Scheme_Head) = {}
  //def translate_Suppose_Head(reservation: Suppose_Head) = {}
  //def translate_Case_Head(reservation: Case_Head) = {}
  def translate_Head(head:Heads) = {}
  def translate_Pred_Antonym(pred_Antonym: Pred_Antonym) = {}
  def translate_Pred_Synonym(pred_Synonym: Pred_Synonym) = {}
  def translate_Attr_Synonym(attr_Synonym: Attr_Synonym) = {}
  def translate_Attr_Antonym(attr_Antonym: Attr_Antonym) = {}
  def translate_Func_Synonym(func_Synonym: Func_Synonym) = {}
  def translate_Func_Antonym(func_Antonym: Func_Antonym) = {}
  def translate_Mode_Synonym(mode_Synonym: Mode_Synonym) = {}
  //def translate_Conclusion(conclusion: Conclusion) = {}
  def translate_Type_Changing_Statement(type_Changing_Statement: Type_Changing_Statement) = {}
  def translate_Theorem_Item(reservation: Theorem_Item) = {}
  def translate_Choice_Statement(reservation: Choice_Statement) = {}
  def translate_Statement(st: Statement) = {}
  def translate_Definition(defn:Definition) = defn match {
    case at: Attribute_Definition => translate_Attribute_Definition(at)
    case cd: Constant_Definition => translate_Constant_Definition(cd)
    case funcDef: Functor_Definition => translate_Functor_Definition(funcDef)
    case md: Mode_Definition => translate_Mode_Definition(md)
    case pd: Predicate_Definition => translate_Predicate_Definition(pd)
    case d: Private_Functor_Definition  => translate_Private_Functor_Definition(d)
    case d: Private_Predicate_Definition => translate_Private_Predicate_Definition(d)
    case d: Structure_Definition => translate_Structure_Definition(d)
  }
  def translate_Structure_Definition(structure_Definition: Structure_Definition) = {}
  def translate_Attribute_Definition(attribute_Definition: Attribute_Definition) = {}
  def translate_Constant_Definition(constant_Definition: Constant_Definition) = {}
  def translate_Functor_Definition(functor_Definition: Functor_Definition) = {}
  def translate_Mode_Definition(mode_Definition: Mode_Definition) = {}
  def translate_Private_Functor_Definition(private_Functor_Definition: Private_Functor_Definition) = {}
  def translate_Private_Predicate_Definition(private_Predicate_Definition: Private_Predicate_Definition) = {}
  def translate_Predicate_Definition(predicate_Definition: Predicate_Definition) = {}
}

object blockTranslator {
  def translate_Definition_Block(block:Block): Unit = {
    val startPos = block.pos.startPosition()
    val endPos = block.pos.endPosition()
    val definitionItems = block._items
    definitionItems foreach {
      case defn:Definition => translate_Definition(defn)
    }
  }
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
    val dpath = bf.narrationDPath.^! / (aid.toLowerCase() + ".omdoc")


    TranslationController.currentBase = getBase(bf.inFile)
    TranslationController.currentAid = aid

    val doc = new Document(dpath, documents.ModuleLevel, None)
    TranslationController.add(doc)

    val th = new Theory(TranslationController.currentThyBase, TranslationController.localPath, Some(Mizar.MizarPatternsTh), Theory.noParams, Theory.noBase)
    TranslationController.add(th)

    articleTranslator.translateArticle(text_Proper, dpath)
    log("INDEXING ARTICLE: " + bf.narrationDPath.last)
    doc
  }

  def getBase(f: File): String = {
    f.toJava.getParentFile.getParent match {
      case null => "./"
      case s => s + "/"
    }
  }
}