package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import documents.Document
import info.kwarc.mmt.api.modules.Theory
import symbols.Declaration
import info.kwarc.mmt.mizar.newxml.Main.makeParser
import info.kwarc.mmt.mizar.newxml.syntax._
import info.kwarc.mmt.mizar.newxml.translator.definiensTranslator.assumptionTranslator


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
    val translatedSubitem : List[info.kwarc.mmt.api.ContentElement] = item._subitem match {
      case subitem: MMLIdSubitem => subitem match {
        case scheme_Block_Item: Scheme_Block_Item => translate_Scheme_Block_Item(scheme_Block_Item)
        case theorem_Item: Theorem_Item => statementTranslator.translate_Theorem_Item(theorem_Item)
        case attrDef: Attribute_Definition => definitionTranslator.translate_Attribute_Definition(attrDef)
        case funcDef: Functor_Definition => definitionTranslator.translate_Functor_Definition(funcDef)
        case pd: Predicate_Definition => definitionTranslator.translate_Predicate_Definition(pd)
      }
      case existAss: Existential_Assumption => assumptionTranslator.translateAssumptions(existAss)
      case propReg: Property_Registration => registrationTranslator.translateRegistration(propReg:Registrations)
      case res: Reservation => translate_Reservation(res)
      case defIt: Definition_Item => translate_Definition_Item(defIt)
      case sectPragma: Section_Pragma => translate_Section_Pragma(sectPragma)
      case pr: Pragma => translate_Pragma(pr)
      case lociDecl: Loci_Declaration => throw new DeclarationLevelTranslationError("Unexpected Loci-Declaration on Top-Level.", lociDecl)
      case cl: Cluster => translate_Cluster(cl)
      case correctness: Correctness => translate_Correctness(correctness)
      case correctness_Condition: Correctness_Condition => translate_Correctness_Condition(correctness_Condition)
      case exemplification: Exemplification => translate_Exemplification(exemplification)
      case assumption: Assumption => translate_Assumption(assumption)
      case identify: Identify => translate_Identify(identify)
      case generalization: Generalization => translate_Generalization(generalization)
      case reduction: Reduction => translate_Reduction(reduction)
      case property: Property => translate_Property(property)
      case per_Cases: Per_Cases => translate_Per_Cases(per_Cases)
      case case_block: Case_Block => translate_Case_Block(case_block)
      case head: Heads => headTranslator.translate_Head(head)
      case nym: Nyms => nymTranslator.translate_Nym(nym)
      case st: Statement => statementTranslator.translate_Statement(st)
      case defn: Definition => definitionTranslator.translate_Definition(defn)
    }
    translatedSubitem map {
      //Currently probably the only case that actually occurs in practise
      case decl: Declaration =>
        val name = decl.name
        TranslationController.add(decl)
      case mod: info.kwarc.mmt.api.modules.Module => TranslationController.add(mod)
      case nar: NarrativeElement => TranslationController.add(nar)
    }
  }
}

import TranslationController._
class MizarXMLImporter extends archives.Importer {
  val key = "mizarxml-omdoc"
  def inExts = List("esx1")

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
    TranslationController.controller = controller
    TranslationController.currentAid = aid
    TranslationController.currentOutputBase = bf.narrationDPath.^!

    //val doc = TranslationController.makeDocument()
    val th = TranslationController.makeTheory()

    articleTranslator.translateArticle(text_Proper)
    log("INDEXING ARTICLE: " + bf.narrationDPath.last)
    TranslationController.endMake()
    log("The translated article " + bf.narrationDPath.last + ": ")

    log("theory "+th.name)
    th.getDeclarations foreach {case decl: Declaration =>
    try {
        log("\t" + TranslationController.controller.presenter.asString(decl))
      } catch {
        case e: GeneralError =>
          println("General error while presenting the declaration: "+decl.toString+": ")
          println(e.toStringLong)
          throw e
      }
    }
    TranslationController.currentDoc//currentThy.asDocument
  }
}