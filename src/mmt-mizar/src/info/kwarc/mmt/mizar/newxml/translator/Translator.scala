package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import documents.{Document, MRef}
import symbols.{Constant, Declaration, DerivedDeclaration}
import info.kwarc.mmt.mizar.newxml.Main.makeParser
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
    implicit val defCtx = DefinitionContext.empty()
    //val translatedSubitem : List[info.kwarc.mmt.api.ContentElement] =
    def add(d: Declaration): Unit = TranslationController.add(TranslatorUtils.hiddenRefTranslator(d))
    item._subitem match {
      case defn: Definition => definitionTranslator.translate_Definition(defn) foreach add
      case scheme_Block_Item: Scheme_Block_Item => translate_Scheme_Block_Item(scheme_Block_Item) foreach add
      case theorem_Item: Theorem_Item => add (statementTranslator.translate_Theorem_Item(theorem_Item))
      case res: Reservation => translate_Reservation(res) foreach add
      case defIt: Definition_Item => translate_Definition_Item(defIt) foreach add
      case sectPragma: Section_Pragma => translate_Section_Pragma(sectPragma) foreach add
      case pr: Pragma => translate_Pragma(pr) foreach add
      case lociDecl: Loci_Declaration => throw new DeclarationLevelTranslationError("Unexpected Loci-Declaration on Top-Level.", lociDecl)
      case cl: Cluster => clusterTranslator.translate_Cluster(cl) foreach add
      case identify: Identify => add (translate_Identify(identify))
      case head: Heads => headTranslator.translate_Head(head) foreach add
      case nym: Nyms => nymTranslator.translate_Nym(nym) foreach add
      case st: Statement with TopLevel => add (statementTranslator.translate_Statement(st))
      case notTopLevel: DeclarationLevel => throw subitemTranslator.notToplevel
    }
  }
}

import TranslationController._
class MizarXMLImporter extends archives.Importer {
  val key = "mizarxml-omdoc"
  def inExts = List("esx")

  def importDocument(bf: archives.BuildTask, index: documents.Document => Unit): archives.BuildResult = {
    val parser = makeParser
    val startParsingTime = System.nanoTime()
    def printTimeDiff(nanoDiff: Long, prefix: String = "It took "): Unit = {
      val diffTime = math.round(nanoDiff / 1e9d)
      val seconds = diffTime % 60
      val minutes = (diffTime - seconds) / 60
      println(prefix+minutes.toString+" minutes and "+seconds.toString+" seconds. ")
    }

    val text_Proper = parser.apply(bf.inFile).asInstanceOf[Text_Proper]


    printTimeDiff(System.nanoTime() - startParsingTime, "The parsing took ")
    val startTranslationTime = System.nanoTime()

    val doc = translate(text_Proper, bf)

    printTimeDiff(System.nanoTime() - startTranslationTime, "The translation took ")
    val startAddingTime = System.nanoTime()

    index(doc)
    printTimeDiff(System.nanoTime() - startParsingTime, "The adding took ")

    //archives.BuildResult.empty
    archives.BuildResult.fromImportedDocument(doc)
  }

  def translate(text_Proper: Text_Proper, bf:archives.BuildTask) : Document = {
    val aid = text_Proper.articleid
    TranslationController.controller = controller
    TranslationController.currentAid = aid
    TranslationController.currentOutputBase = bf.narrationDPath.^!
    
    val doc = TranslationController.makeDocument()
    val thy = TranslationController.makeTheory()

    try {
      articleTranslator.translateArticle(text_Proper)
    } catch {
      case GetError(s) if (s.startsWith("no backend applicable to "+TranslationController.currentOutputBase.toString)) =>
        val Array(dpath, name) = s.stripPrefix("no backend applicable to ").split('?')
        val mpath = DPath(utils.URI(dpath)) ? name
        println("GetError since we require the dependency theory "+mpath+" of the translated theory "+currentThy.name+" to be already translated: \n"+
          "Please make sure the theory is translated (build with mizarxml-omdoc build target) and try again. ")
        TranslationController.addUnresolvedDependency(mpath)
    }
    log("INDEXING ARTICLE: " + bf.narrationDPath.last)
    TranslationController.endMake()
    //typecheckContent(TranslationController.currentThy, Some(this.report))

    /*
    log("The translated article " + bf.narrationDPath.last + ": ")
    doc.getModules(TranslationController.controller.globalLookup) foreach {
      case mpath: MPath => TranslationController.controller.getModule(mpath) match {
        case th: Theory => log("theory " + th.name)
        th.getDeclarations foreach {
          case decl: Declaration =>
            try {
              log(TranslationController.controller.presenter.asString(decl))
            } catch {
              case e: GetError =>
              case e: GeneralError =>
                println("General error while presenting the declaration: " + decl.toString + ": ")
                println(e.toStringLong)
                throw e
            }
        }
      }
    }*/
    val unres = TranslationController.getUnresolvedDependencies()
    if (unres.nonEmpty) {println("Unresolved dependencies: "+unres.map(_.name))}
    val deps = TranslationController.getDependencies()
    if (deps.nonEmpty) {println("Resolved dependencies: "+deps.map(_.name))}

    println (TranslationController.notationStatistics)

    println(TranslationController.articleStatistics.makeArticleStatistics)
    TranslationController.currentDoc
  }
}
