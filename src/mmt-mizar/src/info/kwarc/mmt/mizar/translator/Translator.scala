package info.kwarc.mmt.mizar.translator

import info.kwarc.mmt.api._
import documents.{Document, MRef}
import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.ontology.{Includes, Transitive}
import symbols.{Declaration, HasDefiniens, HasNotation, HasType}
import info.kwarc.mmt.mizar.Main.makeParser
import info.kwarc.mmt.mizar.syntax._
import info.kwarc.mmt.mizar.translator.TranslationController._


object articleTranslator {
  def translateArticle(text_Proper: Text_Proper, processDependency: String => Unit, checkConstants: Boolean = true, typecheckContent: Boolean = true): Unit = {
    setCheckConstants(checkConstants, typecheckContent)
    setProcessDependency (processDependency)
    var errCounter = 0
    def printTrace = errCounter < 5
    def printErr = errCounter < 30
    text_Proper._items foreach { it =>
      try {
        itemTranslator.translateItem(it)
      } catch {
        case implEr: ImplementationError =>
          println ("ImplementationError while translating the "+it._subitem.shortKind+". " )
          throw implEr
        case e: Throwable if (!e.isInstanceOf[ImplementationError]) =>
          if (printErr) println (TranslationController.showErrorInformation(e, " while translating the "+it._subitem.shortKind+": \n"+it._subitem.toString))
          if (printTrace) e.printStackTrace()
          errCounter += 1
      }
    }
  }
}

import subitemTranslator._
object itemTranslator {
  def add(d: Declaration with HasType with HasDefiniens with HasNotation, sourceRegion: Option[parser.SourceRegion])(implicit defContext: DefinitionContext): Unit =
    TranslationController.addDeclaration(TranslatorUtils.hiddenRefTranslator(d), sourceRegion)(defContext)
  // Adds the corresponding content to the TranslationController
  def translateItem(item: Item): Unit = {
    implicit val defCtx: DefinitionContext = DefinitionContext.empty()
    def addItem(d: Declaration with HasType with HasDefiniens with HasNotation): Unit =
      TranslationController.addDeclaration(TranslatorUtils.hiddenRefTranslator(d), Some(item.pos.sourceRegion()))
    item._subitem match {
      case defn: Definition => definitionTranslator.translate_Definition(defn, Some(item.pos.sourceRegion()))
      case scheme_Block_Item: Scheme_Block_Item => translate_Scheme_Block_Item(scheme_Block_Item) foreach addItem
      case theorem_Item: Theorem_Item => addItem (statementTranslator.translate_Theorem_Item(theorem_Item))
      case res: Reservation => translate_Reservation(res) foreach addItem
      case defIt: Definition_Item => translate_Definition_Item(defIt)
      case sectPragma: Section_Pragma => translate_Section_Pragma(sectPragma) foreach addItem
      case pr: Pragma => translate_Pragma(pr) foreach addItem
      case cl: Cluster => clusterTranslator.translate_Cluster(cl)(DefinitionContext.empty())
      case identify: Identify => addItem (translate_Identify(identify))
      case nym: Nyms => addItem (nymTranslator.translate_Nym(nym))
      case st: Statement with TopLevel => statementTranslator.translate_Statement(st) foreach addItem
      case notTopLevel  => if (! notTopLevel.isInstanceOf[TopLevel]) throw subitemTranslator.notToplevel(Some(notTopLevel.shortKind)) else throw ImplementationError("This is impossible.")
    }
  }
}

import TranslationController._
class MizarXMLImporter extends archives.Importer {
  val key = "mizarxml-omdoc"
  def inExts = List("esx")

  def processDependency(dependencyAid: String, bf: archives.BuildTask, index: documents.Document => Unit): Unit = {
    def getBf (aid: String): archives.BuildTask = {
      val oldAid = bf.inFile.segments.last.toLowerCase().takeWhile(_ != '.')
      val ext = bf.inFile.getExtension map ("."+_) getOrElse ""
      val inPath = utils.FilePath(List(aid+ext))
      val inPathFull = utils.FilePath(bf.inFile.toFilePath.segments.init :+ aid+ext)
      val outPath = bf.outFile.toFilePath.copy(segments = bf.outFile.toFilePath.segments.map(_.replace(oldAid, aid)))
      new BuildTask(bf.key, bf.archive, inPathFull.toFile, None, inPath, outPath.toFile, bf.errorCont)
    }
    if (! TranslationController.isBuild(dependencyAid)) {
      println("Building the dependency article "+dependencyAid+" before continuing with translation of the current article "+currentAid+". "
        +(if (articleDependencyParents.length > 1) "\nThe article at the end of the dependency chain is "+articleDependencyParents.last else "")
      )
      val currentData = getArticleData
      resetArticleData
      importDocument(getBf(dependencyAid), index)
      addBuildArticles (currentTheoryPath)
      setArticleData(currentData)
    }
  }
  def importDocument(bf: archives.BuildTask, index: documents.Document => Unit): archives.BuildResult = {
    articleData.currentAid = bf.inFile.segments.last.toLowerCase().takeWhile(_ != '.')
    outputBase = bf.narrationDPath.^!
    TranslationController.controller = controller
    setReport(this.report)
    def buildIt(): Document = {
      articleDependencyParents ::= currentTheoryPath
      val startParsingTime = System.nanoTime()
      val parser = makeParser
      val text_Proper = parser.apply(bf.inFile).asInstanceOf[Text_Proper]
      val parsingTime = timeSince(startParsingTime)
      globalParsingTime += parsingTime

      articleData.resetCurrenTranslatingTimeBegin
      val doc = translate(text_Proper, bf, processDependency(_, bf, index))
      articleData.addCurrentToGlobalTranslatingTime

      index(doc)
      articleDependencyParents = articleDependencyParents.tail
      if (! getBuildArticles.contains(currentTheoryPath)) {
        globalTranslatedDeclsCount += articleData.articleStatistics.grandTotal
      }
      addBuildArticles (currentTheoryPath)
      if (articleDependencyParents.length == 0)
        println(articleData.articleStatistics.makeArticleStatistics)
      doc
    }
    val doc = if (!(isBuild(currentAid) || currentAid == "hidden")) {
      buildIt()
    } else {
      (try {controller.getO(currentTheoryPath) flatMap(_.asInstanceOf[Theory].parentDoc) flatMap (controller.getO(_))} catch {case _: Throwable => None}) match {
        case Some(d: Document) => d
        case _ => buildIt()
      }
    }
    archives.BuildResult.fromImportedDocument(doc)
  }

  def translate(text_Proper: Text_Proper, bf:archives.BuildTask, processDependency: String => Unit) : Document = {
    makeDocument()
    makeTheory()

    articleTranslator.translateArticle(text_Proper, processDependency, typecheckContent = true)
    endMake()

    val deps = articleData.getDependencies
    if (deps.nonEmpty) {log ("Resolved dependencies: "+deps.map(_.name))}

    articleData.currentDoc
  }
}
