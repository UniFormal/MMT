package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import documents.{Document, MRef}
import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.ontology.{Includes, Transitive}
import symbols.{Declaration, HasDefiniens, HasNotation, HasType}
import info.kwarc.mmt.mizar.newxml.Main.makeParser
import info.kwarc.mmt.mizar.newxml.syntax._
import info.kwarc.mmt.mizar.newxml.translator.TranslationController._


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
        case GetError(s) if s.startsWith("no backend applicable to "+outputBase.toString) =>
          val Array(dpath, name) = s.stripPrefix("no backend applicable to ").split('?')
          val mpath = DPath(utils.URI(dpath)) ? name
          println("GetError since we require the dependency theory "+mpath+" of the translated theory "+currentTheory.name+" to be already translated: \n"+
            "Please make sure the theory is translated (build with mizarxml-omdoc build target) and try again. ")
          articleData.addUnresolvedDependency(mpath)
        case e: Throwable =>
          if (printErr) println (TranslationController.showErrorInformation(e, " while translating the "+it._subitem.shortKind+". "))//+it.toString))
          if (printTrace && (e.isInstanceOf[GeneralError] || e.isInstanceOf[LookupError])) e.printStackTrace()
          errCounter += 1
      }
    }
  }
}

import subitemTranslator._
object itemTranslator {
  def add(d: Declaration with HasType with HasDefiniens with HasNotation)(implicit defContext: DefinitionContext): Unit =
    TranslationController.addDeclaration(TranslatorUtils.hiddenRefTranslator(d))(defContext)
  // Adds the corresponding content to the TranslationController
  def translateItem(item: Item): Unit = {
    implicit val defCtx: DefinitionContext = DefinitionContext.empty()
    //val translatedSubitem : List[info.kwarc.mmt.api.ContentElement] =
    item._subitem match {
      case defn: Definition => definitionTranslator.translate_Definition(defn)
      case scheme_Block_Item: Scheme_Block_Item => translate_Scheme_Block_Item(scheme_Block_Item) foreach add
      case theorem_Item: Theorem_Item => add (statementTranslator.translate_Theorem_Item(theorem_Item))
      case res: Reservation => translate_Reservation(res) foreach add
      case defIt: Definition_Item =>
        try {
          translate_Definition_Item(defIt)
        } catch {
          case e: Throwable =>
            throw e
        }
      case sectPragma: Section_Pragma => translate_Section_Pragma(sectPragma) foreach add
      case pr: Pragma => translate_Pragma(pr) foreach add
      case lociDecl: Loci_Declaration => throw new DeclarationLevelTranslationError("Unexpected Loci-Declaration on Top-Level.", lociDecl)
      case cl: Cluster => clusterTranslator.translate_Cluster(cl)(DefinitionContext.empty())
      case identify: Identify => add (translate_Identify(identify))
      case nym: Nyms => add (nymTranslator.translate_Nym(nym))
      case st: Statement with TopLevel =>
        val statement = statementTranslator.translate_Statement(st)
        statement map add
      case notTopLevel: DeclarationLevel => throw subitemTranslator.notToplevel
      case notTopLevel: ProofLevel => throw subitemTranslator.notToplevel
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
        +(if (articleDependencyParents.length > 1) "\nThe article at the end of the dependency chain is "+articleDependencyParents.last else ""))
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
      //printTimeDiff(parsingTime, "The parsing took ")
      globalParsingTime += parsingTime

      articleData.resetCurrenTranslatingTimeBegin
      val doc = translate(text_Proper, bf, processDependency(_, bf, index))
      articleData.addCurrenTranslatingTime
      //printTimeDiff(articleData.currentTranslatingTime, "The translation took ")
      articleData.addCurrentToGlobalTranslatingTime

      index(doc)
      //printTimeDiff(addingTime, "The adding took ")
      articleDependencyParents = articleDependencyParents.tail
      if (! getBuildArticles.contains(currentTheoryPath)) {
        globalTranslatedDeclsCount += articleData.articleStatistics.grandTotal
      }
      addBuildArticles (currentTheoryPath)
      if (articleDependencyParents.length == 0)
        println(articleData.articleStatistics.makeArticleStatistics)
      doc
    }
    val doc = if (!isBuild(currentAid)) {
      buildIt()
    } else {
      controller.getO(currentTheoryPath) flatMap(_.asInstanceOf[Theory].parentDoc) flatMap (controller.getO(_)) match {
        case Some(d: Document) => d
        case _ => buildIt()
      }
    }
    archives.BuildResult.fromImportedDocument(doc)
  }

  def translate(text_Proper: Text_Proper, bf:archives.BuildTask, processDependency: String => Unit) : Document = {
    ////shouldn't be necessary and proabably isn't (filenames and aids should always agree)
    //TranslationController.articleData.currentAid = text_Proper.articleid.toLowerCase().takeWhile(_ != '.')
    makeDocument()
    makeTheory()

    articleTranslator.translateArticle(text_Proper, processDependency, checkConstants = true, typecheckContent = false)
    endMake()

    val unres = articleData.getUnresolvedDependencies
    if (unres.nonEmpty && articleDependencyParents.length == 0) {println ("Unresolved dependencies so far: "+unres.map(_.name))}
    val deps = articleData.getDependencies
    if (deps.nonEmpty) {log ("Resolved dependencies: "+deps.map(_.name))}

    articleData.currentDoc
  }
}
