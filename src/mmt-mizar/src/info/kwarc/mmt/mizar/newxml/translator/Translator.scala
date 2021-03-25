package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import documents.Document
import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.ontology.{Includes, Transitive}
import symbols.{Declaration, HasDefiniens, HasNotation, HasType}
import info.kwarc.mmt.mizar.newxml.Main.makeParser
import info.kwarc.mmt.mizar.newxml.syntax._
import info.kwarc.mmt.mizar.newxml.translator.TranslationController._
import articleSpecificData._


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
        case GetError(s) if s.startsWith("no backend applicable to "+currentOutputBase.toString) =>
          val Array(dpath, name) = s.stripPrefix("no backend applicable to ").split('?')
          val mpath = DPath(utils.URI(dpath)) ? name
          println("GetError since we require the dependency theory "+mpath+" of the translated theory "+currentThy.name+" to be already translated: \n"+
            "Please make sure the theory is translated (build with mizarxml-omdoc build target) and try again. ")
          addUnresolvedDependency(mpath)
        case e: Throwable =>
          if (printErr) println (e.getMessage)
          if (printTrace) e.printStackTrace()
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
        add (statement)
      case notTopLevel: DeclarationLevel => throw subitemTranslator.notToplevel
      case notTopLevel: ProofLevel => throw subitemTranslator.notToplevel
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

    currentAid = bf.inFile.segments.last.toLowerCase().takeWhile(_ != '.')
    currentOutputBase = bf.narrationDPath.^!
    TranslationController.controller = controller

    def processDependency(dependencyAid: String): Unit = {
      lazy val currentData = articleSpecificData
      def getBf (aid: String): archives.BuildTask = {
        val oldAid = bf.inFile.segments.last.toLowerCase().takeWhile(_ != '.')
        val ext = bf.inFile.getExtension map ("."+_) getOrElse ""
        val inPath = utils.FilePath(List(aid+ext))
        val inPathFull = utils.FilePath(bf.inFile.toFilePath.segments.init :+ aid+ext)
        val outPath = bf.outFile.toFilePath.copy(segments = bf.outFile.toFilePath.segments.map(_.replace(oldAid, aid)))
        new BuildTask(bf.key, bf.archive, inPathFull.toFile, None, inPath, outPath.toFile, bf.errorCont)
      }
      if (! TranslationController.isBuild(dependencyAid)) {
        println("Building the dependency article "+dependencyAid+" before continuing with translation of the current article "+currentAid+". ")
        importDocument(getBf(dependencyAid), index)
        setData(currentData)
      }
    }
    val text_Proper = parser.apply(bf.inFile).asInstanceOf[Text_Proper]
    printTimeDiff(System.nanoTime() - startParsingTime, "The parsing took ")

    val startTranslationTime = System.nanoTime()

    val doc = translate(text_Proper, bf, processDependency(_))

    printTimeDiff(System.nanoTime() - startTranslationTime, "The translation took ")

    val startAddingTime = System.nanoTime()
    index(doc)
    printTimeDiff(System.nanoTime() - startParsingTime, "The adding took ")

    archives.BuildResult.fromImportedDocument(doc)
  }

  def translate(text_Proper: Text_Proper, bf:archives.BuildTask, processDependency: String => Unit) : Document = {
    ////shouldn't be necessary and proabably isn't (filenames and aids should always agree)
    //currentAid = text_Proper.articleid.toLowerCase().takeWhile(_ != '.')
    makeDocument()
    makeTheory()

    articleTranslator.translateArticle(text_Proper, processDependency, checkConstants = true, typecheckContent = true)
    endMake(Some(this.report))

    /*
    log("INDEXING ARTICLE: " + bf.narrationDPath.last)
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
    val unres = getUnresolvedDependencies()
    if (unres.nonEmpty) {println("Unresolved dependencies: "+unres.map(_.name))}
    val deps = articleSpecificData.getDependencies
    if (deps.nonEmpty) {println("Resolved dependencies: "+deps.map(_.name))}

    println(articleStatistics.makeArticleStatistics)
    currentDoc
  }
}
