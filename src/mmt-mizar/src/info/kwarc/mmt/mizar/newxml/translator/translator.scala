package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.BuildResult
import info.kwarc.mmt.api.documents.{Document, DocumentLevel}
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.symbols.ContextContainer
import info.kwarc.mmt.api.{DPath, LocalName, NamespaceMap, archives, documents}
import info.kwarc.mmt.mizar.mmtwrappers.Mizar
import info.kwarc.mmt.mizar.newxml.Main.makeParser
import info.kwarc.mmt.mizar.newxml.syntax.{Item, Reservation, Text_Proper}
import info.kwarc.mmt.mizar.mmtwrappers.Mizar._
import info.kwarc.mmt.mizar.translator.TranslationController

object articleTranslator {
  def translateArticle(text_Proper: Text_Proper, dpath:DPath) = {
    val items = text_Proper._items map itemTranslator.translateItem
  }
}

object itemTranslator {
  // Adds the corresponding content to the TranslationController
  def translateItem(item: Item) = {
    item.checkKind()
    val translatedSubitem = item match {
      case res:Reservation =>  subitemTranslator.translateReservation(res)
      case _ =>
    }
  }
}

object subitemTranslator {
  def translateReservation(reservation: Reservation) = {}
}

class MizarImporter extends archives.Importer {
  val key = "mizarxml-omdoc"
  def inExts = List("esx")

  def importDocument(bf: archives.BuildTask, seCont: documents.Document => Unit): BuildResult = {
    val file = bf.inFile
    val parser = makeParser
    val parsedArticle = parser.apply(file).asInstanceOf[Text_Proper]
    val aid = parsedArticle.articleid
    val dpath = bf.narrationDPath.^! / (aid.toLowerCase() + ".omdoc")

    TranslationController.currentBase = getBase(file)
    TranslationController.currentAid = aid

    val doc = new Document(dpath, documents.ModuleLevel, None)
    TranslationController.add(doc)

    val th = new Theory(TranslationController.currentThyBase, TranslationController.localPath, Some(Mizar.MizarPatternsTh), Theory.noParams, Theory.noBase)
    TranslationController.add(th)

    articleTranslator.translateArticle(parsedArticle, dpath)
    log("INDEXING ARTICLE: " + bf.narrationDPath.last)
    seCont(doc)
    BuildResult.empty
  }

  def getBase(f: File): String = {
    f.toJava.getParentFile.getParent match {
      case null => "./"
      case s => s + "/"
    }
  }
}