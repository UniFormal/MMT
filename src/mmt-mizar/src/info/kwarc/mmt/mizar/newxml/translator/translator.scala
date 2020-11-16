package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.{DPath, archives, documents}
import info.kwarc.mmt.mizar.mmtwrappers.Mizar
import info.kwarc.mmt.mizar.newxml.Main.makeParser
import info.kwarc.mmt.mizar.newxml.syntax.{Item, Reservation, Text_Proper}
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