package info.kwarc.mmt.gf

import info.kwarc.mmt.api.archives.{BuildSuccess, BuildTask, Importer}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.utils.File


class GfImporter extends Importer {
  val key = "gf-omdoc"

  val inExts = List("gf")

  def importDocument(bt: BuildTask, index: Document => Unit) = {
    val s = File.read(bt.inFile)



    BuildSuccess(Nil, Nil)   // TODO: put right arguments
  }
}
