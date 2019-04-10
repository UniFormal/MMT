
import info.kwarc.mmt.api.{GeneralError, Level}
import info.kwarc.mmt.api.archives.Update
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.utils.{EmptyList, File, FilePath}
import info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api.ontology.RelationalReader
import info.kwarc.mmt.api.archives.Archive

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents.{Document, NRef}
import info.kwarc.mmt.api.frontend.{Controller, Extension, FormatBasedExtension}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects.{OMID, OMMOD, OMS}
import info.kwarc.mmt.api.ontology.Declares._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.presentation.{HTMLPresenter, MMTDocExporter, MathMLPresenter, StructurePresenter}
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._


object Graphtester extends MagicTest("jgraph") {
  def run : Unit = {
    //val test = WebQuery("type=archivegraph&graphdata=MMT/urtheories&semantic=grounded")
    // println(test)
    //println( test("graphdata"))
    //val serve = new JSONBasedGraphServer()
  }
}

