package info.kwarc.mmt.tptp

import tptp._

import info.kwarc.mmt.api._
import documents._
import utils._
import frontend._
import archives._
import symbols._
import libraries._
import modules._
import objects._
import presentation._

/**
 * TPTP Compiler, translates TPTP sources to OMDoc
 */
// deprecated, probably obsolete
class TptpCompiler extends Importer with backend.QueryTransformer {
  val key = "tptp-omdoc"

  /** for now only fof files */
  def inExts = List("tptp")

  def importDocument(bf: archives.BuildTask, seCont: documents.Document => Unit) {
    val fileName = bf.inPath.segments.last
    // compute TPTP directory in which the input file is (e.g. Axioms/SET007/inputFile)
    val dirPath = bf.inPath.segments.init
    val AxProb = if (dirPath.contains("Axioms")) "Axioms" else "Problems"
    val fileDir = dirPath.drop(dirPath.lastIndexOf(AxProb))

    // translate the input file to OMDoc
    val translator = new TptpTranslator()
    val d = translator.translate(fileDir.mkString("/"), fileName, bf.inFile)
    seCont(d)
  }

	def transformSearchQuery(n: scala.xml.Node, params : List[String]) : List[scala.xml.Node] = {
	   val translator = new TptpTranslator()
	   val s = n match {
	      case scala.xml.Text(s) => s
	   }
      val translated = translator.translateFormula(s).get
      val query = (new backend.TPTP).prepareQuery(translated) // ArchiveCustomization for TPTP in mmt-api
      List(<mws:expr>{query}</mws:expr>)
   }
}
