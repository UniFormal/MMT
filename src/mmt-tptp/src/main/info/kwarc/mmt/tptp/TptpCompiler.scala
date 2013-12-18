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
class TptpCompiler extends Importer with backend.QueryTransformer {
  val key = "tptp-omdoc"  

  def includeFile(n: String) : Boolean = n.endsWith(".tptp") && n.contains(TptpUtils.FORM)
  def buildOne(bf: archives.BuildFile, seCont: documents.Document => Unit) {
    val fileName = bf.inFile.toJava.getName
    val path = bf.inFile.toJava.getPath
    
    // compute TPTP directory in which the input file is (e.g. Axioms/SET007/inputFile)
    var fileDir = ""
    if (path.contains("Axioms"))
      fileDir = path.substring(path.lastIndexOf("Axioms"), path.lastIndexOf("/"))
    else
      fileDir = path.substring(path.lastIndexOf("Problems"), path.lastIndexOf("/"))
    val dir = bf.outFile.toJava.getParentFile
    if (!dir.exists)
      dir.mkdirs
    
    // translate the input file to OMDoc
    val translator = new TptpTranslator()
    val d = translator.translate(fileDir, fileName, bf.inFile)
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
