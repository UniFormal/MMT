package info.kwarc.mmt.tptp

import tptp._

import info.kwarc.mmt.api._
import documents._
import utils._
import frontend._
import backend._
import symbols._
import libraries._
import modules._
import objects._
import presentation._

/**
 * TPTP Compiler, translates TPTP sources to OMDoc
 */
class TptpCompiler extends Compiler with QueryTransformer {
  
  def isApplicable(src : String) : Boolean = src == "tptp"

  override def compile(in : File, dpath: Option[DPath], out : File) : List[SourceError] = {
    var errors: List[SourceError] = Nil
    val fileName = in.toJava.getName
    if (!fileName.contains(TptpUtils.FORM))
      return errors
    val path = in.toJava.getPath
    
    // compute TPTP directory in which the input file is (e.g. Axioms/SET007/inputFile)
    var fileDir = ""
    if (path.contains("Axioms"))
      fileDir = path.substring(path.lastIndexOf("Axioms"), path.lastIndexOf("/"))
    else
      fileDir = path.substring(path.lastIndexOf("Problems"), path.lastIndexOf("/"))
    val dir = out.toJava.getParentFile
    if (!dir.exists)
      dir.mkdirs
    
    // translate the input file to OMDoc
    val translator = new TptpTranslator()
    translator.translate(fileDir, fileName, in)
    
    // write to output file
    write(out.setExtension("omdoc"), fileDir, fileName)
    
    errors
  }
  
	def write(out: File, fileDir: String, name: String) {
		val docPath = out.toJava.getPath
		val base = TptpUtils.baseURI / fileDir
		val pp = new scala.xml.PrettyPrinter(100,2)
		val th = TptpTranslator.controller.get(new DPath(base) ? name)
		val fw = new java.io.FileWriter(docPath)
		
		val nd : scala.xml.Node = 
		<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath" base={base.toString}>
			{th.toNode}
		</omdoc>
		val docNode = pp.format(nd)
		fw.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + docNode.toString)
		fw.close
	}
	
	def transformSearchQuery(n: scala.xml.Node, params : List[String]) : List[scala.xml.Node] = {
	   val translator = new TptpTranslator()
	   val s = n match {
	      case scala.xml.Text(s) => s
	   }
      val translated = translator.translateFormula(s).get
      val query = (new TPTP).prepareQuery(translated) // ArchiveCustomization for TPTP in mmt-api
      List(<mws:expr>{query}</mws:expr>)
   }
}
