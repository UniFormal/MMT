package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import frontend._
import utils._
import parser._

import scala.collection.mutable.HashSet

/** a Compiler that uses the generic MMT functions */
class MMTCompiler extends Compiler {
  def isApplicable(src : String): Boolean = src == "mmt" || src == "elf" || src == "mmt-new"
     
  def compile(in: File, out: File) : List[SourceError] = {
    val dpath = DPath(FileURI(in))
    val source = scala.io.Source.fromFile(in.toJava, "UTF-8")
    val (doc, errorList) = in.getExtension match {
      case Some("elf") | Some("mmt") => 
        val (doc, errorList) = controller.textReader.readDocument(source, dpath)(controller.termParser.apply)
          source.close
          (doc, errorList)
      case Some("mmt-new") =>
        val r = parser.Reader(in)
        val (doc,ps) = controller.textParser(r, dpath)
        r.close
        (doc, ps.getErrors)
      case e => throw ImplementationError("compiler not applicable to extension: " + e)    
    }
    val outFile = out.setExtension("omdoc")
    outFile.toJava.getParentFile.mkdirs 
    File.write(outFile, doc.toNodeResolved(controller.localLookup).toString)
    errorList.toList
  }
}
