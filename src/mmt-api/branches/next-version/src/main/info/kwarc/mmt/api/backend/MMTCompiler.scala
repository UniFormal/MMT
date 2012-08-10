package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import frontend._
import utils._

/** a Compiler that uses the generic MMT functions */
class MMTCompiler extends Compiler {
   def isApplicable(src : String): Boolean = src == "mmt"

   def compile(in: File, out: File) : List[SourceError] = {
      val dpath = DPath(FileURI(in))
      val source = scala.io.Source.fromFile(in.toJava, "UTF-8")
      val tr = new TextReader(controller)
      val (doc, errorList) = tr.readDocument(source, dpath, "mmt")
      source.close
      val outFile = out.setExtension("omdoc")
      outFile.toJava.getParentFile.mkdirs 
      File.write(outFile, doc.toNodeResolved(controller.localLookup).toString)
      errorList.toList
   }
}
