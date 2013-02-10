package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import utils._
import utils.FileConversion._

trait ScalaArchive extends WritableArchive {
    /** Extract scala from a dimension */
    def extractScala(in : List[String] = Nil, dim: String) {
        val inFile = root / dim / in
        if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) extractScala(in ::: List(n), dim)
           }
        } else if (inFile.getExtension == Some("omdoc")) {
           try {
              val controller = new Controller(report)
              val (doc,_) = controller.read(inFile, Some(DPath(narrationBase / in)))
              val outFile = (root / "scala" / in).setExtension("scala")
              outFile.getParentFile.mkdirs
              uom.Extractor.doDocument(controller, doc.path, outFile)
           } catch {
              case e: Error => report(e)
              //case e => report("error", e.getMessage)
           }
        }
    }
    
    /** Integrate scala into a dimension */
    def integrateScala(in : List[String] = Nil, dim: String) {
        val inFile = root / "scala" / in
        if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) integrateScala(in ::: List(n), dim)
           }
        } else if (inFile.getExtension == Some("omdoc")) {
           try {
              val controller = new Controller(report)
              val (doc,_) = controller.read(inFile, Some(DPath(narrationBase / in)))
              val scalaFile = (root / "scala" / in).setExtension("scala")
              uom.Synthesizer.doDocument(controller, doc.path, scalaFile)
              xml.writeFile(doc.toNodeResolved(controller.library), inFile)
           } catch {
              case e: Error => report(e)
              //case e => report("error", e.getMessage)
           }
        }
    }
}