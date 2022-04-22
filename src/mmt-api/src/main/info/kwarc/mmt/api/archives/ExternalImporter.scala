package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import utils._
import parser._

/**
 * an importer implemented as a wrapper around an external tool
 */
abstract class ExternalImporter extends Importer {

  val toolName: String

  /** the wrapper method to implement */
  def runExternalTool(bf: BuildTask, outFile: File)

  /**
    * Compile an external file to OMDoc
    * @param bf the build task
    * @param seCont document continuation for indexing
    */
  def importDocument(bf: BuildTask, seCont: documents.Document => Unit): BuildResult = {
    val outFile = bf.archive / RedirectableDimension(key) / bf.inPath.toFile.setExtension(outExt).toFilePath
    outFile.up.mkdirs()
    outFile.delete()
    runExternalTool(bf, outFile)
    def error(msg: String) {
      val ref = SourceRef(FileURI(bf.inFile), parser.SourceRegion.none)
      val e = CompilerError(key, ref, List(msg), Level.Error)
      bf.errorCont(e)
    }
    if (!(outFile.exists && outFile.length > 0)) {
      error(s"unknown error: $toolName produced no omdoc file")
      BuildFailure(Nil, Nil)
    } else
      try {
        val dp = bf.narrationDPath
        val ps = ParsingStream.fromFile(outFile, Some(dp.copy(uri = dp.uri.setExtension("omdoc"))))
        val doc = controller.read(ps, interpret = false)(bf.errorCont)
        seCont(doc)
        val provided = doc.getModulesResolved(controller.globalLookup).map(_.path) map LogicalDependency
        val used = Nil //TODO how to get exact dependencies at this point?
             //TODO how to return MissingDependency?
        if (bf.errorCont.hasNewErrors)
          BuildFailure(used, provided)
        else
          BuildSuccess(used, provided)
      } catch {
        case e: scala.xml.parsing.FatalError =>
          error(s"XML error in omdoc file (maybe too big for $toolName to write)")
          BuildFailure(Nil, Nil)
      }
  }
}
