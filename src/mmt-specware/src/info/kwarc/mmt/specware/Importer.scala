package info.kwarc.mmt.specware

import errors._

import info.kwarc.mmt.api._
import parser._
import documents._
import utils._
import archives._ 

/**
 * imports Specware files
 *
 * The import is rather inefficient, not using Specware's recursive export facility
 * 
 * It currently only works on Windows because the specware-xmlprint.cmd script has not been ported to Unix.
 * That should be trivial though.
 * 
 */
/*
 * Specware already produces omdoc and err files. These are somewhat awkwardly, reread and indexed.
 * However, that allows fixing them as the generated OMDoc might not always be correct.
 * In particular, names do not always reference the URI.
 * 
 */
class SpecwareImporter extends Importer {
   val key = "specware-omdoc"
   def inExts = List("sw")
   
   private var swdir: File = null

   /** parses the XML file of Specware errors */
   private val ErrorParser = new XMLToScala("info.kwarc.mmt.specware.errors")

   /** wraps around the Specware executable to compile one file */
   private class SwCommand(arch: Archive, inFile: File) {
      val workDir = swdir / "Applications" / "Specware" / "bin" / "windows"
      val inUId = FileURI(inFile).pathAsString
      val physRoot = FileURI(arch / source)
      val command = List(workDir / "specware-local-xmlprint.cmd", physRoot, arch.narrationBase).map(_.toString) ::: List(inUId, "nil")
      def run = ShellCommand.runIn(workDir, command:_*)
      private val baseFile = inFile.up / "omdoc" / inFile.name
      val outFile = baseFile.setExtension("omdoc")
      val errFile = baseFile.setExtension("sw.err")
      def getErrors = ErrorParser(errFile).asInstanceOf[errors]._children
   }

   /** one argument: the location of the Specware directory */
   override def start(args: List[String]) {
      val p = getFromFirstArgOrEnvvar(args, "Specware")
      swdir = File(p)
   }
   
   def importDocument(bt: BuildTask, index: Document => Unit) {
      val swC = new SwCommand(bt.archive, bt.inFile)
      log(swC.command.mkString(" "))
      val result = swC.run
      result foreach {r =>
         throw LocalError("failed to run Specware: " + r)
      }
      swC.getErrors foreach {e => bt.errorCont(SourceError(key, e.getSourceRef, e.shortMsg))}
      
      val dp = bt.narrationDPath
      val ps = ParsingStream.fromFile(swC.outFile, Some(dp.copy(uri = dp.uri.setExtension("omdoc"))))
      val doc = controller.read(ps, false)(bt.errorCont)
      index(doc)
   }
}