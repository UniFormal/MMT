package info.kwarc.mmt.tptp

import info.kwarc.mmt.api._
import archives._
import documents._
import utils._
import FileConversion._

class TPTPImporter extends TraversingBuildTarget {
   val key: String = "tptp-twelf"
   val inDim = source
   val outDim = Dim("twelf")
   def includeFile(name: String) =
      name.endsWith(".ax") || name.endsWith(".p") || name.endsWith(".tptp")

   /** command to run TPTP */
   private var tptpCommand: List[String] = null

   /**
    * expects command to run TPTP as arguments, e.g.,
    *
    * "C:\Program Files\swipl\bin\swipl.exe" -f C:/other/oaff/TPTP/Distribution/TPTP2X/tptp2x.main -g tptp2X('INFILE',none,lf,'OUTDIR'),halt.
    */
   override def start(args: List[String]) {
      if (args.isEmpty)
         throw LocalError("expected TPTP command")
      tptpCommand = args
   }

   private def escape(f: File) = f.toString.replace("\\","/")

   def buildFile(a: Archive, bt: BuildTask) {
      if (bt.inFile.toJava.length > 1000000) {
         bt.errorCont(LocalError("skipped big file: " + bt.inFile))
         return
      }
      val outFile = bt.outFile.setExtension("elf")
      val command = tptpCommand.map {s =>
         s.replace("INFILE", escape(bt.inFile)).replace("OUTDIR", escape(outFile.up))
      }
      log(command.mkString("  "))
      val result = ShellCommand.run(command :_*)
      result foreach {s =>
         bt.errorCont(LocalError(s))
         return
      }
      // outFile wraps module header/footer around non-modular tempFile
      val tempFile = outFile.setExtension("temp")
      outFile.renameTo(tempFile)
      val outWriter = File.Writer(outFile)
      val prefix = s"""%namespace "${a.narrationBase}".
%namespace tptp = "http://latin.omdoc.org/logics/tptp".

%sig ${bt.inFile.removeExtension.name} = {
   %meta tptp.THF.
"""
      outWriter.write(prefix)
      File.ReadLineWise(tempFile) {l =>
         outWriter.println(l)
      }
      outWriter.println("}.")
      outWriter.close
      tempFile.delete
   }
}
