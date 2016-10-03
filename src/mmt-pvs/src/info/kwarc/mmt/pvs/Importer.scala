package info.kwarc.mmt.pvs

import syntax._
import info.kwarc.mmt.api._
import documents._
import archives._

class PVSImporter extends Importer {
   val key = "pvs-omdoc"
   def inExts = List("xml")
   //override def inDim = RedirectableDimension("pvsxml", Some(Dim("src","pvsxml")))

   private val parseXML = syntax.makeParser

//   private var startAt = "/home/raupi/lmh/MathHub/PVS/NASA/source/vect_analysis/pvsxml/cont_real_vect2.xml"
   private var startAt = "/home/raupi/lmh/MathHub/PVS/Prelude/src/pvsxml/finite_sequences.xml"
   def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
      if (bf.inFile.toFilePath.toString < startAt) return BuildResult.empty
      log("Reading " + bf.inFile)
      val e = try {
         parseXML(bf.inFile)
      } catch {
        case e : utils.ExtractError =>
          println(e.msg)
          sys.exit
      }
     //println(e)

      val conv = new PVSImportTask(controller, bf, index)
      e match {
         case d: pvs_file =>
            conv.doDocument(d)
         case m: syntax.Module =>
            conv.doDocument(pvs_file(List(m)))
      }


      //BuildResult.empty
   }
}
