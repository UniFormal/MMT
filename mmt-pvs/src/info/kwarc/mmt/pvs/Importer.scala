package info.kwarc.mmt.pvs

import syntax._

import info.kwarc.mmt.api._
import documents._
import modules._
import utils._
import archives._

class PVSImporter extends Importer {
   val key = "pvs-omdoc"
   def inExts = List("xml")
   //override def inDim = RedirectableDimension("pvsxml", Some(Dim("src","pvsxml")))

   private val parseXML = syntax.makeParser

   private var startAt = "/home/raupi/lmh/MathHub/PVS/NASA/source/pvsxml/stdtokenizer"
   def importDocument(bf: BuildTask, index: Document => Unit): BuildResult = {
   //   if (bf.inFile.toFilePath.toString < startAt) return BuildResult.empty
      val d = bf.inFile.name
      val e = try {
         parseXML(bf.inFile)
      } catch {
         case utils.ExtractError(msg) =>
            //ParseError("error in xml: " + msg)
/*            if (ignoreMsg.exists(msg.startsWith))
               return
            i += 1
            if (i > ignore) { */
               println(msg)
               sys.exit
               //throw utils.ExtractError(msg)
//            } else
  //             return
      }
     //println(e)

      val conv = new PVSImportTask(controller, bf, index)
      e match {
         case d: pvs_file =>
            conv.doDocument(d)
         case m: syntax.Module =>
            conv.doDocument(pvs_file(List(m)))
            //conv.doModule(m)
      }
      BuildResult.empty
   }
}
