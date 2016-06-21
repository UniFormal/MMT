package info.kwarc.mmt.tps

import syntax._
import info.kwarc.mmt.api._
import documents._
import modules._
import utils._
import archives._ 

class TPSImporter extends Importer {
   val key = "tps-omdoc"
   def inExts = List("omdoc")
   //override def inDim = RedirectableDimension("pvsxml", Some(Dim("src","pvsxml")))

   private val parseXML = syntax.makeParser

   private var startAt = "/home/raupi/lmh/MathHub/PVS/Prelude/src/pvsxml/K_props"
   def importDocument(bf: BuildTask, index: Document => Unit) : BuildResult = {
 //     if (bf.inFile.filepath.toString < startAt) return
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

      val conv = new TPSImportTask(controller, bf, index)

      e match {
         case d: omdoc =>
            conv.doDocument(d)
      }


   }
}