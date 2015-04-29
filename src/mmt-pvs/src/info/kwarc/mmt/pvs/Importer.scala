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
   override def inDim = RedirectableDimension("pvsxml")
   
   private val parseXML = syntax.makeParser
   
   private var i = 0
   private val ignore = 0
   private val ignoreMsg = Nil //List("no class for +")
   private var startAt = "finite_sets"
   def importDocument(bf: BuildTask, index: Document => Unit) {
      if (bf.inFile.name < startAt) return
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
      println(e)
      val conv = new PVSImportTask(bf, index)
      e match {
         case d: pvs_file =>
            conv.doDocument(d)
         case m: syntax.Module =>
            conv.doModule(m)
      }
   }
}