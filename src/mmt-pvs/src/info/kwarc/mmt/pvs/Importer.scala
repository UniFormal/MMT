package info.kwarc.mmt.pvs

import syntax._

import info.kwarc.mmt.api._
import documents._
import modules._
import utils._
import archives._ 

class PVSImporter extends Importer {
   val key = "pvs-omdoc"
   def includeFile(n: String) : Boolean = n.endsWith(".xml")
   override def inDim = RedirectableDimension("pvsxml")
   
   private val parseXML = syntax.parser
   
   private var i = 0
   private val ignore = 2
   private val ignoreMsg = List("no class for +")
   private var startAt = "ctl"
   def importDocument(bf: BuildTask, index: Document => Unit) {
      if (bf.inFile.name < startAt) return
      val e = try {
         parseXML(bf.inFile)
      } catch {
         case utils.ExtractError(msg) =>
            //ParseError("error in xml: " + msg)
            if (ignoreMsg.exists(msg.startsWith))
               return
            i += 1
            if (i > ignore) {
               println(msg)
               sys.exit
               //throw utils.ExtractError(msg)
            } else
               return
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

class PVSImportTask(bf: BuildTask, index: Document => Unit) {
   
   def doDocument(d: pvs_file) {
      val modsM = d._modules map doModule
      val path = bf.narrationDPath
      val mrefsM = modsM.map(m => MRef(path, m.path))
      val doc = new Document(path, mrefsM)
      index(doc)
   }
   
   def doModule(m: syntax.Module): modules.Module = m match {
      case t: theory => null
   }
}