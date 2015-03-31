package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import documents._
import utils._
import archives._ 

class PVSImporter extends Importer {
   val key = "pvs-omdoc"
   def includeFile(n: String) : Boolean = n.endsWith(".xml")
   _inDim = Dim("xml")
   
   private val parseXML = new XMLToScala("info.kwarc.mmt.pvs.syntax")
   
   def importDocument(bf: BuildTask, seCont: Document => Unit) {
      val t = parseXML(bf.inFile)
      println(t)
   }
}