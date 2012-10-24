package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import modules._
import symbols._
import objects._
import parser._
import ontology._
import utils._
import utils.FileConversion._

trait MMTArchive extends WritableArchive {
   /** parses and loads an archive */ 
   def readSource(in: List[String] = Nil, controller: Controller, deep: Boolean) {
       val parsingUnits = new scala.collection.mutable.HashMap[CPath,ParsingUnit]
       val tr = new TextReader(controller)
       if (! (root / "source").exists) return
       traverse("source", in, s => extensionIs("elf")(s) || extensionIs("mmt")(s)) {case Current(inFile, inPath) =>
          val source = scala.io.Source.fromFile(inFile, "UTF-8")
          val (doc, errorList) = tr.readDocument(source, DPath(narrationBase / inPath)) {pu =>
             parsingUnits(pu.component) = pu
             log("found parsing unit " + pu.component)
             DefaultParser(pu)
          }
          source.close
          if (!errorList.isEmpty)
             log(errorList.size + " errors in " + inFile.toString + ": " + errorList.mkString("\n  ", "\n  ", ""))
       }
       if (deep) {parsingUnits.values foreach {pu =>
          val cpath = pu.component
          val tm = controller.termParser(pu)
          val decl = controller.get(cpath.parent)
          decl match {
             case c: Constant =>
                log("parsing " + pu.component)
                val tp = if (cpath.component == TypeComponent) Some(tm) else c.tp
                val df = if (cpath.component == DefComponent) Some(tm) else c.df
                val cN = new Constant(c.home, c.name, tp, df, c.rl, c.not)
                controller.delete(c.path)
                controller.add(cN)
             case _ =>
          }
       }}
   }
}
