package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import documents._
import modules._
import symbols._
import objects._
import ontology._
import utils._
import utils.FileConversion._
import info.kwarc.mmt.api.parser.{ParsingUnit,DefaultObjectParser}

trait MMTArchive extends WritableArchive {
   /** parses and loads an archive */ 
   def readSource(in: List[String] = Nil, controller: Controller, deep: Boolean) {
       var docs : List[Document] = Nil
       if (! (root / "source").exists) return
       traverse("source", in, s => Archive.extensionIs("elf")(s) || Archive.extensionIs("mmt")(s)) {case Current(inFile, inPath) =>
          val (doc, errorList) = controller.read(inFile, Some(DPath(narrationBase / inPath)))
          docs ::= doc
          if (!errorList.isEmpty)
             log(errorList.size + " errors in " + inFile.toString + ": " + errorList.mkString("\n  ", "\n  ", ""))
       }
       if (deep) {
          val st = new StructureTraverser(controller)
          docs foreach {
             d => st(d) {case (cpath, tC) =>
               tC.read foreach {s =>
                  val pu = ParsingUnit(null, null, Context(), s)
                  val t = controller.termParser(pu)
                  tC.parsed = t
               }
             }
          }
       }
   }
}
