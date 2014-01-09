package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import libraries._
import frontend._
import backend._
import modules._
import ontology._
import patterns._
import symbols._
import objects._
import utils._
import utils.FileConversion._

import scala.collection.mutable._

/** This trait adds validation operations to Archive's */
trait ValidatedArchive extends WritableArchive {
   /** checks modules in content structurally and generates term-level dependency relation in .occ files */
   def check(in: List[String] = Nil, controller: Controller) {
      val rels = new HashSet[RelationalElement]
      val checker = new StructureChecker(controller) {
         override def reCont(r : RelationalElement) = {
            rels += r
            controller.memory.ontology += r
         }
      }
      traverse(content, in, Archive.extensionIs("omdoc")) {case Current(_, inPath) =>
         rels.clear
         val mpath = Archive.ContentPathToMMTPath(inPath)
         val errors = checker(mpath)
         logGroup {
            errors foreach {e => log(e.getMessage)}
         }
         val relFile = (this/relational / inPath).setExtension("occ")
         val relFileHandle = File.Writer(relFile)
         rels foreach {r => relFileHandle.write(r.toPath + "\n")}
         relFileHandle.close
      }
    }
    
    /** checks modules in content structurally and then validates all ValidationUnits */
    def validate(in: List[String] = Nil, controller: Controller) {
      traverse(content, in, Archive.extensionIs("omdoc")) {case Current(_, inPath) =>
         val mpath = Archive.ContentPathToMMTPath(inPath)
         val errors = controller.checker(mpath)
         logGroup {
            errors foreach {e => log(e.getMessage)}
         }
      }
    }
}

