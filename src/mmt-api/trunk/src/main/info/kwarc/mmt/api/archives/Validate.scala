package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import checking._
import frontend._
import ontology._
import utils._

import scala.collection._

/** This trait adds validation operations to Archive's */
trait Validate {self: Archive =>
  /** checks modules in content structurally and generates term-level dependency relation in .occ files */
  def check(in: FilePath = EmptyPath, controller: Controller) {
    val rels = new mutable.HashSet[RelationalElement]
    val relHandler = new RelationHandler {
      def apply(r: RelationalElement) {
        rels += r
        controller.memory.ontology += r
      }
    }
    val checker = new MMTStructureChecker(new NullChecker.Objects)
    checker.init(controller)
    traverse(content, in, Archive.traverseIf("omdoc")) { case Current(_, inPath) =>
      rels.clear
      val mpath = Archive.ContentPathToMMTPath(inPath)
      checker(mpath)(new CheckingEnvironment(new ErrorLogger(report), relHandler))
      val relFile = (this / relational / inPath).setExtension("occ")
      val relFileHandle = File.Writer(relFile)
      rels foreach { r => relFileHandle.write(r.toPath + "\n") }
      relFileHandle.close()
    }
  }

  /** checks modules in content structurally and then validates all objects */
  def validate(in: FilePath = EmptyPath, controller: Controller) {
    traverse(content, in, Archive.traverseIf("omdoc")) { case Current(_, inPath) =>
      val mpath = Archive.ContentPathToMMTPath(inPath)
      controller.checkAction(mpath, "mmt")
    }
  }
}

