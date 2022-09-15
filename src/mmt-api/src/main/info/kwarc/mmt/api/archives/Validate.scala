package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import checking._
import frontend._
import ontology._
import utils._

import scala.collection._

class ValidationTask extends MMTTask

/** This trait adds validation operations to Archive's */
trait Validate {self: Archive =>
  /** checks modules in content structurally and generates term-level dependency relation in .occ files */
  def check(in: FilePath = EmptyPath, controller: Controller): Unit = {
    val rels = new mutable.HashSet[RelationalElement]
    val relHandler = new RelationHandler {
      def apply(r: RelationalElement): Unit = {
        rels += r
        controller.memory.ontology += r
      }
    }
    val checker = new MMTStructureChecker(new NullChecker.Objects)
    checker.init(controller)
    traverse(content, in, Archive.traverseIf("omdoc")) { case Current(_, inPath) =>
      rels.clear()
      val mpath = Archive.ContentPathToMMTPath(inPath)
      checker(mpath)(new CheckingEnvironment(controller.simplifier, new ErrorLogger(report), relHandler, new ValidationTask))
      val relFile = (this / relational / inPath).setExtension("occ")
      val relFileHandle = File.Writer(relFile)
      rels foreach { r => relFileHandle.write(r.toPath + "\n") }
      relFileHandle.close()
    }
  }

  /** checks modules in content structurally and then validates all objects */
  def validate(in: FilePath = EmptyPath, controller: Controller): Unit = {
    traverse(content, in, Archive.traverseIf("omdoc")) { case Current(_, inPath) =>
      val mpath = Archive.ContentPathToMMTPath(inPath)
      controller.checkPath(mpath, "mmt")(new ValidationTask)
    }
  }
}

