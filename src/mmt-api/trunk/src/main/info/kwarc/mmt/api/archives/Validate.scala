package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils._

import scala.collection.mutable._

/** This trait adds validation operations to Archive's */
trait ValidatedArchive extends WritableArchive {
  /** checks modules in content structurally and generates term-level dependency relation in .occ files */
  def check(in: List[String] = Nil, controller: Controller) {
    val rels = new HashSet[RelationalElement]
    val relHandler = new RelationHandler {
      def apply(r: RelationalElement) {
        rels += r
        controller.memory.ontology += r
      }
    }
    val checker = new MMTStructureChecker(NullChecker.objects)
    checker.init(controller)
    traverse(content, in, Archive.traverseIf("omdoc")) { case Current(_, inPath) =>
      rels.clear
      val mpath = Archive.ContentPathToMMTPath(inPath)
      checker(mpath)(new CheckingEnvironment(new ErrorLogger(report), relHandler))
      val relFile = (this / relational / inPath).setExtension("occ")
      val relFileHandle = File.Writer(relFile)
      rels foreach { r => relFileHandle.write(r.toPath + "\n") }
      relFileHandle.close
    }
  }

  /** checks modules in content structurally and then validates all objects */
  def validate(in: List[String] = Nil, controller: Controller) {
    traverse(content, in, Archive.traverseIf("omdoc")) { case Current(_, inPath) =>
      val mpath = Archive.ContentPathToMMTPath(inPath)
      controller.handle(Check(mpath, "mmt"))
    }
  }
}

