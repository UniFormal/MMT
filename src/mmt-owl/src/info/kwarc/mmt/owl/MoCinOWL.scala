package info.kwarc.mmt.owl

import info.kwarc.mmt.api._
import frontend._
import moc._
import utils.URI

import scala.xml._


//take two owl files as inputs
//add identifiers to them
//translate them to mmt files
//apply mmt diff to the mmt files
//dependsOn
//propFunc
object MoCinOWL {
  def main(args: Array[String]) {
    val controllerOld = new Controller
    val controllerCurr = new Controller

    controllerOld.runMSLFile(new java.io.File("startup.mmt"), None)
    controllerCurr.runMSLFile(new java.io.File("startupCurrent.mmt"), None)

    //http://docs.omdoc.org/older.omdoc
    if (args.length < 1) {
      println("USAGE: DocumentPath")
      sys.exit
    }

    val dpath: DPath = new DPath(URI(args(0)))

    //we need the older version in the source folder to be able to get dependency relations.
    //two versions have the same ontology IRI, so they should not be in the source folder together,
    // because we cannot compare them
    //olderVersion has to be in the source folder
    //currentVersion should not be in the source folder; elsewhere

    //transates older and the current OWL documents to MMT
    controllerOld.handleLine("file build-test.mmt")
    controllerCurr.handleLine("file build-testCurrent.mmt")


    //read mmt documents
    //val olderDoc : DPath  = controllerOld.read()
    //val currentDoc : DPath = controllerCurr.read()

    //val mpath = new MPath(dpath,new LocalPath("_"))
    var diff = Differ.diff(controllerOld, controllerCurr, dpath, dpath)
    val pretty = new PrettyPrinter(150, 3)
    println(pretty.format(diff.toNode))
    //propagate(diff)


  }

}
