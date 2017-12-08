package info.kwarc.mmt.owl

import java.io.File

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.moc._

import scala.xml._

object FirstTry {
  def main(args: Array[String]) {
    implicit val eh = ErrorThrower

    val firstController = new Controller
    val secondController = new Controller

    firstController.runMSLFile(new java.io.File("startup.mmt"), None)
    secondController.runMSLFile(new java.io.File("startup.mmt"), None)

    val older: File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\Test\\compiled\\try.omdoc")
    val current: File = new File("E:\\Fall10\\CompSem\\Project\\MMT\\src\\mmt-owl\\try.omdoc")

    val olderDoc: DPath = firstController.read(parser.ParsingStream.fromFile(older), interpret = false).path
    val currentDoc: DPath = secondController.read(parser.ParsingStream.fromFile(current), interpret = false).path

    var diff = Differ.diff(firstController, secondController, olderDoc, currentDoc)
    val pretty = new PrettyPrinter(150, 3)
    println(pretty.format(diff.toNode))

    //translate strict diff to pragmatic diff for renaming

    val addImpacts = new AddImpacts(firstController.memory)
    val pdiff = addImpacts.propagate(diff)
    println(pretty.format(pdiff.toNode))

    //Patcher.patch(addImpacts.propagate(diff), secondController.memory)
    Patcher.patch(pdiff, secondController)

    val doc = secondController.getDocument(currentDoc).toNodeResolved(secondController.library)
    //println(doc.toString)

  }
}
