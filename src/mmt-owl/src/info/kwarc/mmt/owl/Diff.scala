package info.kwarc.mmt.owl

import java.io.File

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.moc._
import info.kwarc.mmt.api.utils.File._

import scala.xml._


object Diff {
  def main(args: Array[String]) {
    implicit val eh = ErrorThrower

    val firstController = new Controller
    val secondController = new Controller
    firstController.runMSLFile(new java.io.File("startup.mmt"), None)
    secondController.runMSLFile(new java.io.File("startup.mmt"), None)

    var currentVersion: File = null
    var olderVersion: File = null

    if (args.length < 1) {
      println("USAGE: info.kwarc.mmt.owl.Diff NEWFILE [OLDFILE]")
      sys.exit
    }

    currentVersion = new File(args(0))

    if (args.length < 2) {
      // get the older version from svn if it exists
      val currentName = currentVersion.getName() // NAME
      val currentDir = currentVersion.getParent() // DIR

      if (currentDir == null) {
        if (!new File(".svn")./("text-base")./(currentName + ".svn-base").exists()) {
          println("USAGE: info.kwarc.mmt.owl.Diff NEWFILE OLDFILE")
          sys.exit
        }
        else
        //.svn/text-base/Name.svn-base
          olderVersion = (new File(".svn"))./("text-base")./(currentName + ".svn-base")
      }
      else {
        if (!new File(currentDir)./(".svn")./("text-base")./(currentName + ".svn-base").exists()) {
          println("USAGE: info.kwarc.mmt.owl.Diff NEWFILE OLDFILE")
          sys.exit
        }
        else
        // DIR/.svn/text-base/NAME.svn-base
          olderVersion = ((new File(currentDir))./(".svn"))./("text-base")./(currentName + ".svn-base")
      }
    }
    else
      olderVersion = new File(args(1))

    val olderDoc: DPath = firstController.read(parser.ParsingStream.fromFile(olderVersion), interpret = false).path
    val currentDoc: DPath = secondController.read(parser.ParsingStream.fromFile(currentVersion), interpret = false).path

    var diff = Differ.diff(firstController, secondController, olderDoc, currentDoc)
    //    println(diff.toNode.toString())
    val pretty = new PrettyPrinter(150, 3)
    println(pretty.format(diff.toNode))

  }


}
