package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import utils._
import archives._


import scala.tools.nsc._
import scala.tools.nsc.Settings

/**
 * add
 * import (per archive)
 * export (per archive, e.g. html vs planetary)
 * serve  per MMT instance
 *
 *
 * mmt load extension
 * mmt add mathpath
 *
 * MBTArchive
 *   location
 *   import : List[BuildTarget]
 *   export : List[BuildTarget]
 */

class MBTArchive(controller : Controller) {
  def add(location : String) = controller.handle(AddArchive(File(location)))
}

class MBTExtension(controller : Controller, intp : interpreter.IMain) {
  def apply(cls : String, args : List[String] = Nil) = {
    //adding extension to controller
    val ext = controller.extman.addExtension(cls, args)
    ext match {
      case bt : BuildTarget =>
        val str = s"""val ${escapeName(bt.key)} = new MBTBuildTarget("${bt.key}", controller)"""
        intp.interpret(str)
      case _ => //nothing to do
    }
  }


  def escapeName(s : String) = {
    s.replaceAll("-", "_")
  }

}

class MBTBuildTarget(key : String, controller : Controller) {
  def build(ids : List[String], mod : BuildTargetModifier ) : Unit = {
   controller.handle(ArchiveBuild(ids, key, mod))
  }

  def parseArchIds(s : String) : List[String] = {
    if (s.charAt(0) == '[' && s.charAt(s.length-1) == ']') {
      utils.stringToList(s.substring(1, s.length-1), ",")
    } else {
      List(s)
    }
  }

  def build(s : String) : Unit = build(parseArchIds(s), Build)
  def update(s : String) : Unit = build(parseArchIds(s), new Update(true))
  def clean(s : String) : Unit = build(parseArchIds(s), Clean)


}

/**
 * MMTShell is a REPL for MMT based on the scala REPL.
 * Its currently experimental might later merge with or replace MMTILoop
 */
class MMTREPL(controller: Controller) extends interpreter.ILoop {
  override def createInterpreter {
     super.createInterpreter
     init
  }

  def init {
//    intp beQuietDuring {
      intp.bind("intp", intp)
      intp.interpret("import info.kwarc.mmt.api._")
      intp.interpret("import info.kwarc.mmt.api.frontend._")
      //for some reason bind fails so I use this instead
      intp.interpret("val controller = new Controller()")
      intp.interpret("val archive = new MBTArchive(controller)")
      intp.interpret("val extension = new MBTExtension(controller, intp)")
      println(intp, this)
      intp.interpret(s"""extension("info.kwarc.mmt.stex.STeXImporter") """)
      intp.bind("cont", controller)
      println(intp, this)

      println("$$$$$$$$$$")
//    }
  }

  override def closeInterpreter {
    //super.closeInterpreter()
    println("NOW")
  }
}
object MMTShell {
  val controller = new Controller()
  val interp = new MMTREPL(controller)
  val settings = new Settings()
  settings.usejavacp.value = true
  settings.Yreplsync.value = true

  def main(args : Array[String]) {
    interp.process(settings)
  }
}
