package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.sql.{SQLBridge, Table}

object CodeGenerator {

  def main(args: Array[String]): Unit = {
    val controller = Controller.make(true, true, List())
    controller.addArchive(File("/Users/katja/KWARC/Archives/MMT/urtheories"))
    controller.addArchive(File("/Users/katja/KWARC/Archives/MMT/LFX"))
    controller.addArchive(File("/Users/katja/KWARC/Archives/ODK/DiscreteZOO"))
    val maybeTable: Option[Table] = SQLBridge.test2(controller, SQLBridge.example) match {
      case t: Table => Some(t)
      case _ => None
    }

    println(" - - - - - ")
    maybeTable.foreach(t => println(TableCode(t).jsonObjectProperties))
  }

}
