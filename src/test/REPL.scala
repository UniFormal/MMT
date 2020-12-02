import info.kwarc.mmt.api.frontend.actions.ActionResultError
import info.kwarc.mmt.api.frontend.{REPLExtension, StandardREPL}
import info.kwarc.mmt.api.{DPath, Path}
import info.kwarc.mmt.api.utils.URI

import scala.io.StdIn
import scala.util.Try

/**
  * A usable MMT REPL you can start from within IntelliJ.
  *
  * Other means to access the MMT REPL are
  *
  *  - "java -jar mmt.jar": requires the time-consuming building of a jar
  *  - "sbt runMain info.kwarc.mmt.api.frontend.Run" (SBT is slow anyway + SBT requires rebuilding even if IntelliJ
  *    has an up-to-date code cache.)
  *
  * Hence, Navid recommends to exclusively use this REPL.
  */
object REPL extends MagicTest("debug") {
  private val shortcuts = List(
    "build MMT/urtheories mmt-omdoc",
    "build MitM/Foundation mmt-omdoc",
    "build MitM/core mmt-omdoc",
    "build FrameIT/frameworld mmt-omdoc",
    "build FrameIT/frameworld mmt-omdoc Scrolls",
    "-------------------------------------------",
    "build FrameIT/frameworld -mmt-omdoc",
  )

  override def doFirst: Unit = {
    super.doFirst
    val repl = new FastREPL(shortcuts)
    controller.extman.addExtension(repl)

    repl.run

    log("REPL started. You may want to run:")
  }

  override def run(): Unit = {}
}

private class FastREPL(shortcuts: List[String]) extends REPLExtension {
  override def run {
    var line: String = null

    printQuery()
    while ({ line = StdIn.readLine(); line != null }) {
      handleLine(line)
      printQuery()
    }
  }
  override def exit {}

  private def printQuery(): Unit = {
    // print 1-based indices
    val str = shortcuts.zipWithIndex.map {
      case (shortcut, idx) => s"${idx + 1}\t$shortcut"
    }.mkString("\n")

    println(str)
    println("…or enter custom command")
    print(">")
  }

  private def handleLine(line: String): Unit = {
    (Try(line.toInt).toOption match {
      case Some(index) if index >= 1 && index <= shortcuts.size => // accept 1-based indices
        Some(controller.tryHandleLine(shortcuts(index - 1)))

      case Some(_) =>
        log("index out of bounds")
        None

      case None =>
        Some(controller.tryHandleLine(line))
    }).collect {
      case a: ActionResultError => log(a.error)
      case _ =>
    }
  }
}