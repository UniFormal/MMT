import info.kwarc.mmt.api.SemanticObject
import info.kwarc.mmt.api.backend.{NotApplicable, RealizationStorage}
import info.kwarc.mmt.api.frontend.{Controller, Extension, REPLExtension}
import info.kwarc.mmt.api.frontend.actions.ActionResultError

import scala.io.StdIn
import scala.util.Try

/**
  * A usable MMT REPL you can start and use from within IntelliJ.
  *
  * Other means to access the MMT REPL inferior:
  *
  *  - "java -jar mmt.jar": requires the time-consuming building of a jar
  *  - "sbt runMain info.kwarc.mmt.api.frontend.Run" (SBT is slow anyway + SBT requires rebuilding even if IntelliJ
  *    has an up-to-date code cache.)
  *
  * Hence, I (Navid) exclusively use FastREPL from now on.
  */
object FastREPL extends MagicTest("debug") {
  val shortcuts: List[String] = List(
    "build MMT/LATIN2 mmt-omdoc algebra/diagop-test.mmt",
    "build MMT/LATIN2 mmt-omdoc algebra/diagop-theories.mmt",
    "build MMT/LATIN2 mmt-omdoc playground/comptrans.mmt",
    "-------------------------------------------",
    "build MMT/LATIN2 mmt-omdoc proving/atp.mmt",
    "build MMT/LATIN2 mmt-omdoc playground/tptp-exporter_monoid.mmt",
    "build MMT/urtheories mmt-omdoc module-expressions.mmt",
    "-------------------------------------------",
    "build MMT/LATIN2 mmt-omdoc logic/fol-diagop.mmt",
    "-------------------------------------------",
    "build MMT/urtheories mmt-omdoc module-expressions-logrel-test.mmt",
    "build MMT/urtheories mmt-omdoc module-expressions-logrel-pushout-test.mmt",
    "-------------------------------------------",
    "build MMT/urtheories mmt-omdoc",
    "build MMT/urtheories scala-bin",
    "build MitM/Foundation mmt-omdoc",
    "build MitM/core mmt-omdoc",
    "build FrameIT/frameworld mmt-omdoc",
    "build FrameIT/frameworld mmt-omdoc Scrolls",
    "-------------------------------------------",
    "build FrameIT/frameworld -mmt-omdoc",
  )

  private val repl = new FastREPLExtension(shortcuts)

  override def doFirst(): Unit = {
    super.doFirst()
    controller.extman.addExtension(repl)
  }

  override def run(): Unit = {
    repl.run()
    sys.exit(0)
  }
}

private class FastREPLExtension(shortcuts: List[String], runFirst: Option[String] = None) extends REPLExtension {
  override def run(): Unit = {
    val inputLines = runFirst.iterator ++ Iterator.continually(StdIn.readLine()).takeWhile(_ != null)
    inputLines.foreach(line => {
      handleLine(line)
      printQuery()
    })
  }
  override def exit(): Unit = {}

  private def printQuery(): Unit = {
    // print 1-based indices
    val str = shortcuts.zipWithIndex.map {
      case (shortcut, idx) => s"${idx + 1}\t$shortcut"
    }.mkString("\n")

    println()
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