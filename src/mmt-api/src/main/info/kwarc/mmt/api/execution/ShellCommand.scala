package info.kwarc.mmt.api.execution

import info.kwarc.mmt.api._
import frontend._
import objects._
import parser._
import utils._

class ShellCommand extends ShellExtension("run") {
   def helpText = "mmt :run THEORY-URI PROGRAM-TERM"

   def run(shell: Shell, args: List[String]) = {
     val exec = controller.extman.get(classOf[Executor]).headOption.getOrElse {
       throw LocalError("no executor found")
     }
     report.addHandler(ConsoleHandler)
     report.groups += logPrefix
     report.groups += exec.logPrefix
     if (args.isEmpty)
       throw LocalError("MMT URI of constant expected")
     val nsMap = controller.getNamespaceMap
     val thy = Path.parseM(args.head, nsMap)
     // TODO this is an inefficient way to fill the mathpath: load everything we know
     controller.getConfig.getEntries(classOf[LMHConf]).foreach { e => controller.addArchive(e.local)}
     val con = Context(thy)
     val progS = args.tail.mkString(" ")
     val pu = ParsingUnit(SourceRef.anonymous(progS), con, progS, nsMap)
     val parser = controller.extman.get(classOf[Parser], "mmt").get
     val progP = parser(pu)(makeErrorThrower("ill-formed program")).toTerm
      val progC = checking.Solver.check(controller, Stack(con), progP) match {
       case Left((t,_)) => t
       case Right(s) =>
         log("invalid program; trying to execute unchecked program")
         progP
     }
     val result = exec(Context(thy), progC)
     println("\nprogram terminated with value: " + controller.presenter.asString(result))
     true
   }
}
