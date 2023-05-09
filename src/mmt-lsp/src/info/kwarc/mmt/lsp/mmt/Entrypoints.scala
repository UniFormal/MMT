package info.kwarc.mmt.lsp.mmt

import info.kwarc.mmt.api.archives.{BuildManager, TrivialBuildManager}
import info.kwarc.mmt.api.frontend.Run
import info.kwarc.mmt.api.utils.File

object Socket {
  def main(args: Array[String]) : Unit = {
    val lsp = new MMTLSP
    val controller = Run.controller
    List("lsp", "lsp-mmt", "lsp-mmt-socket")
      .foreach(s => controller.handleLine(s"log+ $s"))
    controller.handleLine("log console")
    controller.handleLine("server on 8090")
    controller.extman.addExtension(lsp)
    controller.extman.get(classOf[BuildManager]).foreach(controller.extman.removeExtension)
    controller.extman.addExtension(new TrivialBuildManager)
    lsp.runSocketListener
  }
}

object Main {

  @throws[InterruptedException]
  def main(args: Array[String]): Unit = {
    val controller = Run.controller
    sys.env.get("HOME") match {
      case Some(f) =>
        val logf = File(f) / ".mmt" / "log"
        logf.mkdirs()
        controller.handleLine(s"log html ${logf.toString}/lsp-mmt-log.html")
        controller.handleLine("log+ lsp")
        controller.handleLine("log+ lsp-mmt")
        controller.handleLine("log+ lsp-mmt-server")
        controller.handleLine("log+ lsp-mmt-server-methodcall")
      case _ =>
    }

    val DEFAULT_PORT = 5007
    val port = args.headOption.flatMap(_.toIntOption) match {
      case Some(port) => port
      case _ =>
        println("no valid port specified as first argument")
        DEFAULT_PORT
    }
    controller.handleLine("server on " + port)

    val lsp = new MMTLSP
    controller.extman.get(classOf[BuildManager]).foreach(controller.extman.removeExtension)
    controller.extman.addExtension(new TrivialBuildManager)
    controller.extman.addExtension(lsp)
    lsp.runLocal
  }
}