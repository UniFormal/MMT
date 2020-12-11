package info.kwarc.mmt.frameit.communication.server.playground

import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}

object BuildFrameworld {
  def main(args: Array[String]): Unit = {
    implicit val ctrl: Controller = new Controller()
    ctrl.report.addHandler(ConsoleHandler)
    ctrl.handleLine("log+ build")
    ctrl.handleLine("log+ archive")
    ctrl.handleLine("extension info.kwarc.mmt.lf.Plugin")

    ctrl.handleLine(s"mathpath archive C:\\Users\\nroux\\Desktop\\mmt-archives")
    // ctrl.handleLine(s"build MMT/urtheories scala-bin")
    // ctrl.handleLine(s"build FrameIT/frameworld -mmt-omdoc Scrolls/TriangleScrolls.mmt")
    ctrl.handleLine(s"build FrameIT/frameworld mmt-omdoc Scrolls/TriangleScrolls.mmt")
  }
}
