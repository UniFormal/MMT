package info.kwarc.mmt.frameit.communication.server.playground

import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}

object BuildFrameworld {
  def main(args: Array[String]): Unit = {
    implicit val ctrl: Controller = new Controller()
    ctrl.report.addHandler(ConsoleHandler)
    ctrl.handleLine(s"mathpath archive C:\\Users\\nroux\\Desktop\\FrameIT\\archives")
    ctrl.handleLine(s"build FrameIT/frameworld mmt-omdoc")
  }
}
