package info.kwarc.mmt.frameit.communication.server

import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld

/** For testing things without touching the server code **/
private object Playground {
  def main(args: Array[String]): Unit = {
    implicit val ctrl: Controller = new Controller()
    ctrl.report.addHandler(ConsoleHandler)

    ctrl.handleLine(s"mathpath archive ${args(0)}")
    ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc Scrolls/")
  }
}
