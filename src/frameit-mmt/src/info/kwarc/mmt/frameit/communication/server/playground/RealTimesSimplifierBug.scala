package info.kwarc.mmt.frameit.communication.server.playground

import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.api.objects.{Context, OMS, Term}
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{NamespaceMap, Path}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.lf.ApplySpine

/** For testing things without touching the server code * */
private object RealTimesSimplifierBug {
  def main(args: Array[String]): Unit = {
    implicit val ctrl: Controller = new Controller()
    ctrl.report.addHandler(ConsoleHandler)

    ctrl.handleLine(s"mathpath archive ${args(0)}")
    //ctrl.handleLine(s"build ${FrameWorld.archiveID} -mmt-omdoc")
    //ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc")

    val simplify: Term => Term = {
      val ctx = Context(Path.parseM("http://mathhub.info/MitM/Foundation?RealLiterals", NamespaceMap.empty))
      val simplicationUnit = SimplificationUnit(ctx, expandConDefs = true, expandVarDefs = true, fullRecursion = true)

      t => ctrl.simplifier(t, simplicationUnit)
    }

    val t = ApplySpine(
      OMS(Path.parseS("http://mathhub.info/MitM/Foundation?RealLiterals?times_real_lit", NamespaceMap.empty)),
      FrameWorld.RealLiterals(4.0),
      FrameWorld.RealLiterals(3.0)
    )
    println(simplify(t))
  }
}
