package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.frontend.Controller

/**
 * Created by raupi on 22.07.15.
 */
object PVSTest {
  def main(args: Array[String]) {
    val path = "/home/raupi/lmh/MathHub/PVS/Prelude"
    val controller = new Controller
    controller.handleLine("archive add ../MathHub")
    controller.handleLine("extension info.kwarc.mmt.lf.Plugin")
    controller.handleLine("log console")
    controller.handleLine("log+ archive")
    controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")

    // log+ debug
    """log+ pvs-omdoc

    mathpath archive ../../MathHub

    build PVS/Prelude mmt-omdoc
    build PVS/Prelude pvs-omdoc
""".split("\\n").foreach(controller.handleLine(_))

    //
    // build PVS/Prelude mmt-omdoc
    // build PVS/NASA pvs-omdoc

    //controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
    //controller.handleLine("build PVS/Prelude mmt-omdoc")
    //controller.handleLine("build PVS/Prelude relational")
    //val t1 = controller.get(Path.parse("http://pvs.csl.sri.com/Prelude?bit"))
    //val t2 = controller.get(Path.parse("http://pvs.csl.sri.com/?PVS/defdecl"))
    //implicit val rh = new presentation.StringBuilder
    //val presenter = new MMTSyntaxPresenter
    //controller.extman.addExtension(presenter)
    //presenter(t2)
    //presenter(t1)
    //println(rh.get)
    //println(th.parameters)
    //val th = controller.get(Path.parse("http://pvs.csl.sri.com?PVS")).asInstanceOf[DeclaredTheory]
    //println(th)
    //println(th.getInnerContext)
    //println(th.parameters)

   // val c = controller.get(Path.parse("http://cds.omdoc.org/testcases?DLO?eq_leq")).asInstanceOf[FinalConstant]
   // println(c.tp.get.asInstanceOf[OMBINDC].context)

  }
}