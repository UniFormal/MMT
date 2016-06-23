import info.kwarc.mmt.api.frontend.Run

/** An abstract class for test methods. Instantiates a controller, sets the mathpath for archives,
  * loads the AlignmentsServer (so you can run a Server without getting an error message.
  *
  * You just need to give archivepath and instantiate the run method with your own arbitrary code
  *
  * @param archivepath    : the path to your archives
  * @param alignmentspath : the path to .align files (doesn't need to be set, therefore defaults to
  *                         empty string)
  * @param serverport     : Optional port to start a server. If None, no server is started
  * @param gotoshell      : if true, it will drop to the MMT shell afterwards
  */
abstract class Test(archivepath : String,
                    alignmentspath : String = "",
                    serverport : Option[Int] = None,
                    val gotoshell : Boolean = true) {
  val controller = Run.controller

  controller.handleLine("log console")
  controller.handleLine("extension info.kwarc.mmt.lf.Plugin")
  controller.handleLine("extension info.kwarc.mmt.odk.Plugin")
  controller.handleLine("extension info.kwarc.mmt.pvs.Plugin")
  controller.handleLine("mathpath archive " + archivepath)
  controller.handleLine("extension info.kwarc.mmt.api.ontology.AlignmentsServer " + alignmentspath)

  def run : Unit

  def main(args: Array[String]): Unit = {
    run
    if (serverport.isDefined) controller.handleLine("server on " + serverport.get)
    if (gotoshell) Run.main(Array())
  }
}

/**
  * As an example, here's my default. All test files of mine just extend this:
  */
abstract class DennisTest extends Test("/home/raupi/lmh/MathHub","/home/raupi/Stuff/Public",Some(8080))
