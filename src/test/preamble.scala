import info.kwarc.mmt.api
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{Logger, Run}
import info.kwarc.mmt.api.ontology.{DeclarationTreeExporter, DependencyGraphExporter, PathGraphExporter}
import info.kwarc.mmt.api.web.JSONBasedGraphServer

import scala.concurrent.Future

/** An abstract class for test methods. Instantiates a controller, sets the mathpath for archives,
  * loads the AlignmentsServer (so you can run a Server without getting an error message.
  *
  * You just need to give archivepath and instantiate the run method with your own arbitrary code
  *
  * @param archivepath    : the path to your archives
  * @param logprefixes    : A list of logprefixes to log
  * @param alignmentspath : the path to .align files (doesn't need to be set, therefore defaults to
  *                         empty string)
  * @param serverport     : Optional port to start a server. If None, no server is started
  * @param gotoshell      : if true, it will drop to the MMT shell afterwards
  * @param logfile        : If defined, will log into file
  */
abstract class Test(archivepath : String,
                    logprefixes : List[String] = Nil,
                    alignmentspath : String = "",
                    serverport : Option[Int] = None,
                    gotoshell : Boolean = true,
                    logfile : Option[String] = None) extends Logger {
  val controller = Run.controller
  def logPrefix = "user"
  def report = controller.report

  // setup logging
  controller.handleLine("log console")
  logfile.foreach(lf => controller.handleLine("log html " + lf))
  ("test" :: logprefixes) foreach (s => controller.handleLine("log+ " + s))

  // add the archives
  controller.handleLine("mathpath archive " + archivepath)
  controller.handleLine("lmh root " + archivepath)

  // add the plugins
  controller.handleLine("extension info.kwarc.mmt.lf.Plugin")
  controller.handleLine("extension info.kwarc.mmt.odk.Plugin")
  controller.handleLine("extension info.kwarc.mmt.pvs.PVSImporter")
  // controller.handleLine("extension info.kwarc.mmt.metamath.Plugin")

  controller.handleLine(("extension info.kwarc.mmt.api.ontology.AlignmentsServer " + alignmentspath).trim)


  def doFirst : Unit = {}

  def run : Unit

  /*
  def log(s : String) = {
    controller.report("user",s)
    controller.report.flush
  }
  */

  def main(args: Array[String]): Unit = try {

    controller.extman.addExtension(new DependencyGraphExporter)
    controller.extman.addExtension(new DeclarationTreeExporter)
    controller.extman.addExtension(new JSONBasedGraphServer)
    controller.extman.addExtension(new PathGraphExporter)
      doFirst
      if (serverport.isDefined) {
        //controller.handleLine("clear")
        controller.handleLine("server on " + serverport.get)
      }
      if (gotoshell) {
        Future {
          Run.disableFirstRun = true
          Run.main(Array())
        }(scala.concurrent.ExecutionContext.global)
        Thread.sleep(1000)
      }
      run
    } catch {
      case e: api.Error => println(e.toStringLong)
        sys.exit
    }

  def hl(s : String) = controller.handleLine(s)
  def logp(s : String) = hl("log+ " + s)
}

/**
  * As an example, here's my default. All test files of mine just extend this:
  */
abstract class DennisTest(prefixes : String*) extends Test(
  "/home/jazzpirate/work/MathHub",
  prefixes.toList,
  "/home/jazzpirate/work/Stuff/AlignmentsPublic",
  Some(8080),
  true,
  Some("/home/jazzpirate/work/mmtlog.html")
) {
}

object TomTest {
  val tomMathHub = System.getProperty("user.home") + "/Projects/gl.mathhub.info"
  val tomAlignments = tomMathHub + "/alignments/Public"
}


abstract class TomTest(prefixes : String*) extends Test(
  TomTest.tomMathHub,
  prefixes.toList,
  TomTest.tomAlignments,
  Some(8080),
  false,
  None
)

abstract class JonasTest(prefixes: String*) extends Test(
  "/home/jbetzend/Development/KWARC/content/",
  prefixes.toList,
  "",
  None,
  true,
  None
)

abstract class MichaelTest(prefixes: String*) extends Test(
  "/home/michael/content/",
  prefixes.toList,
  "",
  Some(8080),
  true,
  None
)