import info.kwarc.mmt.api
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{Logger, Run}
import info.kwarc.mmt.api.ontology.{DeclarationTreeExporter, DependencyGraphExporter, PathGraphExporter}
import info.kwarc.mmt.api.utils.File
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
          Thread.sleep(1000)
          run
        }(scala.concurrent.ExecutionContext.global)
        Run.disableFirstRun = true
        Run.main(Array())
      }
      else run
    } catch {
      case e: api.Error =>
        println(e.toStringLong)
        sys.exit
    }

  def hl(s : String) = controller.handleLine(s)
  def logp(s : String) = hl("log+ " + s)
}

/**
  * Auto-magical path-finder for all the development setups.
  * If you use a custom folder, please add into the appropriate lists
  */
object MagicTest {
  lazy private val home = File(System.getProperty("user.home"))

  /** the root for archives to use */
  lazy val archiveRoot: File = {
    List(
      home / "work" / "MathHub", // Dennis
      home / "Projects" / "gl.mathhub.info", // Tom
      home / "Development" / "KWARC" / "content", // Jonas
      home / "content", // Michael
      File("C:") / "other" / "oaff",
    ).find(_.exists).getOrElse(throw GeneralError("MagicTest failed: No known archive root"))
  }

  /** the root for alignments */
  lazy val alignments: Option[File] = List(
    home / "work" / "Stuff" / "AlignmentsPublic", // Dennis
    archiveRoot / "alignments" / "Public", // if installed via lmh
    archiveRoot / "Alignments" / "Public" // if manually installed into differently cased path
  ).find(_.exists)

  /** the logfile to use for MMT */
  lazy val logfile: Option[File] = {
    if((home / "work").exists){
      Some(home / "work" / "mmtlog.html") // Dennis
    } else {
      None
    }
  }
}

/**
  * A magic test configuration that automatically figures out the paths to everything
  */
abstract class MagicTest(prefixes : String*) extends Test(
  MagicTest.archiveRoot.toString,
  prefixes.toList,
  MagicTest.alignments.map(_.toString).getOrElse(""),
  Some(8080),
  false,
  MagicTest.logfile.map(_.toString)
)