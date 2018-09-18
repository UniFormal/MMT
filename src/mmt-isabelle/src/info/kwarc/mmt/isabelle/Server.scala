package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.{File,MMTSystem}
import info.kwarc.mmt.api.web.Util

/**
 * A simple application that starts an MMT server and serves a given directory.
 * This is called by the shell-script mmt_server, via which it is available as a tool from within Isabelle.
 */
object Server
{
  val default_port: Int = 8080

  def main(args: Array[String])
  {
    isabelle.Command_Line.tool0 {
      var archive_dirs : List[isabelle.Path] = Nil
      var port = default_port

      val getopts = isabelle.Getopts("""
Usage: isabelle mmt_server [OPTIONS]

  Options are:
    -A DIR       add archive directory
    -p PORT      server port (default: """ + default_port + """)

  Start MMT HTTP server on localhost, using output directory as archive.
""",
        "A:" -> (arg => archive_dirs = archive_dirs ::: List(isabelle.Path.explode(arg))),
        "p:" -> (arg => port = isabelle.Value.Int.parse(arg)))

      val more_args = getopts(args)
      if (more_args.nonEmpty) getopts.usage()

      if (Util.isTaken(port)) isabelle.error("Port " + port + " already taken")

      val controller = new Controller
      for {
        config <-
          List(File(isabelle.Path.explode("$ISABELLE_MMT_ROOT/deploy/mmtrc").file),
            MMTSystem.userConfigFile)
        if config.exists
      } controller.loadConfigFile(config, false)

      val archives = archive_dirs.flatMap(dir => controller.backend.openArchive(dir.absolute_file))
      for (archive <- archives) {
        println("Adding " + archive)
        controller.handleLine("mathpath archive " + archive.rootString) // FIXME quotes!?
      }

      controller.handleLine("server on " + port)

      println("Server http://127.0.0.1:" + port)
      println("Waiting for INTERRUPT signal ...")

      try { isabelle.POSIX_Interrupt.exception { while(true) Thread.sleep(Integer.MAX_VALUE) } }
      catch { case isabelle.Exn.Interrupt() => }

      controller.handleLine("server off")
    }
  }
}
