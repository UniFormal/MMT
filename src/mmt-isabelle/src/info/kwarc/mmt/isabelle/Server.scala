package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.web.Util

object Server
{
  val default_output_dir = isabelle.Path.explode(Importer.Arguments.default_output_dir)
  val default_port: Int = 8080

  def main(args: Array[String])
  {
    isabelle.Command_Line.tool0 {
      var output_dir = default_output_dir
      var port = default_port

      val getopts = isabelle.Getopts("""
Usage: isabelle mmt_server [OPTIONS]

  Options are:
    -O DIR       output directory for MMT (default: """ + default_output_dir + """)
    -p PORT      server port (default: """ + default_port + """)

  Start MMT HTTP server on localhost, using output directory as archive.
""",
        "O:" -> (arg => output_dir = isabelle.Path.explode(arg)),
        "p:" -> (arg => port = isabelle.Value.Int.parse(arg)))

      val more_args = getopts(args)
      if (more_args.nonEmpty) getopts.usage()

      if (Util.isTaken(port)) isabelle.error("Port " + port + " already taken")

      val controller = new Controller
      controller.setHome(File(output_dir.file))

      controller.handleLine("mathpath archive .")
      controller.handleLine("server on " + port)

      println("Server http://127.0.0.1:" + port + " for archive " + output_dir.absolute)
      println("Waiting for INTERRUPT signal ...")

      try { isabelle.POSIX_Interrupt.exception { while(true) Thread.sleep(Integer.MAX_VALUE) } }
      catch { case isabelle.Exn.Interrupt() => }

      controller.handleLine("server off")
    }
  }
}
