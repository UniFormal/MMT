package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.{File,MMTSystem}
import info.kwarc.mmt.api.web.Util

object Server {
  val default_port: Int = 8080

  val isabelle_tool =
    isabelle.Isabelle_Tool("mmt_server", "start MMT HTTP server for given archive directories",
      isabelle.Scala_Project.here,
      args => {
        var archive_dirs: List[isabelle.Path] = Nil
        var port = default_port

        val getopts = isabelle.Getopts(
          """
Usage: isabelle mmt_server [OPTIONS]

  Options are:
    -A DIR       add archive directory
    -p PORT      server port (default: """ + default_port +
            """)

  Start MMT HTTP server on localhost, using specified archive directories.
""",
          "A:" -> (arg => archive_dirs = archive_dirs ::: List(isabelle.Path.explode(arg))),
          "p:" -> (arg => port = isabelle.Value.Int.parse(arg)))

        val more_args = getopts(args)
        if (more_args.nonEmpty) getopts.usage()

        val options = isabelle.Options.init()
        val progress = new isabelle.Console_Progress()

        val (controller, _) =
          Importer.init_environment(options, progress = progress, archive_dirs = archive_dirs)

        if (Util.isTaken(port)) isabelle.error("Port " + port + " already taken")
        controller.handleLine("server on " + port)

        progress.echo("Server http://127.0.0.1:" + port)
        progress.echo("Waiting for INTERRUPT signal ...")

        try { isabelle.POSIX_Interrupt.exception { while (true) Thread.sleep(Integer.MAX_VALUE) } }
        catch { case isabelle.Exn.Interrupt() => }

        controller.handleLine("server off")
      }
    )
}

