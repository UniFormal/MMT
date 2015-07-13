package info.kwarc.mmt.api.utils

import java.io._

object ShellCommand {
  private def runInOpt(dir: Option[File], command: String*) = {
    val pb = new java.lang.ProcessBuilder(command: _*) // use .inheritIO() for debugging
    pb.redirectErrorStream(true)
    dir.foreach { d => pb.directory(d.toJava) }
    val proc = pb.start()

    // read all output immediately to make sure proc does not deadlock if buffer size is limited
    val output = new StringBuilder
    val outputReader = new BufferedReader(new InputStreamReader(proc.getInputStream))
    var line: Option[String] = None
    while ( {
      line = Option(outputReader.readLine)
      line.isDefined
    }) {
      output.append(line.get)
    }

    proc.waitFor
    outputReader.close()

    val ev = proc.exitValue
    if (ev != 0) {
      Some(output.result())
    } else
      None
  }

  /**
   * @param command the command to run
   * @return output+error stream if not successful
   */
  def run(command: String*): Option[String] = runInOpt(None, command: _*)

  /**
   * like run
   * @param dir the directory in which to run the command
   */
  def runIn(dir: File, command: String*): Option[String] = runInOpt(Some(dir), command: _*)
}
