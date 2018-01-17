package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api._
import java.io._

object ShellCommand {
  private def runInOpt(dir: Option[File], command: String*)(out: String => Unit): Option[String] = {
    val pb = new java.lang.ProcessBuilder(command: _*) // use .inheritIO() for debugging
    pb.redirectErrorStream(true)
    dir.foreach { d => pb.directory(d.toJava) }
    val proc = try {
       pb.start()
    } catch {case e: java.io.IOException =>
       return Some(e.getMessage) // GeneralError("error while trying to run external process").setCausedBy(e)
    }

    // read all output immediately to make sure proc does not deadlock if buffer size is limited
    val output = new StringBuilder
    val outputReader = new BufferedReader(new InputStreamReader(proc.getInputStream))
    var line: Option[String] = None
    while ( {
      line = Option(outputReader.readLine)
      line.isDefined
    }) {
      output.append(line.get + "\n")
      out(line.get)
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
   * @return output+error message if not successful
   */
  def run(command: String*): Option[String] = runInOpt(None, command: _*){_ => ()}

  /**
   * like run
   * @param dir the directory in which to run the command
   */
  def runIn(dir: File, command: String*): Option[String] = runInOpt(Some(dir), command: _*){_=>()}

  /**
   * like runIn but prints output
   */
  def runAndPrint(dir: Option[File], command: String*): Option[String] = {
     dir.foreach {d => println("running in "+d)}
     println(command.mkString(" "))
     runInOpt(dir, command: _*){s => println(s)}
  }

  /**
    * like runIn but buffers output
    */
  def runAndBuffer(dir: File, command: String*): (String, Option[String]) = {
    val buffer = new StringBuilder
    val errorString = runInOpt(Some(dir), command: _*){s => buffer.append(s + "\n")}
    (buffer.mkString, errorString)
  }
}
