package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api._
import java.io._
import scala.collection.JavaConverters._

/**
  * An external command that is run in the shell
  * @param dir optional working directory of process to run
  * @param command Arguments of command to run
  * @param env Optional additional environment variables to set
  * @param out Function to run every time the command prints something on stdout
  * @param err Function to run every time the command prints something on stderr
  * @param redirect Boolean indicating if we should redirect stderr to stdout
  */
class ShellCommand(dir: Option[File], command: List[String], env: Map[String, String] = Map(), redirect: Boolean = false)(out: String => Unit)(err: String => Unit) {

  // create buffers for both streams
  private val stdoutBuffer = new StringBuffer
  private val stderrBuffer = new StringBuffer

  private def _run(): Int = {
    // setup process properties
    val pb = new java.lang.ProcessBuilder(command: _*)
    dir.foreach(d => pb.directory(d.toJava))
    pb.environment().putAll(env.asJava)

    if(redirect){ pb.redirectErrorStream(true) }

    // create the process
    val p = try {
      pb.start()
    } catch {
      case e: Exception =>
        return -1
    }

    // create some readers for stdout and stderr
    val outputReader = new BufferedReader(new InputStreamReader(p.getInputStream))
    val errorReader = new BufferedReader(new InputStreamReader(p.getErrorStream))

    // and prepare the lines to be read
    var outLine: Option[String] = None
    var errLine: Option[String] = None

    // keep reading stdout and stderr as long as there are things to read
    while ( {
      outLine = Option(outputReader.readLine)
      errLine = Option(errorReader.readLine)

      outLine.isDefined || errLine.isDefined
    }) {
      outLine.map(o => {
        out(o)
        stdoutBuffer.append(o + "\n")
      })
      errLine.map(e => {
        err(e)
        stderrBuffer.append(e + "\n")
      })
    }

    // wait for the process to finish
    // and clean up
    p.waitFor
    outputReader.close()
    errorReader.close()


    p.exitValue()
  }

  /** the code returned by this process */
  val code : Int = _run()
  val success: Boolean = code == 0

  /** standard output sent by this process */
  lazy val stdout : String = stdoutBuffer.toString
  /** return stdout of this process, if successful */
  lazy val successOption : Option[String] = if(success) { Some(stdout) } else { None }
  /** return stdout of the process if failed */
  lazy val errorOption : Option[String] = if(!success) { Some(stderr) } else { None }

  /** the STDERR output sent by this process */
  lazy val stderr : String = stderrBuffer.toString

}

object ShellCommand {
  /**
    * Runs a shell command
    */
  def apply(dir: Option[File], env: Map[String, String], command: String*): ShellCommand = new ShellCommand(dir, command.toList, env, redirect = true)(_ => {})(_ => {})
  def apply(dir: Option[File], command: String*): ShellCommand = apply(dir, Map[String, String](), command:_*)
  def apply(dir: File, command: String*): ShellCommand = apply(Some(dir), Map[String, String](), command: _*)
  def apply(dir: File, env: Map[String, String], command: String*): ShellCommand = apply(Some(dir), env, command: _*)

  /** runs a command and print output */
  def loud(dir: Option[File], command: String*): Option[String] = {
    println(command.mkString(" "))
    dir.foreach {d => println("running in "+d)}
    new ShellCommand(dir, command.toList, redirect = true)({s => println(s)})(_ => {}).errorOption
  }

  /**
   * @param command the command to run
   * @return output+error message if not successful
   */
  @deprecated("use apply(None, command:_*).errorOption instead", "")
  def run(command: String*): Option[String] = apply(None, command:_*).errorOption

  /**
   * like run
   * @param dir the directory in which to run the command
   */
  @deprecated("use apply(dir, command:_*).errorOption", "")
  def runIn(dir: File, command: String*): Option[String] = apply(dir, command:_*).errorOption

  /**
   * like runIn but prints output
   */
  @deprecated("renamed to loud()", "")
  def runAndPrint(dir: Option[File], command: String*): Option[String] = loud(dir, command:_*)
}
