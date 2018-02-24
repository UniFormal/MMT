package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api._
import java.io._
import scala.collection.JavaConverters._

object ShellCommand {

  /** return value of [[ShellCommand]] */
  abstract class Result(val errorO: Option[Error]) {
    def success = errorO.isEmpty
  }
  case class Abort(e: Exception) extends Result(Some(GeneralError("shell command aborted").setCausedBy(e)))
  case class Fail(output: String, exitValue: Int) extends Result(Some(GeneralError("unknown error: " + output)))
  case class Success(output: String) extends Result(None)

  private def runInGeneral(dir: Option[File], env: Map[String,String], command: String*)(out: String => Unit): Result = {
    // running shell commands is always brittle; better always print what we're trying to do 
    println("running command: " + command.mkString(" "))
    println("in directory: " + dir.getOrElse(File(System.getProperty("user.dir"))).toString)

    val pb = new java.lang.ProcessBuilder(command: _*) // use .inheritIO() for debugging
    dir.foreach(d => pb.directory(d.toJava))
    pb.environment().putAll(env.asJava)
    pb.redirectErrorStream(true)
    val proc = try {
       pb.start()
    } catch {case e: Exception =>
       return Abort(e) // GeneralError("error while trying to run external process").setCausedBy(e)
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
    val op = output.result()
    val ev = proc.exitValue
    if (ev == 0) {
      Success(op)
    } else {
      Fail(op, ev)
    }
  }

  /**
   * @param command the command to run
   * @return output+error message if not successful
   */
  def run(command: String*) = runInGeneral(None, Map(), command: _*){_ => ()}

  /**
   * like run
   * @param dir the directory in which to run the command
   */
  def runIn(dir: File, command: String*) = runInGeneral(Some(dir), Map(), command: _*){_=>()}

  /**
   * like run in
   * @param env environment variables to pass on
   */
  def runInWithEnv(dir: File, env: Map[String, String], command: String*) = runInGeneral(Some(dir), env, command: _*){_=>()}
  
  /**
   * like runIn but prints output
   */
  def runAndPrint(dir: Option[File], command: String*) = {
     dir.foreach {d => println("running in "+d)}
     println(command.mkString(" "))
     runInGeneral(dir, Map(), command: _*){s => println(s)}
  }
}