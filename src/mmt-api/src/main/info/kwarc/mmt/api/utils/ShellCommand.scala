package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api._
import java.io._
import scala.jdk.CollectionConverters._
import utils.File

object ShellCommand {

  /** return value of [[ShellCommand]] */
  sealed abstract class Result(val errorO: Option[Error]) {
    def success = errorO.isEmpty
  }
  case class Abort(e: Exception) extends Result(Some(GeneralError("shell command aborted").setCausedBy(e)))
  case class Fail(output: String, exitValue: Int) extends Result(Some(GeneralError("unknown error: " + output)))
  case class Success(output: String) extends Result(None)

  private def runInGeneral(dir: Option[File], env: Map[String,String], command: String*)(out: String => Unit): Result = {
    // running shell commands is always brittle; better always print what we're trying to do 
    out("running command: " + command.mkString(" "))
    out("in directory: " + dir.getOrElse(File(System.getProperty("user.dir"))).toString)

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
    val op = StreamReading.read(proc.getInputStream)
    proc.waitFor
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
  def runInWithEnv(dir: File, env: Map[String, String], print: Boolean, command: String*) = {
    val out: String => Unit = if (print) println else (_ => ())
    runInGeneral(Some(dir), env, command: _*)(out)
  }
  
  /**
   * like runIn but prints output
   */
  def runAndPrint(dir: Option[File], command: String*) = {
     dir.foreach {d => println("running in "+d)}
     println(command.mkString(" "))
     runInGeneral(dir, Map(), command: _*){s => println(s)}
  }
}