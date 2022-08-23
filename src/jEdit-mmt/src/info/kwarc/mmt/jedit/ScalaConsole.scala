package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import utils._
import org.gjt.sp.jedit._
import console._

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter._
import scala.tools.nsc.interpreter.shell.ReplReporterImpl

/** a Scala console for jEdit */ 
class ScalaConsole extends ThreadedConsole("scala") {
   override def printInfoMessage (output: Output): Unit = {
    output.print(null, "This is a Scala shell provided by MMT; the names `controller` (the main MMT object) and `view` (the jEdit window) are predefined")
   }

   //override def getCompletions(console: Console, command: String): Shell.CompletionInfo = ???

   /**
    * to maintain the state across calls, we must use a fixed Interpreter
    * so we have to fix the output stream even though we only get the output stream when `execute` is called
    * so we have to awkwardly store it in a variable and use a fixed writer for that variable 
    */
   private var currentOutput: Output = null
   /** forwards all input to the currentOutput */
   private val writer = new java.io.Writer {
     def flush(): Unit = {}
     def close(): Unit = {}
     def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
       val s = new String(cbuf.slice(off, off + len))
       currentOutput.print(null,s)
     }
   }

  /** construct a class that contains everything that jEdit uses; maybe we should also add all classes from MMT archives */
  private def getClasspath = {
    def getJars(folder: File) = folder.children.filter{f => f.getExtension contains "jar"}
    val sep = java.io.File.pathSeparatorChar.toString
    val systemCP = stringToList(System.getProperty("java.class.path", ""), sep)
    val jEditCP = getJars(File(jEdit.getSettingsDirectory) / "jars") ::: getJars(File(jEdit.getJEditHome)) 
    (systemCP ::: jEditCP).mkString(sep)
  }
   
   private lazy val interpreter = {
     val settings = new Settings
     settings.classpath.value = getClasspath
     val intp = new IMain(settings, new ReplReporterImpl(settings,new java.io.PrintWriter(writer)))
     // Makarius does some more stuff with class loaders here, but it doesn't seem to make a difference
     intp beQuietDuring {
       intp.bind("mmt", mmt)
       intp.bind("controller", controller)
     }
     intp
   }
   def execute(console: Console, input: String, output: Output, error: Output, command: String) = executionWrapper {
      currentOutput = output
      try {
        interpreter beQuietDuring {
           interpreter.bind("view", console.getView)
        }
        val r = interpreter.interpret(command)
        r match {
          case Results.Success => true
          case e =>
            logError(e.toString)
            false
        }
      } catch {
         case e: Exception =>
           logError(Error(e).toStringLong)
           false
      } finally {
         output.commandDone()
         currentOutput = null
      }
   }
}
