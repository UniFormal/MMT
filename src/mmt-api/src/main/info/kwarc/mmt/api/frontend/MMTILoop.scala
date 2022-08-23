package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._

import scala.tools.nsc.{settings, _}
import interpreter.shell.{ILoop, ShellConfig}
import scala.tools.nsc.interpreter.Results

/** a wrapper around the interactive Scala interpreter
 *
 *  @param controller a controller that is used to initialize the Scala environment
 */
object MMTILoop {
  val settings = new Settings() // make sure all classes of the Java classpath are available
  settings.usejavacp.value = true // TODO: this does not load classes from archives
  val cfg = ShellConfig(settings)
}
class MMTILoop(controller: Controller) extends ILoop(MMTILoop.cfg) {
   /** this is overridden in order to bind variables after the interpreter has been created */
   override def createInterpreter(settings: Settings = MMTILoop.settings): Unit = {
      super.createInterpreter(settings)
      init
   }
   override def printWelcome: Unit = {
      out.println
      out.println("This is a Scala interpreter running within MMT; ':help' lists commands.")
      out.println("Use 'controller' to access the current MMT Controller.")
      out.println
      out.flush
   }
   override lazy val prompt = "scala-mmt> "
   private def init: Unit = {
     def printError(r: Results.Result, s: String): Unit = {
       if (r != Results.Success)
         println("binding of " + s + " failed")
     }
     //intp. // TODO is this needed?
     intp beQuietDuring {
         intp.interpret("import info.kwarc.mmt.api._")
         printError(intp.bind("controller", controller), "controller")
         val interpolator = new MMTInterpolator(controller)
         printError(intp.bind("interpolator", interpolator), "interpolator")
         val isimp = new InteractiveSimplifier(controller, this)
         val s = intp.bind("isimp", isimp)
         intp.interpret("import interpolator._")
      }
   }
   /** run a command and return or interactively read commands */
   def run(command: Option[String]): Unit = {
      //settings.sourceReader.value = "SimpleReader"
      //settings.debug.value = true
      command match {
         case None =>
            out.println("It may take a few seconds for the Scala prompt to appear.")
         case Some(c) =>
            // code copied from process(settings) but without going into the loop
            createInterpreter()
            intp.interpret(c)
            closeInterpreter
      }
   }
}
