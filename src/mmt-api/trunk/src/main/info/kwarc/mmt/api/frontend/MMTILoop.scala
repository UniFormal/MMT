package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._

import scala.tools.nsc._

/** a wrapper around the interactive Scala interpreter
 *  
 *  @param controller a controller that is used to initialize the Scala environment
 */
class MMTILoop(controller: Controller) extends interpreter.ILoop {
   /** this is overridden in order to bind variables after the interpreter has been created */
   override def createInterpreter {
      super.createInterpreter
      init
   }
   override def printWelcome {
      out.println
      out.println("This is a Scala interpreter running within MMT; ':help' lists commands.")
      out.println("Use 'controller' to access the current MMT Controller.")
      out.println
      out.flush
   }
   override def prompt = "scala-mmt> "
   private def init {
     intp beQuietDuring {
         intp.interpret("import info.kwarc.mmt.api._")
         intp.bind("controller", controller)
         val interpolator = new MMTInterpolator(controller)
         intp.bind("interpolator", interpolator)
         val isimp = new InteractiveSimplifier(controller, this)
         intp.bind("isimp", isimp)
         intp.interpret("import interpolator._")
      }
   }
   /** run a command and return or interactively read commands */
   def run(command: Option[String]) {
      val settings = new Settings
      settings.usejavacp.value = true // make sure all classes of the Java classpath are available
      //settings.sourceReader.value = "SimpleReader"
      command match {
         case None =>
            settings.Yreplsync.value = true
            out.println("It may take a few seconds for the Scala prompt to appear.")
            process(settings)
         case Some(c) =>
            // code copied from process(settings) but without going into the loop
            this.settings = settings
            createInterpreter
            intp.interpret(c)
            closeInterpreter
      }
   }
}
