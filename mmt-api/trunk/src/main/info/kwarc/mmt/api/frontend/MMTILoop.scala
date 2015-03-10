package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._

import scala.tools.nsc._

/** a wrapper around the interactive Scala interpreter
 *  
 *  @param controller a controller that is used to initialize the Scala environment
 */
class MMTILoop(controller: Controller) extends interpreter.ILoop {
   /** this is overridden in order to bind variables after the interpreter has been created */
   override def createInterpreter() {
     if (addedClasspath != "")
       settings.classpath append addedClasspath
     intp = new ILoopInterpreter
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
   override def printWelcome() {
      out.println
      out.println("This is a Scala interpreter running within MMT; it may take a few seconds for the prompt to appear; ':help' lists commands.")
      out.println("Use 'controller' to access the current MMT Controller, use 'mmt\"expression\"' to invoke the MMT parser.")
      out.println
      out.flush
   }
   override def prompt = "scala-mmt> "
   def run {
      val settings = new Settings
      settings.Yreplsync.value = true
      settings.usejavacp.value = true // make sure all classes of the Java classpath are available
      //settings.sourceReader.value = "SimpleReader"
      process(settings)
   }
}
