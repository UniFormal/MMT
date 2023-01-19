package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import utils._
import archives._

/** a make-like shell extension that calls a build target on a file/folder in an archive
 */
class Make extends ShellExtension("make") {
   def helpText = ":make TARGETS ARCHIVE"
   def run(shell: Shell, args: List[String]): Option[Level.Level] = {
     if (args.length < 2) {
       println(helpText)
       return withError
     }
     val key = args(0)
     val input = controller.getHome resolve args(1)
     // get the archive containing the input file
     val (archRoot, relPath) = controller.backend.resolveAnyPhysical(input).getOrElse {
       println(input.toString + " not found")
       return withError
     }
     controller.backend.openArchive(archRoot)
     val arch = controller.backend.getArchive(archRoot).getOrElse {
       println(input.toString + " not found") // should be impossible
       return withError
     }
     // get the build target for the given key
     val bt = controller.extman.getOrAddExtension(classOf[BuildTarget], key).getOrElse {
       println("build target " + key + " not found")
       return withError
     }
     // strip the dimension from the remaining input path (if any)
     val buildPath = relPath.segments match {
       case Nil => relPath
       case dim::rest => bt match {
         case tbt: TraversingBuildTarget =>
           if (FilePath(dim) == arch.resolveDimension(tbt.inDim))
             FilePath(rest)
           else {
             println(input.toString + " is not an input file for " + key)
             return withError
           }
       }
       case _ =>
         println(key + " can only be called on an entire archive")
         return withError
     }
     // run the build target
     controller.report.addHandler(ConsoleHandler)
     controller.report.groups += "debug"
     controller.report.groups += "archives"
     controller.report.groups += bt.logPrefix
     val ec = new ErrorContainer
     val eh = MultipleErrorHandler(List(ec), report)
     bt(BuildAll, arch, buildPath, Some(eh))
     Some(ec.maxLevel)
   }
}

/** a make-like shell extension that runs an msl file and reports an exit code based on errors
  */
class RunFile extends ShellExtension("file") {
  def helpText = ":file FILE"
  def run(shell: Shell, args: List[String]): Option[Level.Level] = {
    if (args.length < 1) {
      println(helpText)
      return withError
    }
    val file = File(args(0))
    doIt(file)
  }
  def doIt(file: File): Option[Level.Level] = {
    controller.report.addHandler(ConsoleHandler)
    // controller.report.groups += "debug"
    controller.report.groups += logPrefix
    val errorContainer = new ErrorContainer
    val errorHandler = MultipleErrorHandler(List(errorContainer), report)
    try {
      controller.runMSLFile(file,None,true,Some(errorHandler))
    } catch {
      case e: Exception =>
        errorHandler(Error(e))
        log("interrupted due to unrecovered error")
        return Some(Level.Fatal)
    }
    val errors = errorContainer.getErrors.filter(_.level >= Level.Warning)
    val maxLev = errorContainer.maxLevel
    val groups = errors.groupBy(_.level)
    val actualErrors = groups.getOrElse(Level.Error, Nil)
    val msg = if (groups.isEmpty) "no errors" else groups.map {case (l,es) => s"${es.length} ${l}s"}.mkString(", ")
    val excMsg = if (actualErrors.isEmpty) ""
      else "; among the errors: " +
        actualErrors.groupBy(_.excuse).map {case (exc,es) => s"${es.length} ${Level.excuseOStr(exc)}s"}.mkString(", ")
    log("finished with " + msg + excMsg)
    if (maxLev <= Level.Error && actualErrors.forall(_.excuse.isDefined)) {
      // we succeed overall if there were at most warnings or excusable errors
      Some(Level.Info)
    } else {
      Some(maxLev)
    }
  }
}
