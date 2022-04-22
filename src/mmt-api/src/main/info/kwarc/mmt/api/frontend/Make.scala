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
       println(input + " not found")
       return withError
     }
     controller.backend.openArchive(archRoot)
     val arch = controller.backend.getArchive(archRoot).getOrElse {
       println(input + " not found") // should be impossible
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
             println(input + " is not an input file for " + key)
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
     val ec = new ErrorContainer(Some(report))
     bt(BuildAll, arch, buildPath, Some(ec))
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
    controller.report.addHandler(ConsoleHandler)
    controller.report.groups += "debug"
    val errorCont = new ErrorContainer(Some(report))
    val maxErrorLevel = try {
      controller.runMSLFile(file,None,true,Some(errorCont))
      errorCont.maxLevel
    } catch {
      case e: Exception =>
        errorCont(Error(e))
        Level.Fatal
    }
    Some(maxErrorLevel)
  }
}
