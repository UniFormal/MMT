package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import utils._

object ScalaOutDim extends RedirectableDimension("bin")

/** a build target that delegates to the standard scala compiler */
class ScalaCompiler extends BuildTarget {
  val key = "scala-bin"

  // the classpath separator
  private val sep = if (OS.detect == Windows) ";" else ":"

  /** recursively finds the classpath of all dependencies of an archive */
  private def transitiveArchiveClasspath(a : Archive) = {
    a.transitiveDependencies(controller.backend).map(singleArchiveClassPath).mkString(sep)
  }

  /** find the classpath of a single archive */
  private def singleArchiveClassPath(a : Archive) = {
    a.properties.get("classpath").map(p => a.root / p).mkString(sep)
  }


   def build(a: Archive, w: Build, in: FilePath, errorCont: Option[ErrorHandler]) {
     (a / ScalaOutDim).mkdirs
     // find all source folders to collect files in
     val folderList = a.properties.getOrElse("scala", "scala")
     val folders = stringToList(folderList).map(d => a / Dim(d)).filter(_.exists)
     // find all the scala files and throw an error if they are not available
     val files = folders.flatMap(f => f.descendants).filter(_.getExtension.contains("scala")).map(_.toString)
     if (files.isEmpty) {
       log("no scala files found in folders " + folders.mkString(", "))
       return
     }

     // compute the classpath of the archives
     val cp = transitiveArchiveClasspath(a)

     // prepare a process for the compiler to run in
     val args = (
       if(report.groups.contains("compiler")) List("-verbose") else Nil
     ) ::: List(
       "-classpath", System.getProperty("java.class.path") + sep + cp,
       "-d", (a / ScalaOutDim).toString,
     ) ::: files

     val process = RunJavaClass("scala.tools.nsc.Main", args, s => report("debug", s))

     // run the process and process its output
     // we must redirect error output to output -
     //   otherwise we'd have to use two threads to read output and error output concurrently to avoid deadlocking
     // if we used inheritIO instead of reading the output ourselves, we'd have to call proc.waitFor() afterwards
     val proc = process.redirectErrorStream(true).start()
     val output = StreamReading.read(proc.getInputStream)
     log(output)
     val ev = proc.waitFor() // waits (which shouldn't be necessary) and returns the exit value
     if (ev != 0) {
       val e = LocalError("scalac returned errors (see log output for error messages)")
       errorCont match {
         case Some(ec) => ec(e)
         case None => throw e
       }
     }
   }

   def clean(a: Archive, in: FilePath) {
     delete(a / Dim("bin"))
   }
}
