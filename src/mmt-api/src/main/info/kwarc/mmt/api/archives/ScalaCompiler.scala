package info.kwarc.mmt.api.archives

import java.net.URLClassLoader

import info.kwarc.mmt.api._
import utils._
import MMTSystem._

import scala.util.Try

object ScalaOutDim extends RedirectableDimension("bin")

/** a build target that delegates to the standard scala compiler */
class ScalaCompiler extends BuildTarget {
  val key = "scala-bin"

  /** recursively finds the classpath of all dependencies of an archive */
  private def transitiveArchiveClasspath(a : Archive) = {
    a.transitiveDependencies(controller.backend).map(singleArchiveClassPath).mkString(":")
  }

  /** find the classpath of a single archive */
  private def singleArchiveClassPath(a : Archive) = {
    a.properties.get("classpath").map(p => a.root / p).mkString(":")
  }


   def build(a: Archive, up: Update, in: FilePath) {
     (a / ScalaOutDim).mkdirs
     // the classpath separator
     val sep = if (OS.detect == Windows) ";" else ":"
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
       "-classpath", System.getProperty("java.class.path") + ":" + cp,
       "-d", (a / ScalaOutDim).toString,
     ) ::: files

     val process = RunJavaClass("scala.tools.nsc.Main", args)
     report("debug", process.command().toArray.mkString(" "))

     // run the process and wait for it
     val proc = process.inheritIO().start()
     proc.waitFor()

     if(proc.exitValue() != 0) {
       throw GeneralError("scalac returned error code")
     }
   }

   def clean(a: Archive, in: FilePath) {
     delete(a / Dim("bin"))
   }
}
