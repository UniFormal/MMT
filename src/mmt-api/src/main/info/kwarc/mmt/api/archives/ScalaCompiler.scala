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
   /** finds all the jars in subfolders of f, or Nil if the folder does not exist */
   private def jars(f: File) = Try(f.children.filter(_.getExtension.contains("jar"))).getOrElse(Nil)


  /** gets the current class path */
  private def getCurrentClassPath: List[File] = ClassLoader.getSystemClassLoader match {
    case loader: URLClassLoader => loader.getURLs.map(u => File(u.getFile)).toList
    case _ => Nil
  }

  /** find the class path needed for compilation */
  private def getClassPath: List[File] = MMTSystem.runStyle match {

    // fat jar => use the jar path directly
    case rs: IsFat =>
      report("debug", s"using FatJar classpath for 'scala-bin'")
      List(rs.jar)

    // thin jars => add all classpaths to weird jars
    case ThinJars(d) =>
      // TODO: We need to load all the dependencies in the scala-bin target
      report("debug", s"using ThinJar classpath for 'scala-bin'")
      jars(d / "lib") ::: jars(d / "main")

    case r: Classes =>
      report("debug", s"using Classes classpath for 'scala-bin'")
      jars(r.deploy / "lib") ::: List(r.classFolder, r.projectFolder("mmt-lf") / "bin") //TODO don't hard-code LF path here

    // we are running some other (weird) way, so we do not know the classpath
    case OtherStyle =>
      Nil
  }

   def build(a: Archive, up: Update, in: FilePath) {
     (a / ScalaOutDim).mkdirs


     // the classpath seperator
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

     // try to magically find the class path
     val styleClassPath = getClassPath
     report("debug", s"found style class path ${styleClassPath.mkString(sep)}")

     // re-use the current (active) class path
     val currentClassPath = getCurrentClassPath
     report("debug", s"found current class path ${currentClassPath.mkString(sep)}")

     // make the class path distinct
     val classPath = (styleClassPath ::: currentClassPath).distinct
     val classPathS = classPath.map(_.toString).mkString(sep)
     report("debug", s"set 'scala-bin' classpath to $classPathS")

     // run the compiler: -verbose -nowarn
     val args = (if(report.groups.contains("compiler")) List("-verbose") else Nil) ::: ("-d" :: (a / ScalaOutDim).toString :: "-cp" :: classPathS :: files)

     report("debug", s"scalac ${args.mkString(" ")}")
     scala.tools.nsc.Main.process(args.toArray)
   }

   def clean(a: Archive, in: FilePath) {
     delete(a / Dim("bin"))
   }
}
