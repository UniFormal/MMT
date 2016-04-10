package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import utils._

import MMTSystem._

/** a build target that delegates to the standard scala compiler */
class ScalaCompiler extends BuildTarget {
   val key = "scala-bin"
   
   def build(a: Archive, up: Update, in: FilePath) {
     val files = (a / Dim("scala") / in).descendants.filter(_.getExtension == Some("scala")).map(_.toString)
     val classPath = MMTSystem.runStyle match {
       case FatJar(j) => List(j)
       case ThinJars(d) => List(d / "main")
       case r: Classes => List(r.parentFolder / "bin", r.projectFolder("mmt-lf") / "bin") //TODO don't hard-code LF path here
     }
     val sep = if (OS.detect == Windows) ";" else ":"
     val classPathS = classPath.map(_.toString).mkString(sep)
     val args = ("-d" :: "bin" :: "-cp" :: classPathS :: files).toArray
     scala.tools.nsc.Main.process(args)
   }
   
   def clean(a: Archive, in: FilePath) {
     delete(a / Dim("bin"))
   }
}