package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import utils._

import MMTSystem._

object ScalaOutDim extends RedirectableDimension("bin")

/** a build target that delegates to the standard scala compiler */
class ScalaCompiler extends BuildTarget {
   val key = "scala-bin"

   private def jars(f: File) = f.children.filter(_.getExtension.contains("jar"))

   def build(a: Archive, up: Update, in: FilePath) {
     (a / ScalaOutDim).mkdirs
     val folderList = a.properties.getOrElse("scala", "scala")
     val folders = stringToList(folderList).map(d => a / Dim(d)).filter(_.exists)
     val files = folders.flatMap(f => f.descendants).filter(_.getExtension.contains("scala")).map(_.toString)
     val classPath = MMTSystem.runStyle match {
       case rs: IsFat => List(rs.jar)
       case ThinJars(d) => jars(d/"lib") ::: jars(d/"main")
       case r: Classes =>
         jars(r.deploy / "lib") ::: List(r.classFolder, r.projectFolder("mmt-lf")/"bin") //TODO don't hard-code LF path here
       case OtherStyle => Nil
     }
     val sep = if (OS.detect == Windows) ";" else ":"
     val classPathS = classPath.map(_.toString).mkString(sep)
     // -verbose -nowarn
     val args = ("-d" :: (a / ScalaOutDim).toString :: "-cp" :: classPathS :: files).toArray
     scala.tools.nsc.Main.process(args)
   }

   def clean(a: Archive, in: FilePath) {
     delete(a / Dim("bin"))
   }
}
