import java.nio.file.Files
import java.nio.file.StandardCopyOption._

case class File(toJava: java.io.File) {
   def /(s: String) = File(new java.io.File(toJava, s))
}
object File {
   def apply(s: String):File = File(new java.io.File(s))
   implicit def toJava(f: File) = f.toJava 
}


object Utils {
   val root = File("..")
   val deploy = root/"deploy"
   /** These methods are used by the target jedit/install to copy files to the local jEdit installation */
   val jEditJars = List("mmt-api.jar", "mmt-lf.jar", "MMTPlugin.jar", "mmt-specware.jar", "mmt-pvs.jar")
   val jEditDeps = List("scala-library.jar","scala-parser-combinators.jar","scala-reflect.jar","scala-xml.jar",
         "tiscaf.jar","lfcatalog.jar")
   def installJEditJars {
      val f = File("jedit-settings-folder")
      if (f.exists) {
         val source = scala.io.Source.fromFile(f)
         val lines = source.getLines.toList
         source.close
         val line = lines.find(l => !l.startsWith("//")).get
         val jsf = File(line)/"jars"
         (jEditJars:::jEditDeps) foreach {n => installJEditJar(jsf, n)}
      } else {
         println("not installed: create a file "+f.toString+" containing the path to jEdit's settings directory")
      }
   }
   def installJEditJar(jsf: File, name: String) {
      val f = deploy/"main"/name
      copy(f, jsf/name)
   }
   def releaseJEditJars {
      val pluginRelease = deploy/"jedit-plugin"/"plugin"/"jars"
      jEditJars.foreach {f => copy(deploy/"main"/f, pluginRelease/f)}
      jEditDeps.foreach {f => copy(deploy/"lib"/f, pluginRelease/f)}
   }
   /** copy a file */
   def copy(from: File, to: File) {
      if (!from.exists) {
         println("file to copy not found "+from)
      }
      if (from.lastModified > to.lastModified) {
         Files.copy(from.toPath, to.toPath, REPLACE_EXISTING)
         println(s"copying $from to $to")
      } else {
         println(s"not copying uptodate file $from to $to")
      }
   }
}