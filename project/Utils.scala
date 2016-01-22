import java.nio.file.Files
import java.nio.file.StandardCopyOption._

case class File(toJava: java.io.File) {
   def /(s: String) = File(new java.io.File(toJava, s))
   override def toString = toJava.toString
}
object File {
   def apply(s: String):File = File(new java.io.File(s))
   implicit def toJava(f: File) = f.toJava
   implicit def fromJava(f: java.io.File) = File(f)
}


object Utils {
   /** MMT root directory */
   val root = File("..")
   /** MMT deploy directory */
   val deploy = root/"deploy"
   /** MMT jEditPlugin release jars directory */
   val jEditPluginRelease = deploy/"jedit-plugin"/"plugin"/"jars"
   /** These methods are used by the target jedit/install to copy files to the local jEdit installation */
   /** jars in deploy/main */
   val jEditJars = List("mmt-api.jar", "mmt-lf.jar", "MMTPlugin.jar", "mmt-specware.jar", "mmt-pvs.jar", "mmt-odk.jar")
   /** jars in deploy/lib */
   val jEditDeps = List("scala-library.jar","scala-parser-combinators.jar","scala-reflect.jar","scala-xml.jar",
         "tiscaf.jar")
   /** copy all jars to jEdit settings directory */
   def installJEditJars {
      val f = File("jedit-settings-folder")
      if (f.exists) {
         val source = scala.io.Source.fromFile(f)
         val lines = source.getLines.toList
         source.close
         val line = lines.find(l => !l.startsWith("//")).get
         val jsf = File(line)/"jars"
         copyJEditJars(jsf)
      } else {
         println("not installed: create a file "+f.toString+" containing the path to jEdit's settings directory")
      }
   }
   /** copy all jars to jEditPluginRelease */
   def releaseJEditJars {
      copyJEditJars(jEditPluginRelease)
   }
   /** copy all jEdit jars to a directory */
   def copyJEditJars(to: File) {
      jEditJars.foreach {f => copy(deploy/"main"/f, to/f)}
      jEditDeps.foreach {f => copy(deploy/"lib"/f, to/f)}
      copy(deploy/"lfcatalog"/"lfcatalog.jar", to/"lfcatalog.jar")
   }
   /** copy a file */
   def copy(from: File, to: File) {
      println(s"copying $from to $to")
      if (!from.exists) {
         println("error: file to copy not found")
      } else if (!to.exists || from.lastModified > to.lastModified) {
         Files.copy(from.toPath, to.toPath, REPLACE_EXISTING)
      } else {
         println("skipped (up-to-date)")
      }
      println("\n")
   }
}
