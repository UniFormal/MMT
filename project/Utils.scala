import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption._

object Utils {
   /** These methods are used by the target jedit/install to copy files to the local jEdit installation */
   val jEditJars = List("mmt-api.jar", "mmt-lf.jar", "MMTPlugin.jar", "mmt-specware.jar", "mmt-pvs.jar")
   def installJEditJars {
      val f = new File("jedit-settings-folder")
      if (f.exists) {
         val source = scala.io.Source.fromFile(f)
         val lines = source.getLines.toList
         source.close
         val line = lines.find(l => !l.startsWith("//")).get
         val jsf = new File(line + "/jars")
         jEditJars foreach {n => installJEditJar(jsf, n)}
      }
   }
   def installJEditJar(jsf: File, name: String) {
      val f = new File("../deploy/main/" + name)
      copy(f, new File(jsf,name))
   }
   /** copy a file */
   def copy(from: File, to: File) {
      if (from.lastModified > to.lastModified) {
         Files.copy(from.toPath, to.toPath, REPLACE_EXISTING)
         println(s"copying $from to $to")
      } else {
         println(s"not copying uptodate file $from to $to")
      }
   }
}