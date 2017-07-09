import java.nio.file.Files
import java.nio.file.StandardCopyOption._

object Utils {
   /** MMT root directory */
   val root = File("..")
   /** source folder */
   val src = root / "src"
   /** MMT deploy directory */
   val deploy = root/"deploy"

   /**
    * settings syntax is: 1 instance of 'key: value' per line 
    */
   val settingsFile = src / "mmt-sbt-settings"
   import collection.mutable.Map
   lazy val settings: Map[String,String] = if (settingsFile.exists) File.readProperties(settingsFile) else Map[String,String]()
   
   /** executes a shell command (in the src folder) */
   def runscript(command: String) = sys.process.Process(Seq(command), src.getAbsoluteFile).!!
   
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
   
   private val mathhub = "mathhub-folder"
   lazy val mathhubFolder: File = {
     settings.get(mathhub) match {
       case None =>
         println(s"no key $mathhub in $settingsFile")
         throw new Exception
       case Some(s) =>
         File(s)
     }
   }
   
   // jEdit-specific utilities
   
   // keys for build settings
   val jeditSettingsFolder = "jedit-settings-folder"
   val startJEDit = "start-jedit"
   val killJEdit  = "kill-jedit"
   
   
   /** MMT jEditPlugin release jars directory */
   val jEditPluginRelease = deploy/"jedit-plugin"/"plugin"/"jars"
   
   /** These methods are used by the target jedit/install to copy files to the local jEdit installation */
   /** jars in deploy/main */
   val jEditJars = List("mmt-api.jar", "mmt-lf.jar", "mmt-lfx.jar", "MMTPlugin.jar", "mmt-specware.jar", "mmt-mizar.jar", "mmt-pvs.jar", "mmt-odk.jar")
   /** jars in deploy/lib */
   val jEditDeps = List("scala-library.jar","scala-parser-combinators.jar","scala-reflect.jar","scala-xml.jar","tiscaf.jar")
   /** copy all jars to jEdit settings directory */
   def installJEditJars {
      settings.get(killJEdit).foreach {x => runscript(x)}
      val fname = settings.get(jeditSettingsFolder).getOrElse {
        println(s"cannot copy jars because there is no setting '$jeditSettingsFolder' in $settingsFile")
        return
      }
      val jsf = File(fname) / "jars"
      copyJEditJars(jsf)
      settings.get(startJEDit).foreach {x => runscript(x)}
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
}
