package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api._

object MMTSystem {
  /** information about how MMT was run, needed to access resources */
  sealed abstract class RunStyle
  /** run from expected folder structure */
  sealed abstract class DeployRunStyle extends RunStyle {
    /** the deploy folder */
    def deploy: File
  }
  /** run from self-contained mmt.jar */
  sealed trait IsFat {
    def jar: File
  }
  /** run from installed fat jar */
  case class FatJar(jar: File) extends DeployRunStyle with IsFat {
    def deploy = jar.up
  }
  /** run from a fat jar that is not part of the expected file structure */
  case class DownloadedFatJar(jar: File) extends RunStyle with IsFat
  /** run from mmt-api.jar in deploy folder */
  case class ThinJars(deploy: File) extends DeployRunStyle
  /** run from classes in src/mmt-api folder */
  case class Classes(classFolder: File) extends DeployRunStyle {
     val root = {
       var src = classFolder.up
       while (!src.isRoot && src.name != "src") src = src.up
       src.up
     }
     def deploy = root / "deploy"
     def api = root / "src" / "mmt-api"
     def projectFolder(name: String) = api.up / name
     def resources = api / "resources"
   }
  /** run in unknown way, in particular as part of jedit */
  case object OtherStyle extends RunStyle
  
  /** the [[RunStyle]] of the current run */
  lazy val runStyle = {
     val location = getClass.getProtectionDomain.getCodeSource.getLocation
     if (location == null)
        OtherStyle
     else {
       val classFolder = File(location.getPath)
       if (classFolder.isFile) {
         if (classFolder.name == "mmt.jar") {
           if (classFolder.up.name == "deploy")
              FatJar(classFolder)
           else
              DownloadedFatJar(classFolder)
         } else
           ThinJars(classFolder.up.up)
      } else {
         Classes(classFolder)
      }
     }
   }

   /** expected location of the user's mmtrc file */
   val userConfigFile = {
      OS.detect match {
         case Windows => OS.settingsFolder / "mmt" / "mmtrc"
         case MacOS => OS.settingsFolder / "mmt" / ".mmtrc"
         case _ => OS.settingsFolder / ".mmtrc"
      }
   }

   /** retrieves a resource from the jar or the resource folder depending on the [[RunStyle]], may be null */ 
   def getResource(path: String): java.io.InputStream = {
      runStyle match {
        case _:IsFat | _:ThinJars | OtherStyle =>
          getClass.getResourceAsStream(path) // the file inside the JAR
        case rs: Classes =>
          val file = rs.resources / path
          if (file.exists)
             new java.io.FileInputStream(file)
          else
             null
      }
   }
   def getResourceAsString(path: String): String = Option(getResource(path)) match {
     case Some(r) => utils.readFullStream(r)
     case None => throw GeneralError(s"cannot find resource $path (run style is $runStyle)")
   }
}
