package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.utils

object MMTSystem {
  /** information about how MMT was run, needed to access resources */
  sealed abstract class RunStyle {
    /** the folder containing the jar file or the bin folder */
    def parentFolder: Option[File]
  }
  sealed abstract class NotFatJar extends RunStyle {
     /** the deploy folder */
     def deploy: File
  }
  /** run from self-contained mmt.jar */
  case class FatJar(jar: File) extends RunStyle {
     def parentFolder = Some(jar.up) 
  }
  /** run from mmt-api.jar in deploy folder */
  case class ThinJars(deploy: File) extends NotFatJar {
    def parentFolder = Some(deploy / "main")
  }
  /** run from classes in src/mmt-api folder */
  case class Classes(classFolder: File) extends NotFatJar {
     val root = {
       var src = classFolder.up
       while (!src.isRoot && src.name != "src") src = src.up
       src.up
     }
     def deploy = root / "deploy"
     def api = root / "src" / "mmt-api" / "trunk"
     def parentFolder = Some(api)
     def projectFolder(name: String) = api.up.up / name
     def resources = api / "resources"
   }
  /** run in unknown way, in particular as part of jedit */
   case object OtherStyle extends RunStyle {
     def parentFolder = None
   }
  
  /** the [[RunStyle]] of the current run */
  lazy val runStyle = {
     val location = getClass.getProtectionDomain.getCodeSource.getLocation
     if (location == null)
        OtherStyle
     else {
       val classFolder = File(location.getPath)
       if (classFolder.isFile) {
         if (classFolder.name == "mmt.jar")
           FatJar(classFolder)
         else
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
        case _:FatJar | _:ThinJars | OtherStyle =>
          getClass.getResourceAsStream(path) // the file inside the JAR
        case rs: Classes =>
          val file = rs.resources / path
          if (file.exists)
             new java.io.FileInputStream(file)
          else
             null
      }
   }
   def getResourceAsString(path: String): String = scala.io.Source.fromInputStream(getResource(path)).getLines.mkString("\n")
}
