package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.utils

object MMTSystem {
  /** information about how MMT was run, needed to access resources */
  sealed abstract class RunStyle {
    /** the folder containing the jar file or the bin folder */
    def parentFolder: File
  }
  sealed abstract class NotFatJar extends RunStyle {
     /** the deploy folder */
     def deploy: File
   }
  /** run from self-contained mmt.jar */
  case class FatJar(jar: File) extends RunStyle {
     def parentFolder = jar.up 
  }
  /** run from mmt-api.jar in deploy folder */
  case class ThinJars(deploy: File) extends NotFatJar {
    def parentFolder = deploy / "main"
  }
  /** run from classes in src/mmt-api folder */
  case class Classes(root: File) extends NotFatJar {
     def deploy = root / "deploy"
     def parentFolder = root / "src" / "mmt-api" / "trunk"
     def api = parentFolder
     def projectFolder(name: String) = api.up.up / name
     def resources = parentFolder / "resources"
   }
     
   /**
     * @return the jar file or the folder containing the class files
     */

  /** the [[RunStyle]] of the current run */
  lazy val runStyle = {
     val classFolder = File(getClass.getProtectionDomain.getCodeSource.getLocation.getPath)
     if (classFolder.isFile) {
       if (classFolder.name == "mmt.jar")
         FatJar(classFolder)
       else
         ThinJars(classFolder.up.up)
    } else {
       var src = classFolder.up
       while (!src.isRoot && src.name != "src") src = src.up
       Classes(src.up)
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
        case _:FatJar | _:ThinJars =>
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
