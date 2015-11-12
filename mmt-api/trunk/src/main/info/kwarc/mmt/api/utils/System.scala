package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.utils

object MMTSystem {
   /**
     * @return the jar file or the folder containing the class files
     */
   lazy val classFolder = File(getClass.getProtectionDomain.getCodeSource.getLocation.getPath)
   lazy val runFromJar = classFolder.name.endsWith(".jar")

   /**
     * @return if run from jar, the folder containing the jar file; otherwise, the project folder
     */
   def rootFolder = if (runFromJar)
      classFolder.up
   else
   // this assumes classes are compiled into, e.g., trunk/bin/info/...
      classFolder.up


   val userConfigFile = {
      OS.detect match {
         case Windows => OS.settingsFolder / "mmt" / "mmtrc"
         case MacOS => OS.settingsFolder / "mmt" / ".mmtrc"
         case _ => OS.settingsFolder / ".mmtrc"
      }
   }

   def getResource(path: String): java.io.InputStream = {
      val stream = getClass.getResourceAsStream(path) // the file inside the JAR
      if (stream != null) {
         stream
      } else {
         // run from classes
         val file = utils.MMTSystem.rootFolder / "resources" / path
         if (file.exists)
            new java.io.FileInputStream(file)
         else
            null
      }
   }
   def getResourceAsString(path: String): String = scala.io.Source.fromInputStream(getResource(path)).getLines.mkString("\n")
}
