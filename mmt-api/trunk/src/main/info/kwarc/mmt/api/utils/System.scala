package info.kwarc.mmt.api.utils

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
}