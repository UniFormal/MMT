package info.kwarc.mmt.api.utils

/** type of operating systems as used in [[OS]] */
abstract class OS
abstract class Unix extends OS
case object Linux extends Unix
case object MacOS extends Unix
case object BSD extends Unix
case object Windows extends OS
case object ProbablyUnix extends Unix

/** abstractions for OS-specific behavior */
object OS {
  /** detects the underlying OS */
  def detect = {
     val os = System.getProperty("os.name")
     if (os.startsWith("Windows")) Windows
     else if (os.startsWith("Mac")) MacOS
     else if (os.startsWith("Linux")) Linux
     else if (os.contains("BSD")) BSD
     else ProbablyUnix
  }
  
  /** the most likely folder to store application-specific settings */
  def settingsFolder = {
     lazy val uh = File(System.getProperty("user.home"))
     detect match {
        case Windows => File(System.getenv("APPDATA"))
        case MacOS => uh / "Library"
        case os : Unix => uh
     }
  }
  
  def jEditSettingsFolder = {
     val jEditSettingsFolderName = OS.detect match {
       case Windows => "jEdit"
       case MacOS => "jEdit"
       case _ => ".jedit"
     }
     val f = OS.settingsFolder / jEditSettingsFolderName
     if (f.isDirectory) Some(f) else None
  }
}