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
  
  /** the default location of the jEdit settings folder, as specified by the jEdit documentation */
  def jEditSettingsFolder = {

    (OS.detect match {
      case Windows => List(File(System.getenv("APPDATA")) / "jEdit")
      case MacOS => List(File(System.getProperty("user.home")) / "Library" / "jEdit")
      case os: Unix => List(
        File(System.getProperty("user.home")) / ".jEdit",
        File(System.getProperty("user.home")) / ".jedit"
      )
    }).find(_.isDirectory)
  }
}