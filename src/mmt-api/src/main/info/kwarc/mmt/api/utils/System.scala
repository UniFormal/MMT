package info.kwarc.mmt.api.utils

import java.util.jar.JarFile
import info.kwarc.mmt.api._

import java.nio.charset.StandardCharsets

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
  lazy val runStyle: RunStyle = {
     val location = getClass.getProtectionDomain.getCodeSource.getLocation
     if (location == null)
        OtherStyle
     else {
       val path = URI.decode(location.getPath)
       val classFolder = File(path)
       if (classFolder.isFile) {
         if (classFolder.name == "mmt.jar") {
           if (classFolder.up.name == "deploy")
              FatJar(classFolder)
           else
              DownloadedFatJar(classFolder)
         } else {
           if (classFolder.up.up.name == "deploy") {
             ThinJars(classFolder.up.up)
           } else {
             ThinJars(classFolder.up.up.up.up / "deploy")
           }
         }
       } else {
         Classes(classFolder)
       }
     }
   }

  /** the manifest file (if run from fat jar) */
  lazy val manifest = (try {Some(getResourceAsString("/META-INF/MANIFEST.MF"))} catch {case e: Exception => None}) map File.readPropertiesFromString
  
  /** the version of MMT being used */
  lazy val version: String = {
    Option(this.getClass.getPackage.getImplementationVersion).getOrElse(getResourceAsString("versioning/system.txt") + "--localchanges")
  }

  /**
    * Gets the latest version of the MMT System using the GitHub API.
    * May throw any kind of exception if it fails -- this should be handled by the caller.
    * Returns a pair (version, url)
    */
  def getLatestVersion: (String, String) = {
    val json = JSONFromURL("https://api.github.com/repos/Uniformal/MMT/releases/latest").getOrElse(throw GeneralError("Unable to load JSON")).asInstanceOf[JSONObject]
    val version = json.getAsString("tag_name").stripPrefix("v").trim()
    val url = json.getAsString("html_url")
    (version, url)
  }

  /** the git version (branch) used by mmt, if available */
  lazy val gitVersion: Option[String] = runStyle match {
    case d: DeployRunStyle => OS.git(d.deploy, "symbolic-ref", "HEAD") match {
      case ShellCommand.Success(op) if op.startsWith("refs/heads/") => Some(op.substring("refs/heads/".length).trim)
      case _ => None
    }
    case _ => None
  }

  /** the time when this version of MMT (if far jar built with sbt) was built */
  lazy val buildTime: Option[String] = {
    manifest.flatMap(_.get("Build-Time"))
  }

  /** legal notices, required by certain licenses */
  lazy val legalNotices: String = MMTSystem.getResourceList("/legal/").sorted.collect({
      case s: String if s.endsWith(".txt") => MMTSystem.getResourceAsString("/legal/" + s)
    }).mkString("\n")

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
      if(!path.startsWith("/")){ return getResource("/" + path) }
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

  /** return a list of available resources at the given path */
  def getResourceList(path: String) : List[String] = runStyle match {
      // running from jar
      case _:IsFat | _:ThinJars | OtherStyle =>

        // find the path to the current jar
        val classPath = getClass.getName.replace(".", "/") + ".class"
        val myPath = getClass.getClassLoader.getResource(classPath)
        val jarPath = myPath.getPath.substring(5, myPath.getPath.indexOf("!"))

        // open the jar and find all files in it
        import scala.jdk.CollectionConverters._
        val entries = new JarFile(URI.decode(jarPath)).entries.asScala.map(e => "/" + e.getName)

        // find all the resources within the given path, then remove the prefix
        entries.filter(e => e.startsWith(path) && !e.endsWith("/")).map(_.stripPrefix(path)).toList

      // running from classes, list the source directory
      case rs: Classes =>
        val base = (rs.resources / path).toJava
        base.listFiles.map(f => { File(base).relativize(f).toString }).toList
    }
}
