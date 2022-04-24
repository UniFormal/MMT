import java.nio.file.Files
import java.nio.file.StandardCopyOption._

import sbt.Keys.packageBin
import sbt._

object Utils {

  // 
  val utils = settingKey[Utils]("Utils")

  def apply(base: java.io.File) = new Utils(File(base))

  def error(s: String) = throw new Exception(s)

  // ************************************************** deploy-specific code (see also the TaskKey's deploy and deployFull)

  /**
    * packages the compiled binaries and copies to deploy
    */
  def deployMathHub(target: File): Def.Initialize[Task[Unit]] =
    packageBin in Compile map { jar => Utils.deployTo(target)(jar) }

  /*
   * copies files to deploy folder
   */
  def deployTo(target: File)(jar: sbt.File): Unit = {
    Files.copy(jar.toPath, target.toPath, REPLACE_EXISTING)
    println("copied file: " + jar)
    println("to file: " + target)
  }

  /**
    * packages the compiled binaries and copies to deploy
    */
  def deployPackage(name: String): Def.Initialize[Task[Unit]] = Def.taskDyn {
    val jarFile = (packageBin in Compile).value
    Def.task {
      Utils.deployTo(utils.value.deploy / name)(jarFile)
    }
  }

  // ************************************************** file system utilities

  /** copy a file */
  def copy(from: File, to: File) {
    println(s"copying $from to $to")
    if (!from.exists) {
      Utils.error(s"error: file $from not found (when trying to copy it to $to)")
    } else if (!to.exists || from.lastModified > to.lastModified) {
      Files.copy(from.toPath, to.toPath, REPLACE_EXISTING)
    } else {
      println("skipped (up-to-date)")
    }
    println("\n")
  }

  /**
    * Recursively deletes a given folder
    *
    * @param log
    * @param path
    */
  def delRecursive(log: Logger, path: File): Unit = {
    def delRecursive(path: File): Unit = {
      path.listFiles foreach { f =>
        if (f.isDirectory) delRecursive(f)
        else {
          f.delete()
          log.debug("deleted file: " + path)
        }
      }
      path.delete()
      log.debug("deleted directory: " + path)
    }

    if (path.exists && path.isDirectory) delRecursive(path)
    else log.warn("ignoring missing directory: " + path)
  }
}

/**
  * @param base File path to the "<MMT repo>/src" directory.
  */
class Utils(base: File) {
  /** MMT root directory */
  val root: File = (base / "..").canonical
  /** source folder */
  val src: File = root / "src"

  /** MMT deploy directory */
  val deploy: File = root / "deploy"
  /** MMT deploy lib directory */
  val lib: File = deploy / "lib"

  /**
    * settings syntax is: 1 instance of 'key: value' per line
    */
  val settingsFile: File = src / "mmt-sbt-settings"

  import collection.mutable.Map

  def settings: Map[String, String] = if (settingsFile.exists) File.readProperties(settingsFile) else Map[String, String]()

  /** executes a shell command (in the src folder) */
  def runscript(command: List[String], home: File = src, waitFor: Boolean = true) {
    println("running: " + command.mkString(" "))
    val pb = sys.process.Process(command, home.getAbsoluteFile)
    if (waitFor) pb.! else pb.run(false)
  }

  /** runs the mmt.jar command in a sister folder MMT-test */
  def testSetup {
    val test = (root / ".." / "MMT-test").canonical
    val jEdit = test / "jEdit"
    jEdit.mkdirs
    println("starting and killing jEdit so that it creates a fresh settings directory")
    val initJEDit = s"jedit -settings=${jEdit}".split("\\s").toList
    runscript(initJEDit, waitFor = false)
    // need to wait a while before killing because it takes jEdit a while to start
    import scala.concurrent.ExecutionContext.Implicits.global
    scala.concurrent.Future {
      Thread.sleep(10000)
      println("killing jEdit now (from a different thread that has waited a while to give it time to start up)")
      settings.get(killJEdit).foreach {x => runscript(List(x),waitFor = true)}
    }

    println("copying mmt.jar and running setup (including jEdit setup)")
    Utils.deployTo(test / "mmt.jar")(deploy / "mmt.jar")
    // starts but can't read interactive input, echo | doesn't work either, so it's fully automated here
    val command = s"java -jar mmt.jar :setup --auto --auto devel $jEdit".split("\\s").toList
    runscript(command, test, waitFor = true)

    println("starting jEdit with a test file")
    val plFile = test / "MMT-content" / "MMT" / "examples" / "source" / "logic" / "pl.mmt"
    runscript(initJEDit ::: List(plFile.toString), waitFor = false)
  }

  // ************************************************** jEdit-specific code

  // keys for build settings
  val jeditSettingsFolder = "jedit-settings-folder"
  val startJEDit = "start-jedit"
  val killJEdit = "kill-jedit"


  /** MMT jEditPlugin release jars directory */
  val jEditPluginRelease: File = deploy / "jedit-plugin" / "plugin" / "jars"

  /** These methods are used by the target jedit/install to copy files to the local jEdit installation */
  /** copy MMT jar to jEdit settings directory */
  def installJEditJars {
    settings.get(killJEdit).foreach {x => runscript(List(x), waitFor = true)}
    Thread.sleep(1000)
    val fname = settings.get(jeditSettingsFolder).getOrElse {
      Utils.error(s"cannot copy jars because there is no setting '$jeditSettingsFolder' in $settingsFile")
      return
    }
    val jsf = File(fname) / "jars"
    copyJEditJars(jsf)
    settings.get(startJEDit).foreach {x => runscript(List(x), waitFor = false)}
  }

  /** copy all jars to jEditPluginRelease */
  def releaseJEditJars {
    copyJEditJars(jEditPluginRelease)
  }

  /** copy all jEdit jars to a directory */
  def copyJEditJars(to: File) {
    Utils.copy(deploy / "mmt.jar", to / "MMTPlugin.jar")
    // all other jars are bundled with the above
    // val jEditDeps = List("scala-library.jar","scala-parser-combinators.jar","scala-reflect.jar","scala-xml.jar","tiscaf.jar")
    // jEditDeps.foreach {f => copy(deploy/"lib"/f, to/f)}
    // copy(deploy/"lfcatalog"/"lfcatalog.jar", to/"lfcatalog.jar")
  }

  // ************************************************** MathHub-specific code

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
}