package info.kwarc.mmt.jeditsetup

import info.kwarc.mmt.api._
import frontend._
import utils._
import utils.File._

import MMTSystem._

/** install methods for jEdit
  *
  * copies jars, modes, abbreviations etc. to jEdit settings directory
  * installation is idempotent
  */
class Setup extends ShellExtension("jeditsetup") {

  def helpText = "needed arguments: (install | install-jars | uninstall) [JEDIT/SETTINGS/FOLDER]"

  /** run method as ShellExtension */
  def run(shell: Shell, args: List[String]): Boolean = {
    val l = args.length
    val (installOpt,fat) = if (l >= 1) args(0) match {
      case "install" => (Some(true),true)
      case "uninstall" => (Some(false), true)
      case "install-jars" => (Some(true), false)
      case "uninstall-jars" => (Some(false), false)
      case _ => (None, true)
    } else (None, true)
    val jeditOpt = if (l >= 2) Some(File(args(1))) else OS.jEditSettingsFolder
    if (jeditOpt.isEmpty) {
      println("jEdit settings folder not found")
    }
    if (l <= 0 || l > 2 || installOpt.isEmpty || jeditOpt.isEmpty) {
      println(helpText)
      return true
    }
    val jedit = jeditOpt.get
    val install = installOpt.get
    if (install) {
      println("trying to install to " + jedit)
    } else {
      println("trying to uninstall from " + jedit)
    }
    doIt(shell, jedit, install, fat)
    true
  }
  
  /** the actual install/uninstall process
    *
    * @param jedit   the jEdit settings folder
    * @param install true/false for install/uninstall
    * @param rl      resource location
    */
  private def doIt(shell: Shell, jedit: File, install: Boolean, fat: Boolean) {
    val rl = shell.runStyle
    def getResource(path: String): String = {
      rl match {
        case _: IsFat | OtherStyle => MMTSystem.getResourceAsString(path)
        case s: DeployRunStyle => File.read(s.deploy.up / "src" / "jEdit-mmt" / "src" / "resources" / path)
      }
    }
    def getPluginResource(f: List[String]): String = getResource("/plugin/" + f.mkString("/"))
    def handleResourceLineWise(path: String)(proc: String => Unit) =
       stringToList(getResource(path), "\\n").foreach(proc)

    /** copies or deletes a file depending on install/uninstall, always overwrites existing files */
    def copyOrDeleteJar(dir: File, f: List[String], g: List[String]) {
      if (install) {
        copy(dir / f, jedit / g, true)
      } else {
        delete(jedit / g)
      }
    }
    /** copies or deletes a file depending on install/uninstall, always overwrites existing files */
    def copyOrDeleteResource(f: List[String], replace: Boolean) = {
      val file = jedit / f
      if (install) {
        if (!file.exists || replace) {
          println("writing: " + file)
          File.write(file, getPluginResource(f))
        }
        else {
          println("already exists, skipping: " + file)
        }
      } else {
        delete(file)
      }
    }
    // copy/delete the jars
    // see src/project/Utils.scala for sbt
    val mainJars = List("mmt-api.jar", "mmt-lf.jar", "MMTPlugin.jar", "mmt-specware.jar", "mmt-pvs.jar")
    val libJars = List("scala-library.jar", "scala-parser-combinators.jar", "scala-reflect.jar", "scala-xml.jar",
      "tiscaf.jar")
    val allJars = List("lfcatalog", "lfcatalog.jar") :: mainJars.map(List("main", _)) ::: libJars.map(List("lib", _))
    (jedit / "jars").mkdirs()
    if (install) {
      rl match {
        case rs: IsFat =>
          copyOrDeleteJar(rs.jar, Nil, List("jars", "MMTPlugin.jar"))
        case nf: DeployRunStyle =>
          if (fat)
            copyOrDeleteJar(nf.deploy, List("mmt.jar"), List("jars", "MMTPlugin.jar"))
          else
            allJars.foreach(f => copyOrDeleteJar(nf.deploy, f, List("jars", f.last)))
        case OtherStyle =>
          println("cannot find jar files to install")
      }
    } else {
        allJars.foreach(f => delete(jedit / List("jars", f.last)))
    }
    // modes
    // * copy/delete the mode files
    val modeFiles = List("mmt.xml", "mmtlog.xml", "msl.xml")
    modeFiles.foreach { e => copyOrDeleteResource(List("modes", e), true) }
    // * read, update, write the catalog file
    val scat = "/plugin/modes/catalog"
    val jcat = jedit / "modes" / "catalog"
    var newCatalog: List[String] = Nil
    val modeEntries = modeFiles.map(e => "FILE=\"" + e + "\"")
    def isMMTEntry(line: String) = modeEntries.exists(line.contains)
    // read current catalog without MMT entries
    // write new catalog
    if (!jcat.exists) {
      newCatalog = "</MODES>" :: newCatalog ::: List("<MODES>")
    } else {
      File.ReadLineWise(jcat) { line =>
        if (install) {
          if (line.contains("</MODES>")) {
            // append MMT modes if installing
            handleResourceLineWise(scat) { l => newCatalog ::= l }
          }
        }
        if (!isMMTEntry(line))
          newCatalog ::= line
      }
    }
    if (install || jcat.exists) {
      println("updating " + jcat)
      File.WriteLineWise(jcat, newCatalog.reverse)
    }
    // abbrevs
    val sabb = "/plugin/abbrevs"
    val jabb = jedit / "abbrevs"
    var newAbbrevs: List[String] = Nil
    var remove = false
    // read current abbrevs without MMT abbrevs
    if (jabb.exists) File.ReadLineWise(jabb) { line =>
      if (install)
        newAbbrevs ::= line
      else {
        if (remove && line.startsWith("["))
          remove = false
        if (line.trim == "[mmt]")
          remove = true
        if (!remove)
          newAbbrevs ::= line
      }
    }
    // append MMT abbrevs if installing
    if (install) {
      handleResourceLineWise(sabb) {l => newAbbrevs ::= l}
    }
    // write new abbrevs
    if (newAbbrevs.nonEmpty) {
      println("updating " + jabb)
      File.WriteLineWise(jabb, newAbbrevs.reverse)
    } else {
      delete(jabb)
    }
    // copy/delete pluginFolder
    val plug = List("plugins", "info.kwarc.mmt.jedit.MMTPlugin")
    copyOrDeleteResource(plug ::: List("startup.msl"), false)
    if (!install) {
      val d = jedit / plug
      if (d.isDirectory) {
        d.deleteDir
        println("deleting directory " + d)
      }
    }

    val propsFile = jedit / "properties"
    val propsOld = if (propsFile.exists) File.read(propsFile) else ""
    val archKey = "info.kwarc.mmt.jedit.MMTPlugin.archives="
    if (install) controller.getOAF.map { oaf =>
      val contentFolder = oaf.root
      println("adding property for content folder " + contentFolder)
      val encoded = contentFolder.toString.replace("\\", "\\\\").replace(":", "\\:").replace("=", "\\=")
      val newValues = archKey + encoded
      val propsNew = propsOld + newValues + "\n"
      File.write(propsFile, propsNew)
    } else {
      println("deleting property for content folder: " + archKey)
      val newLines = propsOld.split("\n").filter(!_.startsWith(archKey)).toList
      if (newLines.nonEmpty && newLines != List("")) {
        File.WriteLineWise(propsFile, newLines)
      } else delete(propsFile)
    }
  }

  private def delete(f: File) {
    if (f.exists && f.isFile) {
      f.delete
      println("deleting " + f)
    }
  }

  private def copy(from: File, to: File, overwrite: Boolean) {
    if (to.exists && !overwrite) {
      println("warning: " + to + " already exists, skipping; you probably want to run uninstall first")
    } else {
      println("copying " + from + " to " + to)
      val success = File.copy(from, to, overwrite)
      if (!success) {
        println("warning: " + from + " does not exist! (skipping)")
        println("jedit setup may be incomplete or inconsistent.")
      }
    }
  }
}
