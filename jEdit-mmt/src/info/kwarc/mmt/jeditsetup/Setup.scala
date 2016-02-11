package info.kwarc.mmt.jeditsetup

import info.kwarc.mmt.api._
import frontend._
import utils._
import utils.File._

/** install methods for jEdit
  *
  * copies jars, modes, abbreviations etc. to jEdit settings directory
  * installation is idempotent
  */
class Setup extends ShellExtension("jeditsetup") {

  def helpText = "needed arguments: (install | uninstall) [JEDIT/SETTINGS/FOLDER]"

  /** run method as ShellExtension */
  def run(args: List[String]): Boolean = {
    val l = args.length
    val installOpt = if (l >= 1) args(0) match {
      case "install" => Some(true)
      case "uninstall" => Some(false)
      case _ => None
    } else None
    val jeditOpt = if (l >= 2) Some(File(args(1))) else OS.jEditSettingsFolder
    if (jeditOpt.isEmpty) {
        println("jEdit settings folder not found")
    }
    if (!MMTSystem.runFromJar) {
      println("jeditsetup can only be run from a .jar")
      return true
    }
    if (l <= 0 || l > 2 || installOpt.isEmpty || jeditOpt.isEmpty) {
      println(helpText)
      return true
    }
    val jarFile: File = MMTSystem.classFolder
    val programLocation: File = jarFile.up
    val (deploy, fat) = if (programLocation.getName == "main") {
      (programLocation.up, false)
    } else {
      (jarFile, true)
    }
    val jedit = jeditOpt.get
    val install = installOpt.get

    if (install) {
      println("trying to install to " + jedit)
    } else
    {
      println("trying to uninstall from " + jedit)
    }
    doIt(deploy, jedit, install, fat)
    true
  }

  /** the actual install/uninstall process
    *
    * @param jedit   the jEdit settings folder
    * @param install true/false for install/uninstall
    * @param fat     use the fat mmt.jar
    */
  private def doIt(deploy: File, jedit: File, install: Boolean, fat: Boolean) {
    /** copies or deletes a file depending on install/uninstall */
    def copyFromOrDelete(dir: File, f: List[String], g: List[String]) {
      if (install) {
        copy(dir / f, jedit / g)
      } else {
        delete(jedit / g)
      }
    }
    // copy/delete the jars
    // see src/project/Utils.scala for sbt
    val mainJars = List("mmt-api.jar", "mmt-lf.jar", "MMTPlugin.jar", "mmt-specware.jar", "mmt-pvs.jar")
    val libJars = List("scala-library.jar", "scala-parser-combinators.jar", "scala-reflect.jar", "scala-xml.jar",
      "tiscaf.jar")
    val allJars = List("lfcatalog", "lfcatalog.jar") :: mainJars.map(List("main", _)) ++
      libJars.map(List("lib", _))
    (jedit / "jars").mkdirs()
    if (fat) {
      copyFromOrDelete(deploy, Nil, List("jars", "MMTPlugin.jar"))
    } else {
      allJars.foreach(f => copyFromOrDelete(deploy, f, List("jars", FilePath(f).name)))
    }
    // modes
    def copyOrDelete(f: List[String]) = {
      val file = jedit / f
      if (install) {
        if (file.exists) {
          println("warning: " + file + " already exists, skipping; you probably want to run uninstall first")
        }
        else {
          println("creating " + file)
          File.write(file, MMTSystem.getResourceAsString("/plugin/" + f.mkString("/")))
        }
      } else {
        delete(file)
      }
    }
    // * copy/delete the mode files
    val modeFiles = List("mmt.xml", "mmtlog.xml", "msl.xml")
    modeFiles.foreach { e => copyOrDelete(List("modes", e)) }
    // * read, update, write the catalog file
    val scat = "/plugin/modes/catalog"
    val jcat = jedit / "modes" / "catalog"
    var newCatalog: List[String] = Nil
    val modeEntries = modeFiles.map(e => "FILE=\"" + e + "\"")
    def isMMTEntry(line: String) = modeEntries.exists(line.contains)
    def handleResourceLineWise(path : String)(proc: String => Unit) =
      scala.io.Source.fromInputStream(MMTSystem.getResource(path)).getLines.foreach(proc)
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
      handleResourceLineWise(sabb) { l => newAbbrevs ::= l }
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
    copyOrDelete(plug ::: List("startup.msl"))
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
      if (newLines != List("")) {
        File.WriteLineWise(propsFile, newLines)
      } else delete(propsFile)
    }
  }

  private def delete(f: File) {
    f.delete
    println("deleting " + f)
  }

  private def copy(from: File, to: File) {
    if (to.exists) {
      println("warning: " + to + " already exists, skipping; you probably want to run uninstall first")
    } else {
      to.getParentFile.mkdirs
      if (from.exists) {
        java.nio.file.Files.copy(from.toPath, to.toPath)
        println("copying " + from + " to " + to)
      } else {
        println("warning: " + from + " does not exist! (skipping)")
        println("jedit setup may be incomplete or inconsistent.")
      }
    }
  }
}
