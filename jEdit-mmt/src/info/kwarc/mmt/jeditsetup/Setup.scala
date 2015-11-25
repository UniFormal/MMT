package info.kwarc.mmt.jeditsetup

import info.kwarc.mmt.api.utils.File._
import info.kwarc.mmt.api.utils._

/** install script for jEdit
  *
  * copies jars, modes, abbreviations etc. to jEdit settings directory
  * installation is idempotent
  */
object Setup {
  /** This code works if run from setup script in the deploy/jedit-plugin folder
    *
    * @param args the location of the jedit settings folder
    */
  def main(args: Array[String]) {
    val l = args.length
    val installOpt = if (l >= 1) args(0) match {
      case "install" => Some(true)
      case "uninstall" => Some(false)
      case _ => None
    } else None
    if (l > 3 || installOpt.isEmpty) {
      println("usage: setup (install | uninstall) /JEDIT/SETTINGS/FOLDER [/DEFAULT/CONTENT/FOLDER]")
      sys.exit()
    }
    val jedit = File(args(1))
    val contentOpt = if (l == 3) Some(File(args(2))) else None
    val programLocation = File(getClass.getProtectionDomain.getCodeSource.getLocation.getPath).getParentFile
    val deploy = programLocation.getParentFile
    val setup = deploy / "jedit-plugin" / "plugin"
    val install = installOpt.get

    println("trying to (un)install from " + setup + " to " + jedit)
    if (!setup.isDirectory || !jedit.isDirectory) {
      println("error: not valid directories")
      sys.exit()
    }
    doIt(setup, deploy, jedit, install, contentOpt)
  }

  /** install/uninstall routine
    *
    * @param setup the deploy/jedit-plugin/plugin folder
    * @param jedit the jEdit settings folder
    * @param install true/false for install/uninstall
    * @param contentOpt the folder in which to look for archives
    */
  def doIt(setup: File, deploy: File, jedit: File, install: Boolean, contentOpt: Option[File]) {
    /** copies or deletes a file depending on install/uninstall */
    def copyFromOrDelete(dir: File, f: List[String]) {
      if (install) {
        copy(dir / f, jedit / f)
      } else {
        delete(jedit / f)
      }
    }
    def copyOrDelete(f: List[String]) = copyFromOrDelete(setup, f: List[String])
    // copy/delete the jars
    // see src/project/Utils.scala for sbt
    val mainJars = List("mmt-api.jar", "mmt-lf.jar", "MMTPlugin.jar", "mmt-specware.jar", "mmt-pvs.jar")
    val libJars = List("scala-library.jar", "scala-parser-combinators.jar", "scala-reflect.jar", "scala-xml.jar",
      "tiscaf.jar")
    val allJars = List("lfcatalog", "lfcatalog.jar") :: mainJars.map(List("main", _)) ++
      libJars.map(List("lib", _))
    allJars.foreach(copyFromOrDelete(deploy, _))
    // modes
    // * copy/delete the mode files
    val modeFiles = (setup / "modes").list.filter(_.endsWith(".xml"))
    modeFiles.foreach { e => copyOrDelete(List("modes", e)) }
    // * read, update, write the catalog file
    val scat = setup / "modes" / "catalog"
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
            File.ReadLineWise(scat) { l => newCatalog ::= l }
          }
        }
        if (!isMMTEntry(line))
          newCatalog ::= line
      }
    }
    println("updating " + jcat)
    File.WriteLineWise(jcat, newCatalog.reverse)
    // abbrevs
    val sabb = setup / "abbrevs"
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
      File.ReadLineWise(sabb) { l => newAbbrevs ::= l }
    }
    // write new abbrevs
    println("updating " + jabb)
    File.WriteLineWise(jabb, newAbbrevs.reverse)
    // copy/delete pluginFolder
    val plug = List("plugins", "info.kwarc.mmt.jedit.MMTPlugin")
    copyOrDelete(plug ::: List("startup.msl"))
    val mars = plug ::: List("mars")
    val setupmars = setup / mars
    if (setupmars.exists) {
      setupmars.list.foreach { e =>
        if (e.endsWith(".mar"))
          copyOrDelete(mars ::: List(e))
      }
    }
    if (!install) {
      val d = jedit / plug
      if (d.isDirectory) {
        d.deleteDir
        println("deleting directory " + d)
      }
    }

    if (install && contentOpt.isDefined) {
      println("adding property for content folder")
      val propsFile = jedit / "properties"
      val propsOld = if (propsFile.exists) File.read(propsFile) else ""
      val encoded = contentOpt.get.toString.replace("\\", "\\\\").replace(":", "\\:").replace("=", "\\=")
      val newValues = "info.kwarc.mmt.jedit.MMTPlugin.archives=" + encoded
      val propsNew = propsOld + "\n" + newValues
      File.write(propsFile, propsNew)
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
      java.nio.file.Files.copy(from.toPath, to.toPath)
      println("copying " + from + " to " + to)
    }
  }
}
