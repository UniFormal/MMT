package info.kwarc.mmt.jeditsetup

import info.kwarc.mmt.api.utils.File._
import info.kwarc.mmt.api.utils._

object Setup {
  def main(args: Array[String]) {
    val l = args.length
    val installOpt = if (l >= 1) args(0) match {
      case "install" => Some(true)
      case "uninstall" => Some(false)
      case _ => None
    } else None
    if (l != 2 || installOpt.isEmpty) {
      println("usage: setup (install | uninstall) /JEDIT/SETTINGS/FOLDER")
      sys.exit
    }
    val programLocation = File(getClass.getProtectionDomain.getCodeSource.getLocation.getPath).getParentFile
    val setup = programLocation.getParentFile
    val jedit = File(args(1))
    val install = installOpt.get

    println("trying to (un)install from " + setup + " to " + jedit)
    if (!setup.isDirectory || !jedit.isDirectory) {
      println("error: not valid directories")
      sys.exit
    }

    /** copies or delete a file depending on install/uninstall */
    def copyOrDelete(l: List[String]) {
      val f = FilePath(l)
      if (install) {
        copy(setup / f, jedit / f)
      } else {
        delete(jedit / f)
      }
    }
    // copy/delete the jars
    (setup / "jars").list.foreach { e =>
      if (e.endsWith(".jar")) {
        copyOrDelete(List("jars", e))
      }
    }
    // modes
    // * copy/delete the mode files
    val modeFiles = (setup / "modes").list.filter(e => e.endsWith(".xml"))
    modeFiles.foreach { e => copyOrDelete(List("modes", e)) }
    // * read, update, write the catalog file
    val scat = setup / "modes" / "catalog"
    val jcat = jedit / "modes" / "catalog"
    var newCatalog: List[String] = Nil
    val modeEntries = modeFiles.map(e => "FILE=\"" + e + "\"")
    File.ReadLineWise(jcat) { line =>
      if (install) {
        if (line.contains("</MODES>")) {
          File.ReadLineWise(scat) { l => newCatalog ::= l }
        }
        newCatalog ::= line
      } else {
        if (!modeEntries.exists(e => line.contains(e)))
          newCatalog ::= line
      }
    }
    if (!jcat.exists) {
      newCatalog = "</MODES>" :: newCatalog ::: List("<MODES>")
    }
    println("updating " + jcat)
    File.WriteLineWise(jcat, newCatalog.reverse)
    // abbrevs
    val sabb = setup / "abbrevs"
    val jabb = jedit / "abbrevs"
    var newAbbrevs: List[String] = Nil
    var remove = false
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
    if (install) {
      File.ReadLineWise(sabb) { l => newAbbrevs ::= l }
    }
    println("updating " + jabb)
    File.WriteLineWise(jabb, newAbbrevs.reverse)
    // copy/delete pluginFolder
    val plug = List("plugins", "info.kwarc.mmt.jedit.MMTPlugin")
    copyOrDelete(plug ::: List("startup.msl"))
    val mars = plug ::: List("mars")
    (setup / FilePath(mars)).list.foreach { e =>
      if (e.endsWith(".mar"))
        copyOrDelete(mars ::: List(e))
    }
    if (!install) {
      val d = jedit / FilePath(plug)
      if (d.isDirectory) {
        d.deleteDir
        println("deleting directory " + d)
      }
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
