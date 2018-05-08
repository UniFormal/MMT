package info.kwarc.mmt.jeditsetup

import info.kwarc.mmt.api._
import frontend._
import utils._
import utils.File._

import MMTSystem._

import info.kwarc.mmt.jedit._

/** install methods for jEdit
  *
  * copies jars, modes, abbreviations etc. to jEdit settings directory
  * installation is idempotent
  *
  */
class Setup extends ShellExtension("jeditsetup") {
  def helpText = "needed arguments: (install | uninstall) [JEDIT/SETTINGS/FOLDER]"

  /** a function tog log a message */
  val log: String => Unit = println

  /** run method as ShellExtension */
  def run(shell: Shell, args: List[String]): Boolean = {
    val l = args.length
    if (l <= 0 || l > 2) {
      log(helpText)
      return true
    }
    val jeditOpt = if (l >= 2) Some(File(args(1))) else OS.jEditSettingsFolder
    val jedit = jeditOpt.getOrElse {
      log("The jEdit settings folder was not found. If you have installed jEdit just now, you may have to run it once so that it creates the folder.")
      return true
    }
    val doIt = new DoIt(shell, jedit)
    if (args(0) == "customize") {
      doIt.customize
    } else {
      val installOpt = args(0) match {
        case "install" => Some(true)
        case "uninstall" => Some(false)
        case _ => None
      }
      val install = installOpt.getOrElse {
        log(helpText)
        return true
      }
      if (install) {
        log("trying to install to " + jedit)
      } else {
        log("trying to uninstall from " + jedit)
      }
      doIt.install(install)
    }
    true
  }


  private class DoIt(shell: Shell, jedit: File) {
    val jarFolder = jedit / "jars"
    val propsFile = jedit / "properties"
    val keymapPath = List("keymaps", "imported_keys.props")

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

    /** merge properties from a resource into a jEdit file */
    def mergeProps(propsOldFile: File, propsAddResource: List[String]) {
       log("merging " + propsAddResource.mkString("/") + " into " + propsOldFile)
       var propsAdd = utils.stringToList(getPluginResource(propsAddResource), "\\n").flatMap {line =>
         val i = line.indexOf("=")
         if (!line.startsWith("#") && i != -1)
           List((line.substring(0,i-1), line))
         else
           Nil
       }
       val propsOld = utils.stringToList(if (propsOldFile.exists) File.read(propsOldFile) else "", "\\n")
       // override existing properties
       var propsNew = propsOld.map {old =>
         propsAdd.find{case (key,_) => old.startsWith(key)} match {
           case None => old
           case Some((key,nw)) =>
             propsAdd = propsAdd.filterNot(_._1 == key)
             nw
         }
       }
       // append remaining new properties
       propsNew = propsNew ::: propsAdd.map(_._2)
       File.WriteLineWise(propsOldFile, propsNew)
    }

    /** the actual install/uninstall process
      *
      * @param jedit   the jEdit settings folder
      * @param install true/false for install/uninstall
      * @param fat install fat jar
      */
    def install(install: Boolean) {
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
            log("writing: " + file)
            File.write(file, getPluginResource(f))
          }
          else {
            log("already exists, skipping: " + file)
          }
        } else {
          delete(file)
        }
      }
      // copy/delete the jar
      jarFolder.mkdirs
      if (install) {
        rl match {
          case rs: IsFat =>
            copyOrDeleteJar(rs.jar, Nil, List("jars", "MMTPlugin.jar"))
          case nf: DeployRunStyle =>
            copyOrDeleteJar(nf.deploy, List("mmt.jar"), List("jars", "MMTPlugin.jar"))
          case OtherStyle =>
            log("cannot find jar files to install")
        }
      } else {
        delete(jedit / "jars" / "MMTPlugin.jar")
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
        log("updating " + jcat)
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
        log("updating " + jabb)
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
          log("deleting directory " + d)
        }
      }
      // add properties that depend on MMT installation folder
      val propsOld = if (propsFile.exists) File.read(propsFile) else ""
      val archKey = MMTOptions.archives.jeditKey + "="
      if (install) controller.getMathHub.map {mh =>
        val contentFolder = mh.local
        log("adding property for content folder " + contentFolder)
        val encoded = contentFolder.toString.replace("\\", "\\\\").replace(":", "\\:").replace("=", "\\=")
        val newValues = archKey + encoded
        val propsNew = propsOld + newValues + "\n"
        File.write(propsFile, propsNew)
      } else {
        log("deleting property for content folder: " + archKey)
        val newLines = propsOld.split("\n").filter(!_.startsWith(archKey)).toList
        if (newLines.nonEmpty && newLines != List("")) {
          File.WriteLineWise(propsFile, newLines)
        } else delete(propsFile)
      }
    }


    val jars = List(("ErrorList", "2.3"), ("SideKick", "1.8"), ("Hyperlinks","1.1.0"), ("Console","5.1.4"), /*("ContextMenu","0.4"),*/ ("BufferTabs","1.2.4"))
    /** installs plugin dependencies and useful properties */
    def customize {
       // download jars from jEdit plugin central
       jars.foreach {case (name,version) =>
         val target = jarFolder / (name + ".jar")
         if (!target.exists) {
           val url = URI(s"https://sourceforge.net/projects/jedit-plugins/files/$name/$version/$name-$version-bin.zip")
           val zip = target.setExtension("zip")
           log("downloading " + url)
           try {
             File.download(url, zip)
             File.unzip(zip, jarFolder)
           } catch {
             case e: Exception => log(e.getMessage)
           } finally {
             zip.delete
           }
         }
       }

       // add all properties and keymaps
       mergeProps(propsFile, List("properties"))
       mergeProps(jedit / keymapPath, keymapPath)
    }
  }

  private def delete(f: File) {
    if (f.exists && f.isFile) {
      f.delete
      log("deleting " + f)
    }
  }

  private def copy(from: File, to: File, overwrite: Boolean) {
    if (to.exists && !overwrite) {
      log("warning: " + to + " already exists, skipping; you probably want to run uninstall first")
    } else {
      log("copying " + from + " to " + to)
      val success = File.copy(from, to, overwrite)
      if (!success) {
        log("warning: " + from + " does not exist! (skipping)")
        log("jedit setup may be incomplete or inconsistent.")
      }
    }
  }
}
