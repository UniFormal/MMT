package info.kwarc.mmt.doc

import info.kwarc.mmt.api._
import utils._

object Setup {
   def main(args: Array[String]) {
    val l = args.length
    val systemFolder: File = if (l == 1) File(args(0)) else {
      println("usage: setup ROOT/FOLDER")
      sys.exit
    }
    val contentFolder = systemFolder.up/"content"
    val jEditSettingsFolderName = OS.detect match {
       case Windows => "jEdit"
       case MacOS => "jEdit"
       case _ => ".jedit"
    }
    val jEditSettingsFolder = OS.settingsFolder / jEditSettingsFolderName
    val dojEdit = jEditSettingsFolder.exists

    println("If the following shows any error messages, you can try looking at the source code.")
    println("The code is in info.kwarc.mmt.doc.Setup\n")

    println("using the following data")
    println("MMT system folder:     " + systemFolder)
    println("MMT content folder:    " + contentFolder)
    println("jEdit settings folder: " + (if (dojEdit) jEditSettingsFolder else "not found"))
    println("")

    val startupFile = systemFolder/"deploy"/"main"/"startup.msl"
    println("generating " + startupFile)
    val startup = s"""// generated startup file
log console
log+ oaf
oaf root $contentFolder
"""
    File.write(startupFile, startup)
    println("done\n")

    println("cloning content repositories (by calling git; make sure it's installed and on your path)")
    contentFolder.mkdirs
    val c = new frontend.Controller
    c.execFileAction(startupFile, None)
    try {
       c.cloneRecursively("MMT/examples")
    } catch {case e: Error =>
       println(e.toStringLong)
    }
    println("done\n")

    if (dojEdit) {
       val ext = OS.detect match {case Windows => "bat" case _ => "sh"}
       println("installing jEdit plugin")
       val res = ShellCommand.runAndPrint(None, (systemFolder/"deploy"/"jedit-plugin"/("setup." + ext)).toString, "install",
         jEditSettingsFolder.toString, contentFolder.toString)
       println("done\n")

       println("configuring sbt target for rebuilding and reinstalling the jEdit plugin from sources")
       File.write(systemFolder/"src"/"jedit-settings-folder", jEditSettingsFolder.toString)
       println("done\n")
    }


   }
}
