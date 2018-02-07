package info.kwarc.mmt.doc

import info.kwarc.mmt.api._
import frontend._
import utils._

import MMTSystem._

/**
 * creates folder structure and a few files for MMT
 *
 * if run without arguments: asks for root folder and creates folders systems/MMT and content/MathHub, installs jEdit plugin if jEdit found
 * if run with arguments: fine-tuning of systems, content, and jEdit folder
 *
 * if run from downloaded mmt.jar: creates the same folders, copies mmt.jar into systems/MMT/deploy, extracts files from resources/setup
 * if run in any other way, in particular after cloning and building MMT: creates the same folders, configures sbt
 *
 * In all cases, the MMT folder has the same structure
 *
 * @param logger Optional function to print interactive setup statements in. Defaults to println.
 */
class Setup extends ShellExtension("setup") {
   def helpText = "usage: setup [SYSTEM/FOLDER [CONTENT/FOLDER [JEDIT/SETTINGS/FOLDER]]]"

   /** a function to log a message */
   val log: String => Unit = println

   def run(shell: Shell, args: List[String]): Boolean = {
      log("\n\n\n\nThis is MMT's setup routine.\n" +
              "MMT is provided self-contained, so there is not much to do.\n" +
              "But I'll create a few auxiliary files for you and (if you have 'git' and internet) checkout some basic archives.\n" +
              "I can also setup the integration with jEdit for you.\n\n\n"
      )
      val l = args.length
      if (l > 3) {
        log(helpText)
        return true
      }

      val (sysFolder, conFolder, jeditSettingsFolder) = if (l == 0) {
        // interactive setup
        // choose system folder, content folder, and possibly fatjar to copy
        // the latter applies if the fat jar was run from outside MMT's directory structure, i.e., by running a binary-only download
        val (sf, cf) = shell.runStyle match {
          case d: DeployRunStyle =>
            val mmt = d.deploy.up
            val con = shell.getFile("Enter a folder into which archives should be checked out", Some(mmt.up / "content"))
            (mmt, con)
          case _ =>
            val root = shell.getFile("Enter a folder into which MMT should be installed: ", Some(shell.controller.getHome))
            (root / "systems" / "MMT", root / "content")
        }
        // get jEdit folder (if any)
        val jsf = OS.jEditSettingsFolder orElse {
          log("\n\nMMT can provide an IDE by acting as a jEdit plugin, but no jEdit settings folder was detected.\n" +
                   "You can cancel setup, install jEdit, and rerun setup,\n" +
                   "     or manually enter the path to your jEdit settings folder now,\n" +
                   "     or press return to skip jEdit integration for now.")
           val j = shell.getFile("", None)
           if (j.segments.isEmpty) None
           else Some(File(j))
        }
        (sf, cf, jsf)
      } else {
        // setup via command line arguments
        val sf = File(args(0))
        val cf = if (l >= 2) File(args(1)) else {
          sf.up/"content"
        }
        val jsf = if (l >= 3) Some(File(args(2))) else {
          OS.jEditSettingsFolder
        }
        (sf, cf, jsf)
      }
      setup(sysFolder, conFolder, jeditSettingsFolder.map(f => (shell,f)))
      true
   }

   /**
    * carries out all setup activities without further interaction
    * @param systemFolder create folder MMT/deploy/mmt.jar and auxiliary files in this folder
    * @param contentFolder add this folder as the default folder containing archives, clone MathHub/MMT/examples into here
    * @param setupJEdit if given, also install MMT as a plugin for jEdit
    * @param installContent if set to false, to do install the content archives
    */
   def setup(systemFolder: File, contentFolder: File, setupJEdit: Option[(Shell, File)], installContent: Boolean = true) {
     log("\n\nI'm going to try to set things up now.\n" +
              "If the following code fails and no help is around, you can try looking at the source code in info.kwarc.mmt.doc.Setup\n")

     log("MMT will be installed using the following data\n" +
         "MMT system folder:     " + systemFolder + "\n" +
         "MMT content folder:    " + contentFolder
      )
      setupJEdit match {
        case Some((_,f)) =>
          log("jEdit settings folder: " + f)
        case None =>
          log("jEdit settings folder: not provided (The jEdit plugin can be installed separately using 'mmt :jeditsetup'.)")
      }
     log("\n")

      val deploy = systemFolder / "deploy"

      // copy the jar itself and all resources in the mmt.jar/setup folder to the systemFolder
      // resources to be placed in the setup folder are listed in the sbt file
      MMTSystem.runStyle match {
        case DownloadedFatJar(jar) =>
          log("copying mmt.jar to " + deploy)
          File.copy(jar, deploy / "mmt.jar", true)
          log("done")

          val jarF = new java.util.jar.JarFile(jar)
          val entries = jarF.entries
          val unix = OS.detect.isInstanceOf[Unix]
          while (entries.hasMoreElements) {
             val entry = entries.nextElement
             val name = entry.getName
             if (!entry.isDirectory && name.startsWith("setup/")) {
                val rest = name.substring(6)
                // skip OS-specific files
                val winMain = rest.endsWith(".bat")
                val unixMain = rest == "deploy/mmt"
                if (! (unix && winMain || !unix && unixMain)) {
                   val dest = systemFolder / rest
                   log("extracting " + dest)
                   val s = utils.readFullStream(jarF.getInputStream(entry))
                   File.write(dest, s)
                   if (unix && unixMain) {
                     ShellCommand.run("chmod", "+x", dest.toString)
                   }
                  log("done")
                }
             }
          }
        case _ =>
      }

      val mathhubFolder = contentFolder/"MathHub"
      mathhubFolder.mkdirs
      val configFile = deploy/"mmtrc"
      if (!configFile.exists) {
        log("generating " + configFile)
        File.stream(configFile) {write =>
            write("// generated by setup, may be extended\n")
            write("#backends\n")
            write("// The following line allows using MMT commands for cloning MathHub archives\n")
            write("// By default, cloning uses https. Change this to 'oaf FOLDER ssh' to clone via ssh" + "\n")
            write("oaf " + mathhubFolder + "\n")
            write("// The following line loads all archives every time. This is good for beginners but advanced users may want to choose a specific set of archives each time.\n")
            write("mathpath " + contentFolder + "\n")
        }
        log("done\n")
      }
      controller.loadConfigFile(configFile, false)

      contentFolder.mkdirs

      if(installContent){
        log("cloning or downloading content repositories (I'll try to use git; if that fails, I download a zip file)")
        try {
         controller.handleLine("oaf clone MMT/examples")
        } catch {case e: Error =>
          log(e.toStringLong)
        }
        log("done\n")
      }

      setupJEdit foreach {case (shell,jsf) =>
         controller.extman.getOrAddExtension(classOf[ShellExtension], "jeditsetup") match {
            case None =>
              log("jedit-mmt is not on the classpath, so I can't set up jEdit.")
            case Some(jEditSetup) =>
              log("installing jEdit plugin")
               jEditSetup.run(shell, List("install",jsf.toString))
              log("done\n")

               if (!(jsf / "jars" / "SideKick.jar").exists) {
                 log("\n\nIt looks like you do not use jEdit a lot - maybe you've installed it just now?\n")
                 log("I can change some of jEdit's settings and install additional jEdit plugins that MMT uses.\n")
                 log("Do you want me to do that (y/n)?")
                 if (shell.getYesNo(true)) {
                   jEditSetup.run(shell, List("customize", jsf.toString))
                   log("done\n")
                 }
               }
         }
      }

      MMTSystem.runStyle match {
        case _: DeployRunStyle =>
          log("configuring sbt with MMT-specific settings")
          val settingsFile = "mmt-sbt-settings"
          val srcFolder = systemFolder / "src"
          File.copy(srcFolder / (settingsFile + ".example"),  srcFolder / settingsFile, false)
          log("done\n")
        case _ =>
      }

     log("\n\n\nThat's it. If there are no error messages above, you're ready to go.")
     log("\nThe main jar to execute is " + deploy/"mmt.jar" + ".")
     log("\n\nTo force rerunning setup or to update MMT, just run `java -jar mmt.jar :setup`.")
     }
}
