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
 */
class Setup extends ShellExtension("setup") {
   def helpText =
     """
       |usage: setup SYSTEM CONTENT BRANCH JEDIT
       |
       |  SYSTEM:       Folder that MMT System is in. Empty or --auto for current folder.
       |  CONTENT:      Content folder to place and load archives from. Empty or --auto for sibling of SYSTEM.
       |  BRANCH:       The branch of the content archives to install.
       |                Empty or --auto for main branch. --no-content to avoid installing archives.
       |  JEDIT:        jEdit settings folder to install MMT plugin for jEdit.
       |                Empty or --auto to detect automatically. --no-jedit to avoid.
       |     """.stripMargin

   /** a function to log a message */
   val log: String => Unit = println

   def run(shell: Shell, args: List[String]): Option[Level.Level] = {
      log("\n\n\n\nThis is MMT's setup routine.\n" +
              "It will create a few auxiliary files, clone and build example repositories and setup the integration with jEdit.\n"
      )
      val l = args.length
      if (l > 4) {
        log(helpText)
        return withError
      }

      val (sysFolder, conFolder, installContent, jeditSettingsFolder, customizeJEdit) = if (l == 0) {
        // interactive setup
        // choose system folder, content folder, and possibly fatjar to copy
        // the latter applies if the fat jar was run from outside MMT's directory structure, i.e., by running a binary-only download
        val (sf, cf) = shell.runStyle match {
          case d: DeployRunStyle =>
            val mmt = d.deploy.up
            val con = shell.getFile("Enter a folder into which archives should be checked out", Some(mmt.up / "MMT-content"))
            (mmt, con)
          case _ =>
            val root = shell.getFile("Enter a folder into which MMT should be installed: ", Some(shell.controller.getHome / "MMT"))
            (root / "systems" / "MMT", root / "MMT-content")
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
        (sf, cf, Some(""), jsf, None)
      } else {
        // setup via command line arguments
        val sf = if (args(0) == "--auto") {
          MMTSystem.runStyle match {
            case d: DeployRunStyle =>
              d.deploy.up
            case _ =>
              File.currentDir / "MMT-system"
          }
        } else File(args(0))
        val cf = if (l >= 2 && args(1) != "--auto") File(args(1)) else {
          sf.up / "MMT-content"
        }
        val ic = if (l >= 3 && args(2) != "--auto") {
          if (args(2) == "--no-content") None
          else Some(args(2))
        } else Some("")
        val jsf = if (l >= 4) {
          if (args(3) == "--no-jedit") {
            None
          } else {
            Some(File(args(3)))
          }
        } else {
          OS.jEditSettingsFolder
        }
        (sf, cf, ic, jsf, Some(true))
      }
      setup(sysFolder, conFolder, jeditSettingsFolder.map(f => (shell,f, customizeJEdit)), installContent)
      withSuccess
   }

   /**
    * carries out all setup activities without further interaction
    * @param systemFolder create folder MMT/deploy/mmt.jar and auxiliary files in this folder
    * @param contentFolder add this folder as the default folder containing archives, clone MathHub/MMT/examples into here
    * @param setupJEdit if given, also install MMT as a plugin for jEdit
    * @param installContent if given, the branch of the content archives to install (Some("") for main branch)
    */
   def setup(systemFolder: File, contentFolder: File, setupJEdit: Option[(Shell, File, Option[Boolean])], installContent: Option[String] = Some("")): Unit = {
     //log("\n\nI'm going to try to set things up now.\n" +
     //         "If the following code fails and no help is around, you can try looking at the source code in info.kwarc.mmt.doc.Setup\n")

     log("MMT will be installed using the following data\n" +
         "MMT system folder:     " + systemFolder + "\n" +
         "MMT content folder:    " + contentFolder
      )
      setupJEdit match {
        case Some((_,f,_)) =>
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
              val winFile = rest.endsWith(".bat")
              if (unix && !winFile || !unix && winFile) {
                val dest = systemFolder / rest
                log("extracting " + dest)
                val s = utils.readFullStream(jarF.getInputStream(entry))
                File.write(dest,s)
                if (unix) {
                  ShellCommand.run("chmod","+x",dest.toString)
                }
                log("done")
              }
            }
          }
        case _ =>
           // TODO this case should also copy fiels
           //  if MMT is run with a parameter for the system folder that's not an MMT clone folder (but nobody should do that anyway)
      }

      val configFile = deploy/"mmtrc"
      if (!configFile.exists) {
        log("generating " + configFile)
        File.stream(configFile) {write =>
            write("// generated by setup, may be extended\n")
            write("#backends\n")
            write("// The following line allows using MMT commands for cloning MathHub archives\n")
            write("// By default, cloning uses https. Change this to 'lmh FOLDER ssh' to clone via ssh" + "\n")
            write("lmh " + contentFolder + "\n")
            write("// The following line loads all archives every time. This is good for beginners but advanced users may want to choose a specific set of archives each time.\n")
            write("mathpath " + contentFolder + "\n")
        }
        log("done\n")
      }
      controller.loadConfigFile(configFile, false)

      val contentFolderMMT = contentFolder / "MMT"
      contentFolderMMT.mkdirs
      installContent.foreach {b =>
        val baseArchives = List("urtheories","examples","LATIN2")
        log("cloning content repositories via git")
        val rf = controller.extman.get(classOf[RunFile]).head
        val branch = if (b.isEmpty) "master" else b
        var build = true
        try {
          baseArchives.foreach {a =>
            log("cloning " + a)
            OS.git(contentFolderMMT, "clone", "-b", branch, "https://gl.mathhub.info/MMT/" + a) match {
              case ShellCommand.Abort(e) =>
                log("failed with message: " + e.getMessage)
                throw e
              case ShellCommand.Fail(op,_) =>
                log(op)
                log("failed, skipping the building from now on")
                build = false
              case ShellCommand.Success(op) =>
                log(op)
                if (build) {
                  log("building the repository (this may take a minute)")
                  val ec = rf.doIt(contentFolderMMT / a / "build.msl")
                  if (ec.get > Level.Info) {
                    log("errors while building " + a + " (see output above)")
                  }
                }
            }
          }
        } catch {case e: Exception =>
          log(Error(e).toStringLong)
        }
        log("done\n")
      }

      setupJEdit foreach {case (shell,jsf,customize) =>
         controller.extman.getOrAddExtension(classOf[ShellExtension], "jeditsetup") match {
            case None =>
              log("jedit-mmt is not on the classpath, so I can't set up jEdit.")
            case Some(jEditSetup) =>
              log("installing jEdit plugin")
              jEditSetup.run(shell, List("install",jsf.toString))
              log("done\n")

               if (!(jsf / "jars" / "SideKick.jar").exists) {
                 val doCustomize = customize.getOrElse {
                   log("\n\nIt looks like you do not use jEdit a lot - maybe you've installed it just now?\n")
                   log("I can change some of jEdit's settings and install additional jEdit plugins that MMT uses.\n")
                   log("Do you want me to do that (y/n)?")
                   shell.getYesNo(true)
                 }
                 if (doCustomize) {
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

     log("\nThat's it. If there are no error messages above, you're ready to go.")
     log("\nIf git caused errors, you can just clone the archives manually and build them using `run-file :file PATH/TO/build.msl` in the above order.")
     log(s"\nFor standalone use of MMT, the main jar to execute is `${deploy}/mmt.jar`. jEdit will find MMT automatically.")
     log("\nTo force rerunning setup or to update MMT, just run `java -jar mmt.jar :setup` (or delete the generated `mmtrc` file and rerun MMT).")
     }
}
