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
 */
class Setup extends ShellExtension("setup") {
   def helpText = "usage: setup [SYSTEM/FOLDER [CONTENT/FOLDER [JEDIT/SETTINGS/FOLDER]]]" 
   
   def run(shell: Shell, args: List[String]): Boolean = {
      println("\n\n\n\nThis is MMT's setup routine.\n" + 
              "MMT is provided self-contained, so there is not much to do.\n" +
              "But I'll create a few auxiliary files for you and (if you have 'git' and internet) checkout some basic archives.\n" +
              "I can also setup the integration with jEdit for you.\n\n\n"
      )
      val l = args.length
      if (l > 3) {
        println(helpText)
        return true
      }
      
      if (l == 0) {
        // interactive setup

        // choose system folder, content folder, and possibly fatjar to copy
        // the latter applies if the fat jar was run from outside MMT's directory structure, i.e., by running a binary-only download
        val (sysFolder, conFolder) = shell.runStyle match {
          case d: DeployRunStyle =>
            val mmt = d.deploy.up
            val con = shell.getFile("Enter a folder into which archives should be checked out", Some(mmt.up / "content"))
            (mmt, con)
          case _ =>
            val root = shell.getFile("Enter a folder into which MMT should be installed: ", Some(shell.controller.getHome))
            (root / "systems" / "MMT", root / "content")
        }
      
        // get jEdit folder (if any)
        val jEditSettingsFolder = OS.jEditSettingsFolder orElse {
           println("\n\nMMT can provide an IDE by acting as a jEdit plugin, but no jEdit settings folder was detected.\n" +
                   "You can cancel setup, install jEdit, and rerun setup,\n" +
                   "     or manually enter the path to your jEdit settings folder now,\n" +
                   "     or press return to skip jEdit integration for now.")
           val j = shell.getFile("", None)
           if (j.segments.isEmpty) None
           else Some(File(j))
        }
        
        setup(shell, sysFolder, conFolder, jEditSettingsFolder)
        
      } else {
        // setup via command line arguments
        
        val systemFolder: File = File(args(0))
        val contentFolder = if (l >= 2) File(args(1)) else {
          systemFolder.up/"content"
        }
        val jEditSettingsFolder = if (l >= 3) Some(File(args(2))) else {
          OS.jEditSettingsFolder
        }
        setup(shell, contentFolder, systemFolder, jEditSettingsFolder)
      }
      true
   }
   
   /**
    * @param systemFolder create folder MMT/deploy/mmt.jar and auxiliary files in this folder
    * @param contentFolder add this folder as the default folder containing archives, clone MathHub/MMT/examples into here
    * @param jEditSettingsFolder if given, also install MMT as a plugin for jEdit
    */
   def setup(shell: Shell, systemFolder: File, contentFolder: File, jEditSettingsFolder: Option[File]) {
      println("\n\nI'm going to try to set things up now.\n" +
              "If the following code fails and no help is around, you can try looking at the source code in info.kwarc.mmt.doc.Setup\n")
  
      println("MMT will be installed using the following data\n" +
              "MMT system folder:     " + systemFolder + "\n" +
              "MMT content folder:    " + contentFolder
      )
      jEditSettingsFolder match {
        case Some(f) => 
          println("jEdit settings folder: " + f)
        case None =>
          println("jEdit settings folder: not provided (The jEdit plugin can be installed separately using 'mmt :jeditsetup'.)")
      }
      println("\n")
  
      val deploy = systemFolder / "deploy"

      // copy the jar itself and all resources in the mmt.jar/setup folder to the systemFolder
      // resources to be placed in the setup folder are listed in the sbt file
      shell.runStyle match {
        case DownloadedFatJar(jar) =>
          println("copying mmt.jar to " + deploy)
          File.copy(jar, deploy / "mmt.jar", true)
          println("done")
          
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
                   println("extracting " + dest)
                   val s = utils.readFullStream(jarF.getInputStream(entry))
                   File.write(dest, s)
                   if (unix && unixMain) {
                     ShellCommand.run("chmod", "+x", dest.toString)
                   }
                   println("done")
                }
             }
          }
        case _ =>
      }
      
      val mathhubFolder = contentFolder/"MathHub"
      mathhubFolder.mkdirs
      val configFile = deploy/"mmtrc"
      if (!configFile.exists) {
        println("generating " + configFile)
        File.stream(configFile) {write =>
            write("// generated by setup, may be extended\n")
            write("#backends\n")
            write("// The following line allows using MMT commands for cloning MathHub archives\n") 
            write("oaf " + mathhubFolder + "\n")
            write("// The following line loads all archives every time. This is good for beginners but advanced users may want to choose which archives to load when.\n")
            write("mathpath " + contentFolder + "\n")
        }
        println("done\n")
      }
      controller.loadConfigFile(configFile, false)
  
      println("cloning content repositories (by calling git; make sure it's installed and on your path)")
      contentFolder.mkdirs
      
      try {
         controller.cloneRecursively("MMT/examples")
      } catch {case e: Error =>
         println(e.toStringLong)
      }
      println("done\n")
  
      jEditSettingsFolder foreach {jsf =>
         controller.extman.getOrAddExtension(classOf[ShellExtension], "jeditsetup") match {
            case None =>
              println("jedit plugin not on classpath")
            case Some(jEditSetup) => 
               println("installing jEdit plugin")
               jEditSetup.run(shell, List("install",jsf.toString))
               println("done\n")
        
               shell.runStyle match {
                 case _: DeployRunStyle =>
                   println("configuring sbt target for rebuilding and reinstalling the jEdit plugin from sources")
                   File.write(systemFolder/"src"/"jedit-settings-folder", jsf.toString)
                   println("done\n")
                 case _ =>
               }
         }
      }
      println("\n\n\nThat's it. If there are no error messages above, you're ready to go.")
      println("\nThe main jar to execute is " + deploy/"mmt.jar")
      println("\n\nTo update MMT, just rerun setup.")
     }
}
