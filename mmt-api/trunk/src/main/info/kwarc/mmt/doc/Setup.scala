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
   def run(args: List[String]): Boolean = {
      val l = args.length
      if (l > 3) {
        println(helpText)
        return true
      }
      // Some(fatjar) if fat jar is run from outside clone of MMT repos
      val binaryOnly = MMTSystem.runStyle match {
        case DownloadedFatJar(f) => Some(f)
        case _ => None
      }
      if (l == 0) {
        // interactive setup
        println("Enter a folder into which MMT should be installed: ")
        val input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
        val root = File(input.readLine)
        val jEditSettingsFolder = OS.jEditSettingsFolder orElse {
           println("MMT can provide an IDE by acting as a jEdit plugin, but no jEdit settings folder was detected.\n"+
                   "You can cancel setup, install jEdit, and rerun setup,\n" +
                   "     or manually enter the path to your jEdit settings folder now,\n" +
                   "     or press return to skip jEdit integration for now.")
           val j = input.readLine()
           if (j.isEmpty) None
           else Some(File(j))
        }
        setup(root / "systems" / "MMT", root / "content", jEditSettingsFolder, binaryOnly)
      } else {
        // setup via command line arguments
        val systemFolder: File = if (l >= 1) File(args(0)) else {
          println(helpText)
          return true
        }
        val contentFolder = if (l >= 2) File(args(1)) else {
          systemFolder.up/"content"
        }
        val jEditSettingsFolder = if (l >= 3) Some(File(args(2))) else {
          OS.jEditSettingsFolder
        }
        setup(contentFolder, systemFolder, jEditSettingsFolder, binaryOnly)
      }
      true
   }
   
   /**
    * @param systemFolder create folder MMT/deploy/mmt.jar and auxiliary files in this folder
    * @param contentFolder add this folder as the default folder containing archives, clone MathHub/MMT/examples into here
    * @param jEditSettingsFolder if given, also install MMT as a plugin for jEdit
    */
   def setup(systemFolder: File, contentFolder: File, jEditSettingsFolder: Option[File], binaryOnly: Option[File]) {
      println("If the following shows any error messages, you can try looking at the source code.")
      println("The code is in info.kwarc.mmt.doc.Setup\n")
  
      println("MMT will be installed using the following data")
      println("MMT system folder:     " + systemFolder)
      println("MMT content folder:    " + contentFolder)
      jEditSettingsFolder match {
        case Some(f) => 
          println("jEdit settings folder: " + f)
        case None =>
          println("jEdit settings folder: not provided (The jEdit plugin can be installed separately using 'mmt :jeditsetup'.)")
      }
      println("")
  
      val deploy = systemFolder / "deploy"

      // copy the jar itself and all resources in the mmt.jar/setup folder to the systemFolder
      // resources to be placed in the setup folder are listed in the sbt file
      binaryOnly match {
        case Some(jar) =>
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
        val config = "// generated by setup, may be extended\n" +
                     "#backends\n" +
                    s"oaf $mathhubFolder"
        File.write(configFile, config)
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
               jEditSetup.run(List("install",jsf.toString))
               println("done\n")
        
               if (binaryOnly.isEmpty) {
                 println("configuring sbt target for rebuilding and reinstalling the jEdit plugin from sources")
                 File.write(systemFolder/"src"/"jedit-settings-folder", jsf.toString)
                 println("done\n")
               }
         }
      }
      println("")
      println("That's it. If there are no error messages above, you're ready to go.")
     }
}
