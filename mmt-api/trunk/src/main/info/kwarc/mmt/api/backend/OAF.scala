package info.kwarc.mmt.api.backend

import info.kwarc.mmt.api._
import frontend._
import utils._
import utils.FileConversion._

/** builds git commands */
abstract class Git {
   /** the path to git, defaults to "git" */
   val gitPath = "git"
   /**
    * @param args the git commands (excluding "git")
    */
   def apply(args: String*): List[String]
}

/** prepends the path to git to the git commands */
object UnixGit extends Git {
   def apply(args: String*) = gitPath :: args.toList
}

/**
 * git behaves rather weirdly under windows, especially if authentication is needed
 * 
 * this runs the [[UnixGit]] commands in the bash shell that comes with windows git
 * 
 * @param sh the path to git's bash (may contain spaces), defaults to "sh" 
 */
class WindowsGit(sh: String = "sh") extends Git {
   def apply(args: String*) = List(sh, "--login", "-c", UnixGit(args:_*).mkString("\"", " ", "\""))
}

/**
 * wraps around a git shell client to repositories holdings MMT archives
 * 
 * git is called as follows
 *  - on Windows: sh --login -c "git"  
 *  - on other OS's: git
 * Users must make sure their shell interprets these commands properly.
 * 
 * @param root the directory in which clones are created
 * @param report for logging
 */
class OAF(uri: URI, root: File, val report: Report) extends Logger {
   val logPrefix = "oaf"
   /** choose UnixGit or WindowsGit depending on OS */
   private val gitCommand = if (System.getProperty("os.name").startsWith("Windows")) new WindowsGit() else UnixGit
   private def git(dir: File, args: String*): Boolean = {
      val command = gitCommand(args:_*)
      log(command.mkString(" ") + " in " + dir.toString)
      val result = ShellCommand.runIn(dir, command :_*)
      result match {
         case Some(m) =>
            report("error", m)
            false
         case None =>
            true
      }
   }
   /** clones a repository */
   def clone(path: String): Option[File] = {
      val localPath = root / path
      if (localPath.exists) {
         log("target directory exists, skipping")
      } else {
         val success = git(root, "clone", (uri/path).toString + ".git", localPath.toString)
         if (!success) return None
      }
      Some(localPath)
   }
   /** initializes a repository */
   def init(uri: URI): Option[File] = {
      //untested
      val localPath = root / uri.path
      if (localPath.exists) {
         logError("target directory exists")
      } else {
         val mi = localPath / "META-INF"
         mi.mkdir
         (localPath / "source").mkdir
         File.write(mi / "MANIFEST.MF", s"id: ${uri.path}\n")
         git(root, "init", localPath.toString)
         git(root, "add", ".", localPath.toString)
      }
      Some(localPath)
   }
   /** @return all repositories, i.e., grandchild directories of root */
   def repositories = root.subdirs.flatMap(_.subdirs)
   /** pulls all repositories under root */
   def pull {
      repositories.foreach {repos =>
         git(repos, "pull", "origin", "master")
      }
   }
   /** pushes all repositories under root (does not commit) */
   def push {
      repositories.foreach {repos =>
         git(repos, "push")
      }
   }
}

object OAF {
   val defaultURL = URI("http", "gl.mathhub.info") 
}
