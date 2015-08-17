package info.kwarc.mmt.api.backend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils._

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
class OAF(val uri: URI, val root: File, val report: Report) extends Logger {
   val logPrefix = "oaf"
   /** choose UnixGit or WindowsGit depending on OS */
   private val gitCommand = OS.detect match {case Windows => new WindowsGit() case _ => UnixGit}
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
   /** @return the ssh of the remote git manager - git@authority: */
   def ssh = "git@" + uri.authority.getOrElse("") + ":"
   /** initializes a repository */
   def init(pathS: String) {
      val path = utils.stringToList(pathS, "/")
      val repos = root / path
      val readme = "README.txt"
      val mf = "MANIFEST.MF"
      repos.mkdirs
      git(repos, "init")
      File.write(repos / "source" / readme, "commit your sources in this folder")
      git(repos, "add", s"source/$readme")
      File.WriteLineWise(repos / mf, List(s"id: $pathS", s"narration-base: http://mathhub.info/$pathS"))
      git(repos, "add", mf)
      git(repos, "commit", "-m", "\"automatically created by MMT\"")
      git(repos, "remote", "add", "origin", ssh + pathS + ".git")
      git(repos, "push", "origin", "master")
   }
   /** clones a repository */
   def clone(path: String): Option[File] = {
      val localPath = root / path
      if (localPath.exists) {
         log("target directory exists, skipping")
      } else {
         val success = git(root, "clone", (uri/path).toString + ".git", path)
         if (!success) return None
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
