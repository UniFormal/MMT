package info.kwarc.mmt.api.backend

import info.kwarc.mmt.api._
import frontend._
import utils._
import utils.FileConversion._

/**
 * wraps around a git client to repositories holdings MMT archives  
 */
class OAF(root: File, val report: Report) extends Logger {
   val logPrefix = "oaf"
   private val gitPath = """C:\Program Files\Git\bin\git""" 
   private def git(args: String*): Boolean = {
      val command = gitPath :: args.toList
      log(command.mkString(" "))
      val result = ShellCommand.run(command :_*)
      result match {
         case Some(m) =>
            report("error", m)
            false
         case None =>
            true
      }
   }
   /** clones a repository */
   def clone(uri: URI): Option[File] = {
      val localPath = root / uri.path
      if (localPath.exists) {
         log("target directory exists, skipping")
      } else {
         val success = git("clone", uri.toString + ".git", localPath.toString)
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
         File.write(mi / "MANIFEST.MF", s"""id: ${uri.path}\n""")
         git("init", localPath.toString)
         git("add", ".", localPath.toString)
      }
      Some(localPath)
   }
   /*
   private def getSubdirs(f: File) = f.list.toList.map(f/_).filter(_.isDirectory)
   def getAll: List[File] = {
      getSubdirs(root).flatMap {getSubdirs(_)}
   }
   def updateAll {
      getAll foreach {f =>
         git("pull", f.toString)
      }
   }
   def commitAll {
      getAll foreach {f =>
         git("commit", """-m""""", f.toString)
         git("push", f.toString)
      }
   }
   */
}
