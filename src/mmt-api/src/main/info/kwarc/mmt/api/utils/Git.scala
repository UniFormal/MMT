package info.kwarc.mmt.api.utils

/** builds git commands */
abstract class Git {
  /** the path to git, defaults to "git" */
  val gitPath = "git"
  /**
    * @param args the git commands (excluding "git")
    */
  def toArgs(args: String*): List[String]

  /**
    * runs git and returns a pair of (stdout, success)
    * @param wd Working directory to run command in
    * @param args Arguments to pass to git
    * @return
    */
  def apply(wd: File, args: String*): ShellCommand.Result = {
    val fullArgs = toArgs(args:_*)
    // we always print out git commands because it is annoying for users not to know what's going on
    ShellCommand.runInWithEnv(wd, Map("GIT_TERMINAL_PROMPT" -> "0", "GIT_ASKPASS" -> "/bin/false"), true, fullArgs:_*)
  }
}

/** prepends the path to git to the git commands */
object UnixGit extends Git {
  def toArgs(args: String*) = gitPath :: args.toList
}

/**
  * git behaves rather weirdly under windows, especially if authentication is needed
  *
  * this runs the [[UnixGit]] commands in the bash shell that comes with windows git
  *
  * @param sh the path to git's bash (may contain spaces), defaults to "sh"
  */
class WindowsGit(sh: String = "sh") extends Git {
  def toArgs(args: String*) = gitPath :: args.toList
}
