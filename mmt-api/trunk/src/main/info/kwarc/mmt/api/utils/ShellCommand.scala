package info.kwarc.mmt.api.utils

object ShellCommand {
   private def runInOpt(dir: Option[File], command: String*) = {
      val pb = new java.lang.ProcessBuilder(command: _*)// use .inheritIO() for debugging
      dir.foreach {d => pb.directory(d.toJava)}
      val proc = pb.start()
      proc.waitFor
      val ev = proc.exitValue
      if (ev != 0) {
         val scanner = new java.util.Scanner(proc.getErrorStream).useDelimiter("\\A")
         val message = if (scanner.hasNext) scanner.next else ""
         Some(message)
      } else
         None
   }
   
   /**
    * @param command the commnd to run
    * @return error message if not successful
    */
   def run(command: String*): Option[String] = runInOpt(None, command:_*)
   /**
    * like run
    * @param dir the directory in which to run the command
    */
   def runIn(dir: File, command: String*): Option[String] = runInOpt(Some(dir), command:_*)
}