package info.kwarc.mmt.api.utils

object ShellCommand {
   /**
    * @param command the commnd to run
    * @return error message if not successful
    */
   def run(command: List[String]): Option[String] = {
      val proc = new java.lang.ProcessBuilder(command: _*).start() // use .inheritIO() for debugging
      proc.waitFor
      val ev = proc.exitValue
      if (ev != 0) {
         val scanner = new java.util.Scanner(proc.getErrorStream).useDelimiter("\\A")
         val message = if (scanner.hasNext) scanner.next else ""
         Some(message)
      } else
         None
   }
}