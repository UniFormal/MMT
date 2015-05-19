import java.io._

import sbt.Logger

object PostProcessApi {
  def postProcess(log: Logger) = {
    val mmtFolder = new File(System.getProperty("user.dir")).getParent
    val oldPrefix = "file:/" + mmtFolder
    def doFolder(path: File, newPrefix: String) {
      log.info("processing: " + path)
      path.list foreach { e =>
        if (!e.startsWith(".")) {
          val f = new File(path + "/" + e)
          if (f.isDirectory)
            doFolder(f, "../" + newPrefix)
          else if (e.endsWith(".html") && !e.startsWith("index")) {
            val s = io.Source.fromFile(f).getLines().mkString("", "\n", "\n")
            if (s.indexOf(oldPrefix) >= 0) {
              val w = new BufferedWriter(new FileWriter(f))
              log.info("rewrote: " + f)
              w.write(s.replace(oldPrefix, newPrefix))
            } else
              log.info("skipping: " + f)
          }
        }
      }
    }
    doFolder(new File(mmtFolder + "/doc/api"), "../..")
  }
}
