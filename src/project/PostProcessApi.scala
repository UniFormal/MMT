import java.io.{BufferedWriter,FileWriter}
import java.nio.file.Files
import java.nio.file.StandardCopyOption._

import sbt._

object PostProcessApi {

  def postProcess(log: Logger) = {
    val mmtFolder = File(System.getProperty("user.dir")).getParentFile
    val oldPrefix = "file:/" + mmtFolder.toString
    def doFolder(path: File): Unit = {
      log.debug("processing: " + path)
      path.list foreach { e =>
        if (!e.startsWith(".")) {
          val f = path / e
          if (f.isDirectory)
            doFolder(f)
          else if (e.endsWith(".html") && !e.startsWith("index")) {
            val s = io.Source.fromFile(f).getLines().mkString("", "\n", "\n")
            if (s.indexOf(oldPrefix) >= 0) {
              val w = new BufferedWriter(new FileWriter(f))
              log.debug("rewrote: " + f)
              w.write(s.replace(oldPrefix, "https://github.com/UniFormal/MMT/blob/master"))
              w.close()
            } else
              log.debug("skipping: " + f)
          }
        }
      }
    }
    val apiDir = mmtFolder / "apidoc"
    if (apiDir.exists && apiDir.isDirectory) doFolder(apiDir)
    else log.error("missing api directory: " + apiDir)
  }
}
