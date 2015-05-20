import java.io._
import java.nio.file.Files
import java.nio.file.StandardCopyOption._

import sbt.Logger

object PostProcessApi {
  def deployTo(name: String)(jar: File): Unit = {
    Files.copy(jar.toPath,
      new File("../deploy/jedit-plugin/plugin/jars/" + name).toPath,
      REPLACE_EXISTING)
  }

  def delRecursive(path: File) {
    path.listFiles foreach { f =>
      if (f.isDirectory) delRecursive(f)
      else f.delete()
    }
    path.delete()
  }

  def postProcess(log: Logger) = {
    val mmtFolder = new File(System.getProperty("user.dir")).getParent
    val oldPrefix = "file:/" + mmtFolder
    def doFolder(path: File, newPrefix: String) {
      log.debug("processing: " + path)
      path.list foreach { e =>
        if (!e.startsWith(".")) {
          val f = new File(path + "/" + e)
          if (f.isDirectory)
            doFolder(f, "../" + newPrefix)
          else if (e.endsWith(".html") && !e.startsWith("index")) {
            val s = io.Source.fromFile(f).getLines().mkString("", "\n", "\n")
            if (s.indexOf(oldPrefix) >= 0) {
              val w = new BufferedWriter(new FileWriter(f))
              log.debug("rewrote: " + f)
              w.write(s.replace(oldPrefix, newPrefix))
            } else
              log.debug("skipping: " + f)
          }
        }
      }
    }
    doFolder(new File(mmtFolder + "/doc/api"), "../..")
  }
}
