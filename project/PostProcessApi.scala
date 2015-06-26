import java.io.{File, _}
import java.nio.file.Files
import java.nio.file.StandardCopyOption._

import sbt.Keys.packageBin
import sbt._

object PostProcessApi {
  def deployPackage(name: String): Def.Initialize[Task[Unit]] =
    packageBin in Compile map { p =>
      deployTo(name)(p)
      p
    }

  def deployTo(name: String)(jar: File): Unit = {
    val tar = new File("../deploy/" + name)
    Files.copy(jar.toPath, tar.toPath, REPLACE_EXISTING)
    println("copied file: " + jar)
    println("to file: " + tar)
  }

  def delRecursive(log: Logger, path: File) {
    def delRecursive(path: File) {
      path.listFiles foreach { f =>
        if (f.isDirectory) delRecursive(f)
        else {
          f.delete()
          log.debug("deleted file: " + path)
        }
      }
      path.delete()
      log.debug("deleted directory: " + path)
    }
    if (path.exists && path.isDirectory) delRecursive(path)
    else log.warn("ignoring missing directory: " + path)
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
              w.close()
            } else
              log.debug("skipping: " + f)
          }
        }
      }
    }
    val apiDir = new File(mmtFolder + "/doc/api")
    if (apiDir.exists && apiDir.isDirectory) doFolder(apiDir, "../..")
    else log.error("missing api directory: " + apiDir)
  }
}
