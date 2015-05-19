import java.io.{BufferedWriter, File, FileWriter}

import sbt._

object PostProcessApi {
  def postProcessApi = Command.command("postProcessApi") { state =>
    val mmtFolder = new File(System.getProperty("user.dir")).getParent
    val oldPrefix = "file:/" + mmtFolder
    def doFolder(path: File, newPrefix: String) {
      path.list foreach { e =>
        if (!e.startsWith(".")) {
          val f = new File(path + "/" + e)
          if (f.isDirectory)
            doFolder(f, "../" + newPrefix)
          else {
            val s = io.Source.fromFile(f).getLines().mkString("", "\n", "\n")
            if (s.indexOf(oldPrefix) >= 0) {
              val w = new BufferedWriter(new FileWriter(f))
              w.write(s.replace(oldPrefix, newPrefix))
              w.flush()
              w.close()
            }
          }
        }
      }
    }
    doFolder(new File(mmtFolder + "/doc/api/info"), "../../..")
    state
  }
}
