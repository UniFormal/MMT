package info.kwarc.mmt.doc

import info.kwarc.mmt.api.utils._

/**
 * AdjustScaladoc C:\MMT
 * replaces the generated source links in the scaladoc with relative paths
 */
object AdjustScaladoc {
  def main(args: Array[String]): Unit = {
    val mmtFolder = args(0)
    val apifolder = File(mmtFolder) / "doc" / "api" / "info"
    val oldPrefix = FileURI(File(args(0))).toString
    val newPrefix = "../../.."
    println(s"topfolder: $apifolder, old: $oldPrefix, new: $newPrefix")
    def doFolder(l: List[String]): Unit = {
      (apifolder / l).list foreach { e =>
        if (e != ".svn") {
          val le = l ::: List(e)
          val f = apifolder / le
          if (f.isDirectory) {
            println("folder: " + f)
            doFolder(le)
          } else {
            //println("file: " + f)
            val c = File.read(f)
            val up = l.map(_ => "../").mkString("")
            val cR = c.replace(oldPrefix, up + newPrefix)
            File.write(f, cR)
          }
        }
      }
    }
    doFolder(Nil)
  }
}
