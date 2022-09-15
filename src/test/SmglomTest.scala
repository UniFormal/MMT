import info.kwarc.mmt.api.archives.{BuildQueue, Dim}
import info.kwarc.mmt.api.utils.File

object SmglomTest extends MagicTest("stex-xhtml") {
  override def run(): Unit = {
    // uncomment these to build smglom and fix the paths (if needed)
    // buildSmglom()
    // fixSmglomPaths()
  }
  private def buildSmglom(): Unit = {
    controller.extman.get(classOf[BuildQueue]).foreach(bq => controller.extman.removeExtension(bq))
    controller.backend.getArchives.filter(_.id.startsWith("smglom")).foreach {a =>
      hl("build " + a.id + " stex-xhtml")
    }
  }
  private def fixSmglomPaths(): Unit = {
    val root = controller.getMathHub.get.local / "smglom"
    val hrefLocal = "href=\"" + root

    controller.backend.getArchives.foreach {
      case a if a.id.startsWith("smglom") =>
        val htmlfolder = a / Dim("xhtml")
        if (htmlfolder.isDirectory) htmlfolder.descendants.foreach {file =>
          println(file)
          var rets = ""
          var orig = File.read(file)
          while (orig.nonEmpty) {
            if (orig.startsWith(hrefLocal)) {
              rets += "href=\"stex-module://smglom/"
              orig = orig.drop(hrefLocal.length)
              val aname = orig.takeWhile(_ != '/')
              rets += aname + "/"
              orig = orig.drop(aname.length + 8)
              val fname = orig.takeWhile(_ != '#')
              assert(fname.endsWith(".pdf"))
              if (fname.contains('.')) rets += fname.takeWhile(_ != '.') else
                rets += fname.dropRight(4)
              orig = orig.dropWhile(_ != '\"')
              print("")
            } else {
              rets += orig.head
              orig = orig.tail
            }
          }
          File.write(file,rets)
        }
      case _ =>
    }
  }
}
