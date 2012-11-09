package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import modules._
import utils._
import utils.FileConversion._

import java.io.{FileInputStream, FileOutputStream}
import java.util.zip._

trait ZipArchive extends WritableArchive {
    /** Add a file to a MAR file (only used internally by toMar)
      * @throws java.io.IOException */
    private def addFileToMar(f: File, base: File, out: ZipOutputStream, buffer: Array[Byte]) {
        var bytesRead = 0
        val in = new FileInputStream(f)
        out.putNextEntry(new ZipEntry(f.getPath.substring(base.getPath.length + 1)))
        var stop = false
        while (bytesRead != -1) {
            bytesRead = in.read(buffer)
            if (bytesRead != -1)
                out.write(buffer, 0, bytesRead)
        }
        in.close
    }
      
    
    /** Add a folder to a MAR file (only used internally by toMar). Caution: empty folders are not put in the archive.
      * @throws java.io.IOException */
    private def addFolderToMar(f: File, base: File, out: ZipOutputStream, buffer: Array[Byte]) {
        // if the folder is empty, add a special entry for it
        //if (childList.isEmpty)
        //    out.putNextEntry(new ZipEntry(f.getPath.substring(base.getPath.length + 1)))
        f.listFiles foreach {child =>
            if (child.isDirectory) {
                if (includeDir(child.getName)) addFolderToMar(child, base, out, buffer)
            } else
                addFileToMar(child, base, out, buffer)
        }
    }
    
    /** Pack everything in a MAR archive. Caution: empty folders are not put in the archive.
      * @param target the target MAR file. Default is <name>.mar in the root folder, where <name> is the name of the root */
    def toMar(target: java.io.File = root / (root.getName + ".mar")) {
        log("building archive at " + target.getPath)
        val out = new ZipOutputStream(new FileOutputStream(target))
        val buffer = new Array[Byte] (100000)   // 100KB buffer size
        try {
            List("META-INF", "source", "narration", "content", "presentation", "relational") foreach {dim =>
                if ((root/dim).canRead)
                    addFolderToMar(root/dim, root, out, buffer)
            }
        } catch {
            case e: java.io.IOException => log("error when packing into a MAR file: " + (if (e.getCause == null) "" else e.getCause))
        }
        log("done")
        out.close
    }
}