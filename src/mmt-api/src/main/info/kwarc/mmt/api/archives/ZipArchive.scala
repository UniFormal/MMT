package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import modules._
import utils._

import java.io.{FileInputStream, FileOutputStream}
import java.util.zip._
import java.util.jar._

trait ZipArchive {self: Archive =>
    /**
     * add a file to a MAR file
     */
    private def addFileToMar(f: File, base: File, out: ZipOutputStream, buffer: Array[Byte]): Unit = {
        var bytesRead = 0
        val in = new FileInputStream(f)
        out.putNextEntry(new ZipEntry(f.segments.drop(base.segments.length).mkString("/")))
        var stop = false
        while (bytesRead != -1) {
            bytesRead = in.read(buffer)
            if (bytesRead != -1)
                out.write(buffer, 0, bytesRead)
        }
        in.close
    }


    /**
     * recursively add all files in a folder to a mar file
     */
    private def addFolderToMar(f: File, base: File, out: ZipOutputStream, buffer: Array[Byte]): Unit = {
        f.listFiles foreach {child =>
            if (child.isDirectory) {
                if (includeDir(child.getName)) addFolderToMar(child, base, out, buffer)
            } else
                addFileToMar(child, base, out, buffer)
        }
    }

    /**
     * pack everything in a mar zip archive.
     * @param target the target mar file. Default is <name>.mar in the root folder, where <name> is the name of the root
     */
    def toMar(target: File = root / (root.getName + ".mar")): Unit = {
        log("building math archive at " + target.getPath)
        val out = new ZipOutputStream(new FileOutputStream(target))
        val buffer = new Array[Byte](100000)   // 100KB buffer size
        try {
            List(Dim("META-INF"), source, narration, content, relational) foreach {dim =>
                if ((this/dim).canRead)
                    addFolderToMar(this/dim, root, out, buffer)
            }
        } catch {
            case e: java.io.IOException => logError("error while packing into a mar file: " + (if (e.getCause == null) "" else e.getCause))
        }
        log("done")
        out.close
        if ((root/"bin").canRead) {
           val targetJar = target.setExtension("jar")
           log("building Java archive at " + targetJar.getPath)
           val manifest = new Manifest()
           manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0")
           val outJar = new JarOutputStream(new FileOutputStream(targetJar), manifest)
           try {
              addFolderToMar(root/"bin", root/"bin", outJar, buffer)
           } catch {
              case e: java.io.IOException => logError("error while packing into a jar file: " + (if (e.getCause == null) "" else e.getCause))
           }
           log("done")
           outJar.close
        }
    }
}
