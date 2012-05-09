package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import utils._
import utils.FileConversion._

abstract class Modification
case object Added extends Modification
case object Deleted extends Modification
case object Modified extends Modification
case object Unmodified extends Modification

class Timestamps(srcFolder: File, stampFolder: File) {
   def set(path: List[String]) {
      val file = stampFolder / path
      file.getParentFile.mkdirs()
      val out = File.Writer(file)
      out.write(System.currentTimeMillis.toString)
      out.close
   }
   def get(path: List[String]) : Long = {
      val file = stampFolder / path
      if (file.exists) {
         val in = File.Reader(file)
         val s = in.readLine().trim.toLong
         in.close
         s
      } else 0
   }
   def modified(path: List[String]) : Modification =
      if (! (stampFolder / path).exists) Added
      else if (! (srcFolder / path).exists) Deleted
      else if (get(path) < (srcFolder / path).lastModified()) Modified
      else Unmodified
}