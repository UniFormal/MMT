package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import utils._
import utils.FileConversion._

sealed abstract class Modification
case object Added extends Modification
case object Deleted extends Modification
case object Modified extends Modification
case object Unmodified extends Modification

object Modification {
   /**
    * @param input the file to be compiled/built
    * @param result the output file of the previous compile
    * @return status of input file, obtained by comparing to result file
    */
   def apply(input: File, result: File) = { 
      if (! result.exists) Added
      else if (! input.exists) Deleted
      else {
        val lastRun = result.lastModified
        val lastChanged = input.lastModified
        if (lastRun < lastChanged) Modified
        else Unmodified
      }
   }
}


/**
 not needed anymore because error files are used

class Timestamps(srcFolder: File, stampFolder: File) {
   def set(path: List[String]) {
      val file = stampFolder / path
      File.write(file,System.currentTimeMillis.toString)
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

class TimestampManager(archive: WritableArchive, mainStampFolder: File) {
   private val timestamps = new scala.collection.mutable.ListMap[String,Timestamps]
   def apply(bt: TraversingBuildTarget) = timestamps.getOrElseUpdate(bt.key,
         new Timestamps(archive.root / bt.inDim.toString, mainStampFolder / bt.key)
   )
}
*/