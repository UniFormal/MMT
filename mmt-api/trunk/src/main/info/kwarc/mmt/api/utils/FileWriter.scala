package info.kwarc.mmt.api.utils

trait FileWriter {
   val filename : java.io.File
   val file = FileWriter(filename) 
}

object FileWriter {
   def apply(filename : java.io.File) =
      new java.io.PrintWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream(filename), java.nio.charset.Charset.forName("UTF-8")))
}