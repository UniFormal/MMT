package jomdoc.utils

trait FileWriter {
   val filename : java.io.File
   val file = new java.io.PrintWriter(
                 new java.io.OutputStreamWriter(new java.io.FileOutputStream(filename), java.nio.charset.Charset.forName("UTF-8"))
   )
}