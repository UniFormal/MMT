import java.io._
import scala.collection._

case class File(toJava: java.io.File) {
   def /(s: String) = File(new java.io.File(toJava, s))
   override def toString = toJava.toString
}

/** copied from mmt-api */
object File {
   implicit def toJava(f: File) = f.toJava
   implicit def fromJava(f: java.io.File) = File(f)

  /** constructs a File from a string, using the java.io.File parser */
  def apply(s: String): File = File(new java.io.File(s))

  def append(f : File, strings: String*) {
    scala.tools.nsc.io.File(f.toString).appendAll(strings:_*)
  }

  /**
   * convenience method for reading a file into a string
   *
   * @param f the source file
   * @return s the file content (line terminators are \n)
   */
  def read(f: File): String = {
    val s = new StringBuilder
    ReadLineWise(f) {l => s.append(l + "\n")}
    s.result
  }

  /** convenience method to obtain a typical (buffered, UTF-8) reader for a file */
  def Reader(f: File): BufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(f.toJava),
    java.nio.charset.Charset.forName("UTF-8")))

  /** convenience method to read a file line by line
    * @param f the file
    * @param proc a function applied to every line (without line terminator)
    */
  def ReadLineWise(f: File)(proc: String => Unit) {
    val r = Reader(f)
    var line: Option[String] = None
    try {
      while ( {
        line = Option(r.readLine)
        line.isDefined
      })
        proc(line.get)
    } finally {
      r.close
    }
  }

  def readProperties(manifest: File): mutable.Map[String, String] = {
    val properties = new scala.collection.mutable.ListMap[String, String]
    File.ReadLineWise(manifest) { case line =>
      // usually continuation lines start with a space but we ignore those
      val tline = line.trim
      if (!tline.startsWith("//")) {
        val p = tline.indexOf(":")
        if (p > 0) {
          // make sure line contains colon and the key is non-empty
          val key = tline.substring(0, p).trim
          val value = tline.substring(p + 1).trim
          properties(key) = properties.get(key) match {
            case None => value
            case Some(old) => old + " " + value
          }
        }
      }
    }
    properties
  }
  
  /** copies a file */
  def copy(from: File, to: File, replace: Boolean): Boolean = {
    if (!from.exists || (to.exists && !replace)) {
      false
    } else {
      to.getParentFile.mkdirs
      val opt = if (replace) List(java.nio.file.StandardCopyOption.REPLACE_EXISTING) else Nil
      java.nio.file.Files.copy(from.toPath, to.toPath, opt:_*)
      true
    }
  }
}