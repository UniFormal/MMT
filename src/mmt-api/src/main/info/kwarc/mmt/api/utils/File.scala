package info.kwarc.mmt.api.utils

import java.io._
import java.util.zip._

import scala.collection.mutable
import scala.language.implicitConversions

/** File wraps around java.io.File to extend it with convenience methods
  *
  * see implicit conversions with java.io.File at the end of this file
  */
case class File(toJava: java.io.File) {
  /** resolves an absolute or relative path string against this */
  def resolve(s: String): File = {
    val sf = new java.io.File(s)
    val newfile = if (sf.isAbsolute)
      sf
    else
      new java.io.File(toJava, s)
    File(newfile.getCanonicalPath)
  }

  /** makes a file relative to this one */
  def relativize(f: File): File = {
    val relURI = FileURI(this).relativize(FileURI(f))
    val FileURI(rel) = relURI
    rel
  }

  /** appends one path segment */
  def /(s: String): File = File(new java.io.File(toJava, s))

  /** appends a list of path segments */
  def /(ss: List[String]): File = ss.foldLeft(this) { case (sofar, next) => sofar / next }

  /** appends a relative path */
  def /(ss: FilePath): File = this / ss.segments

  /** parent directory */
  def up: File = {
    val par = Option(toJava.getParentFile)
    if (par.isEmpty) this else File(par.get)
  }
  
  def isRoot = up == this

  /** file name */
  def name: String = toJava.getName

  /** segments as a FilePath
    */
  def toFilePath = FilePath(segments)

  /** the list of file/directory/volume label names making up this file path
    * an absolute Unix paths begin with an empty segment
    */
  def segments: List[String] = {
    val name = toJava.getName
    val par = Option(toJava.getParentFile)
    if (par.isEmpty)
      if (name.isEmpty) if (toString.nonEmpty) List(toString.init) else Nil
      else List(name) // name == "" iff this File is a root
    else
      File(par.get).segments ::: List(name)
  }

  def isAbsolute: Boolean = toJava.isAbsolute

  override def toString = toJava.toString

  /** @return the last file extension (if any) */
  def getExtension: Option[String] = {
    val name = toJava.getName
    val posOfDot = name.lastIndexOf(".")
    if (posOfDot == -1) None else Some(name.substring(posOfDot + 1))
  }

  /** sets the file extension (replaces existing extension, if any) */
  def setExtension(ext: String): File = getExtension match {
    case None => File(toString + "." + ext)
    case Some(s) => File(toString.substring(0, toString.length - s.length) + ext)
  }

  /** appends a file extension (possibly resulting in multiple extensions) */
  def addExtension(ext: String): File = getExtension match {
    case None => setExtension(ext)
    case Some(e) => setExtension(e + "." + ext)
  }

  /** removes the last file extension (if any) */
  def stripExtension: File = getExtension match {
    case None => this
    case Some(s) => File(toString.substring(0, toString.length - s.length - 1))
  }

  /** @return children of this directory */
  def children: List[File] = toJava.list.toList.sorted.map(this / _)

  /** @return subdirectories of this directory */
  def subdirs: List[File] = children.filter(_.toJava.isDirectory)
  
  /** @return all files in this directory or any subdirectory */
  def descendants: List[File] = children.flatMap {c =>
    if (c.isDirectory) c.descendants else List(c)
  }

  /** delete this, recursively if directory */
  def deleteDir {
    toJava.list foreach { n =>
      val f = this / n
      if (f.toJava.isDirectory) f.deleteDir
      else f.toJava.delete
    }
    toJava.delete
  }
}

/** a relative file path usually within an archive below a dimension */
case class FilePath(segments: List[String]) {
  def toFile = File(toString)

  def name: String = if (segments.nonEmpty) segments.last else ""

  def dirPath = FilePath(if (segments.nonEmpty) segments.init else Nil)

  /** append a segment */
  def /(s: String): FilePath = FilePath(segments ::: List(s))

  override def toString: String = segments.mkString("/")
  
  def getExtension = toFile.getExtension
  def setExtension(e: String) = toFile.setExtension(e).toFilePath
}

object EmptyPath extends FilePath(Nil)

object FilePath {
   def apply(s:String): FilePath = FilePath(List(s))
   implicit def filePathToList(fp: FilePath) = fp.segments
   implicit def listToFilePath(l: List[String]) = FilePath(l)

  def getall(f : File) : List[File] = rec(List(f))

  private def rec(list : List[File]) : List[File] = list.flatMap(f => if (f.isDirectory) rec(f.children) else List(f))
}

/** constructs and pattern-matches absolute file:URIs in terms of absolute File's */
object FileURI {
  def apply(f: File): URI = {
    val ss = f.segments
    URI(Some("file"), None, if (ss.headOption.contains("")) ss.tail else ss, f.isAbsolute)
  }

  def unapply(u: URI): Option[File] =
    if ((u.scheme.isEmpty || u.scheme.contains("file")) && (u.authority.isEmpty || u.authority.contains("")))
      Some(File(new java.io.File(u.copy(scheme = Some("file"), authority = None))))
    // empty authority makes some Java versions throw error
    else None
}

/** MMT's default way to write to files; uses buffering, UTF-8, and \n */
class StandardPrintWriter(f: File) extends
OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(f.toJava)),
  java.nio.charset.Charset.forName("UTF-8")) {
  def println(s: String): Unit = {
    write(s + "\n")
  }
}

/** This defines some very useful methods to interact with text files at a high abstraction level. */
object File {
  /** constructs a File from a string, using the java.io.File parser */
  def apply(s: String): File = File(new java.io.File(s))

  def Writer(f: File): StandardPrintWriter = {
    f.up.toJava.mkdirs
    new StandardPrintWriter(f)
  }

  /**
   * convenience method for writing a string into a file
   *
   * overwrites existing files, creates directories if necessary
   * @param f the target file
   * @param strings the content to write
   */
  def write(f: File, strings: String*) {
    val fw = Writer(f)
    strings.foreach { s => fw.write(s) }
    fw.close
  }
  
  /**
   * streams a list-like object to a file
   * @param f the file to write to
   * @param begin initial text
   * @param sep text in between elements
   * @param end terminal text
   * @param work bind a variable "write" and call it to write into the file
   * example: (l: List[Node]) => stream(f, "<root>", "\n", "</root>"){out => l map {a => out(a.toString)}}
   */
  def stream(f: File, begin: String = "", sep: String = "", end: String="")(work: (String => Unit) => Unit) = {
     val fw = Writer(f)
     fw.write(begin)
     var writeSep = false
     def out(s: String) {
       if (writeSep)
         fw.write(sep)
       else
         writeSep = true
       fw.write(s)
     }
     try {
       work(out)
       fw.write(end)
     } finally {
       fw.close
     }
  }

  /**
   * convenience method for writing a list of lines into a file
   *
   * overwrites existing files, creates directories if necessary
   * @param f the target file
   * @param lines the lines (without line terminator - will be chosen by Java and appended)
   */
  def WriteLineWise(f: File, lines: List[String]) {
    val fw = Writer(f)
    lines.foreach {l =>
      fw.println(l)
    }
    fw.close
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

  /** unzips a file */
  def unzip(from: File, toDir: File) {
    val mar = new ZipFile(from)
    try {
      var bytes = new Array[Byte](100000)
      var len = -1
      val enum = mar.entries
      while (enum.hasMoreElements) {
        val entry = enum.nextElement
        val outFile = toDir / entry.getName
        outFile.up.mkdirs
        if (!entry.isDirectory) {
          val istream = mar.getInputStream(entry)
          val ostream = new java.io.FileOutputStream(outFile)
          try {
            while ({
              len = istream.read(bytes, 0, bytes.length)
              len != -1
            }) {
              ostream.write(bytes, 0, len)
            }
          } finally {
            ostream.close
            istream.close
          }
        }
      }
    } finally {
      mar.close
    }
  }
  /** dereference a URL and save as a file */
  def download(url: java.net.URL, file: File) {
    val conn = url.openConnection
    if (List("https","http") contains url.getProtocol) {
      val httpConn = conn.asInstanceOf[java.net.HttpURLConnection]
      // setFollowRedirects does not actually follow redirects
      if (httpConn.getResponseCode.toString.startsWith("30")) {
        val redirectURL = new java.net.URL(conn.getHeaderField("Location"))
        return download(redirectURL, file) 
      }
    }
    val input = conn.getInputStream
    val output = new java.io.FileOutputStream(file)
    try {
      val byteArray = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
      output.write(byteArray)
    } finally {
      input.close
      output.close
    }
  }

  /** implicit conversion Java <-> Scala */
  implicit def scala2Java(file: File): java.io.File = file.toJava

  /** implicit conversion Java <-> Scala */
  implicit def java2Scala(file: java.io.File): File = File(file)
}
