package info.kwarc.mmt.api.utils

import java.awt.Desktop
import info.kwarc.mmt.api._

import java.io._
import java.util
import java.util.zip._
import scala.collection.mutable

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

  /** opens this file using the associated (native) application */
  def openInOS() {
    Desktop.getDesktop.open(toJava)
  }

  def canonical = File(toJava.getCanonicalFile)

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

  /** tests if this exists, possibly in compressed form */
  def existsCompressed = toJava.exists || Compress.name(this).exists
  
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

  /** removes the last file extension (if any) */
  def stripExtensionCompressed = Compress.normalize(this).stripExtension

  /** @return children of this directory */
  def children: List[File] = {
    if (toJava.isFile) Nil else {
      val ls = toJava.list
      if (ls == null) throw GeneralError("directory does not exist or is not accessible: " + toString)
      ls.toList.sorted.map(this / _)
    }
  }

  /** @return subdirectories of this directory */
  def subdirs: List[File] = children.filter(_.toJava.isDirectory)

  /** @return all files in this directory or any subdirectory */
  def descendants: List[File] = children.flatMap {c =>
    if (c.isDirectory) c.descendants else List(c)
  }

  /** @return true if that begins with this */
  def <=(that: File) = that.segments.startsWith(segments)

  /** if that<=this, return the remainder of this */
  def -(that: File) = if (that <= this) Some(FilePath(this.segments.drop(that.segments.length))) else None

  /** delete this, recursively if directory */
  def deleteDir {
   children foreach {c =>
      if (c.isDirectory) c.deleteDir
      else c.toJava.delete
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
  def /(s: String): FilePath = this / FilePath(List(s))
  /** append a path */
  def /(p: FilePath): FilePath = FilePath(segments ::: p.segments)

  override def toString: String = segments.mkString("/")

  def getExtension = toFile.getExtension
  def setExtension(e: String) = toFile.setExtension(e).toFilePath
  def stripExtension = toFile.stripExtension.toFilePath
}

object EmptyPath extends FilePath(Nil)

object FilePath {
   def apply(s:String): FilePath = FilePath(List(s))
   implicit def filePathToList(fp: FilePath) = fp.segments
   implicit def listToFilePath(l: List[String]) = FilePath(l)

  def getall(f : File) : List[File] = rec(List(f))

  private def rec(list : List[File]) : List[File] = list.flatMap(f => if (f.isDirectory) rec(f.children) else List(f))
}

/** Constructs and pattern-matches absolute file:URIs in terms of absolute File's.*/
object FileURI {
  def apply(f: File): URI = {
    val ss = f.segments
    URI(Some("file"), None, if (ss.headOption.contains("")) ss.tail else ss, f.isAbsolute)
  }

  def unapply(u: URI): Option[File] = {
    /* In contrast to RFC 8089 (https://tools.ietf.org/html/rfc8089), we allow for empty schemes for
       "File URI References" that, like URIs, allow omitting stuff from the left. */
    val valid_scheme    : Boolean = u.scheme.isEmpty || u.scheme.contains("file")

    // empty authority makes some Java versions throw errors
    val valid_authority : Boolean = u.authority.isEmpty || u.authority.contains("") || u.authority.contains("localhost")

    // We set authority to None because it's ignored later, anyway. No use to distinguish.
    if (valid_scheme && valid_authority) {
      val uR = u.copy(scheme = None, authority = None)
      Some(File(uR.toString)) // one would expect File(new java.io.File(uR)) here but that constructor cannot handle relative paths in a URI
    } else None
  }
}

/** wrappers for streams that allow toggling compressions */
object Compress {
  import org.tukaani.xz._
  private val cache = BasicArrayCache.getInstance
  def name(f: File) = f.addExtension("xz")
  def normalize(f: File) = if (f.getExtension contains "xz") f.stripExtension else f
  def out(s: OutputStream, c: Boolean) = if (c) new XZOutputStream(s, new LZMA2Options(), cache) else new BufferedOutputStream(s)
  def in(s: InputStream, c: Boolean) = if (c) new XZInputStream(s, cache) else s
}

/** MMT's default way to write to files; uses buffering, UTF-8, and \n */
class StandardPrintWriter(f: File, compress: Boolean) extends
      OutputStreamWriter(Compress.out(new FileOutputStream(f.toJava), compress), java.nio.charset.Charset.forName("UTF-8")) {
  def println(s: String) {
    write(s + "\n")
  }
}

/** This defines some very useful methods to interact with text files at a high abstraction level. */
object File {
  /** constructs a File from a string, using the java.io.File parser */
  def apply(s: String): File = File(new java.io.File(s))
  
  /** @param compress if true, the file is compressed while writing */
  def Writer(f: File, compress: Boolean = false): StandardPrintWriter = {
    f.up.toJava.mkdirs
    val fC = if (compress) Compress.name(f) else f
    new StandardPrintWriter(fC, compress)
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

  def append(f : File, strings: String*) {
    scala.tools.nsc.io.File(f.toString).appendAll(strings:_*)
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

  /** convenience method to obtain a typical (buffered, UTF-8) reader for a file
   *  
   *  If f does not exist, Compress.name(f) is tried and automatically decompressed.
   */
  def Reader(f: File): BufferedReader = {
    val fC = Compress.name(f)
    val compress = !f.exists && fC.exists
    val fileName = if (compress) fC else f
    val in = Compress.in(new FileInputStream(fileName.toJava), compress)
    StreamReading.Reader(in)
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

  def readPropertiesFromString(s: String): mutable.Map[String, String] = {
    val properties = new scala.collection.mutable.ListMap[String, String]
    s.split("\n") foreach {line =>
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
  def readProperties(manifest: File) = readPropertiesFromString(File.read(manifest))

  /** current directory */
  def currentDir = File(System.getProperty("user.dir"))

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
  def unzip(from: File, toDir: File, skipRootDir: Boolean = false) {
    val mar = new ZipFile(from)
    try {
      var bytes = new Array[Byte](100000)
      var len = -1
      val enum = mar.entries
      while (enum.hasMoreElements) {
        val entry = enum.nextElement
        var relPath = stringToList(entry.getName, "/")
        if (skipRootDir && relPath.length > 1)
          relPath = relPath.tail
        val outFile = toDir / relPath
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
  def download(uri: URI, file: File) {
    val input = URI.get(uri)
    file.up.mkdirs
    val output = new java.io.FileOutputStream(file)
    try {
      val step = 8192
      var byteArray = new Array[Byte](step)
      var pos, n = 0
      while ({
        if (pos + step > byteArray.length) byteArray = util.Arrays.copyOf(byteArray, byteArray.length << 1)
        n = input.read(byteArray, pos, step)
        n != -1
      }) pos += n
      if (pos != byteArray.length) byteArray = util.Arrays.copyOf(byteArray, pos)
      output.write(byteArray)
    } finally {
      input.close()
      output.close()
    }
  }

  /** implicit conversion Java <-> Scala */
  implicit def scala2Java(file: File): java.io.File = file.toJava

  /** implicit conversion Java <-> Scala */
  implicit def java2Scala(file: java.io.File): File = File(file)
}
