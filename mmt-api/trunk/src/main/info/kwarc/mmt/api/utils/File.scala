package info.kwarc.mmt.api.utils

import java.io._

import scala.language.implicitConversions


/**
 * a file path
 */
case class FilePath(segments: List[String]) {
  def toFile: File = File(toString)

  def baseName: String = if (segments.nonEmpty) segments.last else ""

  def dirPath: FilePath = FilePath(if (segments.nonEmpty) segments.init else Nil)

  override def toString: String = segments.mkString("/")
}

object EmptyPath extends FilePath(Nil)

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
  def /(ss: FilePath): File = ss.segments.foldLeft(this) { case (sofar, next) => sofar / next }

  /** parent directory */
  def up: File = File(toJava.getParentFile)

  /** file name */
  def name: String = toJava.getName

  /** the list of file/directory/volume label names making up this file path
    * absolute Unix paths begin with an empty segment
    */
  def filepath: FilePath = FilePath(segments)

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

  override def toString: String = toJava.toString

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

  /** delete this, recursively if directory */
  def deleteDir(): Unit = {
    toJava.list foreach { n =>
      val f = this / n
      if (f.toJava.isDirectory) f.deleteDir()
      else f.toJava.delete()
    }
    toJava.delete()
  }
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
  def write(f: File, strings: String*): Unit = {
    val fw = Writer(f)
    strings.foreach { s => fw.write(s) }
    fw.close()
  }

  /**
   * convenience method for writing a list of lines into a file
   *
   * overwrites existing files, creates directories if necessary
   * @param f the target file
   * @param lines the lines (without line terminator - will be chosen by Java and appended)
   */
  def WriteLineWise(f: File, lines: List[String]): Unit = {
    val fw = Writer(f)
    lines.foreach { l =>
      fw.println(l)
    }
    fw.close()
  }

  /**
   * convenience method for reading a file into a string
   *
   * @param f the source file
   * @return s the file content (line terminators are \n)
   */
  def read(f: File): String = {
    val s = new StringBuilder
    ReadLineWise(f) { l => s.append(l + "\n") }
    s.result()
  }

  /** convenience method to obtain a typical (buffered, UTF-8) reader for a file */
  def Reader(f: File): BufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(f.toJava),
    java.nio.charset.Charset.forName("UTF-8")))

  /** convenience method to read a file line by line
    * @param f the file
    * @param proc a function applied to every line (without line terminator)
    */
  def ReadLineWise(f: File)(proc: String => Unit): Unit = {
    val r = Reader(f)
    var line: Option[String] = None
    try {
      while ( {
        line = Option(r.readLine)
        line.isDefined
      })
        proc(line.get)
    } finally {
      r.close()
    }
  }

  /** implicit conversion Java <-> Scala */
  implicit def scala2Java(file: File): java.io.File = file.toJava

  /** implicit conversion Java <-> Scala */
  implicit def java2Scala(file: java.io.File): File = File(file)
}
