package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.utils._

import scala.collection.mutable

/** convenience class for traversing an Archive */
case class Current(file: File, path: FilePath)

/** grouped argument when traversing */
case class TraverseMode(includeFile: String => Boolean,
                        includeDir: String => Boolean, parallel: Boolean)

abstract class ROArchive extends Storage with Logger {
  val rootString: String

  override def toString = "archive " + rootString

  val properties: mutable.Map[String, String]
  val report: Report
  val logPrefix = "archive"

  val narrationBackend: Storage
}

abstract class WritableArchive extends ROArchive {
  val root: File
  val id = properties("id")
  val narrationBase = utils.URI(properties.getOrElse("narration-base", ""))
  /** the NamespaceMap built from the ns and ns-prefix properties */
  val namespaceMap = {
    var nsMap = NamespaceMap.empty
    val Matcher = new utils.StringMatcher2("", "-", "")
    properties.foreach {
      case ("ns", uri) => nsMap = nsMap.base(uri)
      case (Matcher("ns", prefix), uri) => nsMap = nsMap.add(prefix, uri)
      case _ =>
    }
    nsMap
  }

  /**
   * @param dim a dimension in the archive
   * @return the relative path to that
   */
  def /(dim: ArchiveDimension): File = dim match {
    case r@RedirectableDimension(key, _) => properties.get(key) match {
      case Some(p) => root / p
      case None => this / r.default
    }
    case Dim(path@_*) => root / FilePath(path.toList)
  }

  def includeDir(n: String): Boolean = n != ".svn" && n != ".mmt"

  val narrationBackend = LocalCopy(narrationBase.schemeNull, narrationBase.authorityNull, narrationBase.pathAsString, this / narration)

  def load(p: Path)(implicit controller: Controller): Unit = {
    p match {
      case doc: DPath => narrationBackend.load(doc)
      case mod: MPath =>
        val p = MMTPathToContentPath(mod)
        if (!p.exists) throw NotApplicable("file not found")
        // dpath is a dummy URI to be used when creating the Document that contains the module mod
        val dpath = DPath(narrationBase / Archive.MMTPathToContentPath(mod).segments)
        loadXML(mod.doc.uri, dpath, File.Reader(p))
      case OMMOD(m) % _ => load(m)
    }
  }

  protected val custom: ArchiveCustomization = {
    properties.get("customization") match {
      case None => new DefaultCustomization
      case Some(c) => java.lang.Class.forName(c).asInstanceOf[java.lang.Class[ArchiveCustomization]].newInstance
    }
  }

  protected def deleteFile(f: File): Unit = {
    log("deleting " + f)
    f.delete
  }

  /** Get the disk path of the module in the content folder
    * @param m the MPath of the module
    * @return the File descriptor of the destination  file in the content folder
    */
  def MMTPathToContentPath(m: MPath): File = this / content / Archive.MMTPathToContentPath(m)

  /** traverses a dimension calling continuations on files and subdirectories */
  def traverse[A](dim: ArchiveDimension, in: FilePath, mode: TraverseMode, sendLog: Boolean = true)
                 (onFile: Current => A, onDir: (Current, List[A]) => A = (_: Current, _: List[A]) => ()): Option[A] = {
    val TraverseMode(filter, filterDir, parallel) = mode
    def recurse(n: String): List[A] =
      traverse(dim, FilePath(in.segments ::: List(n)), mode, sendLog)(onFile, onDir).toList
    val inFile = this / dim / in
    val inFileName = inFile.getName
    if (inFile.isDirectory) {
      if (includeDir(inFileName) && filterDir(inFileName)) {
        if (sendLog) log("entering " + inFile)
        val children = inFile.list.sorted.toList
        val results = if (parallel) children.par flatMap recurse else children flatMap recurse
        val result = onDir(Current(inFile, in), results.toList)
        if (sendLog) log("leaving  " + inFile)
        Some(result)
      } else None
    } else if (filter(inFileName))
      try {
        val r = onFile(Current(inFile, in))
        Some(r)
      } catch {
        case e: Error => report(e); None
      }
    else None
  }
}

/** archive management
  * @param root the root folder that contains the source folder
  * @param properties a key value map
  * @param report the reporting mechanism
  *
  *               Archive is a very big class, so most of its functionality is outsourced to various traits that are mixed in here
  */
class Archive(val root: File, val properties: mutable.Map[String, String], val report: Report)
  extends WritableArchive with Validate with ScalaCode with ZipArchive {

  val rootString = root.toString
  val groupDir = root.up
  val baseDir = groupDir.up

  /**
   * computes the flattened theories by elaborating the patterns
   * @param in input path
   * @param controller the controller
   */
  def produceFlat(in: FilePath, controller: Controller): Unit = {
    val inFile = this / content / in
    log("to do: [CONT -> FLAT]        -> " + inFile)
    if (inFile.isDirectory) {
      inFile.list foreach { n =>
        if (includeDir(n)) produceFlat(FilePath(in.segments ::: List(n)), controller)
      }
    } else if (inFile.getExtension.contains("omdoc")) {
      val mpath = Archive.ContentPathToMMTPath(in)
      val mod = controller.globalLookup.getModule(mpath)
      val ie = new InstanceElaborator(controller)
      val flatNode = mod match {
        case thy: DeclaredTheory =>
          ie.elaborate(thy)
          thy.toNodeElab
        case _ => mod.toNode
      }
      val flatNodeOMDoc = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">
        {flatNode}
      </omdoc>
      xml.writeFile(flatNodeOMDoc, this / flat / in)
      controller.delete(mpath)
    }
    log("done:  [CONT -> FLAT]        -> " + inFile)
  }


  /**
   * collapses (de-structures) the theory graph by replacing arrows with automatically constructed objects (e.g. a view becomes a theory by applying it to the
   * domain).
   * @param in input path
   * @param controller the controller
   */
  def produceEnriched(in: FilePath, modElab: ModuleElaborator, controller: Controller): Unit = {
    val inFile = this / content / in
    //val modElab = new ModuleElaborator(controller)
    val enrichedDir = root / "enriched"
    log("to do: [CONT -> FLAT]       -> " + inFile)
    if (inFile.isDirectory) {
      inFile.list foreach { n =>
        if (includeDir(n)) produceEnriched(FilePath(in.segments ::: List(n)), modElab, controller)
      }
    } else if (inFile.getExtension.contains("omdoc")) {
      try {
        val mpath = Archive.ContentPathToMMTPath(in)
        val mod = controller.globalLookup.getModule(mpath)
        modElab.apply(mod) {
          case m: Module =>
            val collapsedOMDoc = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">
              {m.toNode}
            </omdoc>
            xml.writeFile(collapsedOMDoc, enrichedDir / Archive.MMTPathToContentPath(m.path))
          case _ => None
        }
      } catch {
        case e: Throwable => log("ERR : " + e)
      }
    }
    log("done:  [CONT -> FLAT]       -> " + inFile)
  }

  def readRelational(in: FilePath = EmptyPath, controller: Controller, kd: String): Unit = {
    if ((this / relational).exists) {
      traverse(relational, in, Archive.traverseIf(kd)) { case Current(inFile, inPath) =>
        utils.File.ReadLineWise(inFile) { line =>
          val re = controller.relman.parse(line, NamespaceMap(DPath(narrationBase)))
          re match {
            case Relation(Includes, to: MPath, from: MPath) =>
              controller.library.addImplicit(OMMOD(from), OMMOD(to), OMIDENT(OMMOD(to)))
            case Relation(HasMeta, thy: MPath, meta: MPath) =>
              controller.library.addImplicit(OMMOD(meta), OMMOD(thy), OMIDENT(OMMOD(thy)))
            case _ =>
          }
          controller.depstore += re
        }
      }
    }
  }
}

object Archive {
  /** a string containing all characters that are illegal in file names */
  val illegalChars = "'"

  // TODO: (un)escape illegal characters, make case-insensitive distinct
  def escape(s: String): String = s.replace("'", "(apos)")

  def unescape(s: String): String = s.replace("(apos)", "'")

  // scheme..authority / seg / ments / name.omdoc ----> scheme :// authority / seg / ments ? name
  def ContentPathToMMTPath(segs: FilePath): MPath = segs.segments match {
    case Nil => throw ImplementationError("")
    case hd :: tl =>
      val p = hd.indexOf("..")
      val fileNameNoExt = tl.length match {
        case 0 => throw ImplementationError("")
        case _ => tl.last.lastIndexOf(".") match {
          case -1 => tl.last
          case i => tl.last.substring(0, i)
        }
      }
      DPath(URI(hd.substring(0, p), hd.substring(p + 2)) / tl.init) ? unescape(fileNameNoExt)
  }

  // scheme..authority / seg / ments  ----> scheme :// authority / seg / ments
  def ContentPathToDPath(segs: FilePath): DPath = segs.segments match {
    case Nil => DPath(URI.empty)
    case hd :: tl =>
      val p = hd.indexOf("..")
      DPath(URI(hd.substring(0, p), hd.substring(p + 2)) / tl)
  }

  /** Get the disk path of the module in the content folder
    * @param m the MPath of the module
    * @return the File descriptor of the destination  file in the content folder
    */
  def MMTPathToContentPath(m: MPath): FilePath = {
    // TODO: Use narrationBase instead of "NONE"?
    val uri = m.parent.uri
    val schemeString = uri.scheme.fold("")(_ + "..")
    FilePath(
      (schemeString + uri.authority.getOrElse("NONE")) :: uri.path :::
        List(escape(m.name.toPath) + ".omdoc"))
  }

  /**
   * Makes sure that a path refers to a file, not to a folder, using .extension files to store information about folders
   * @param segs a path in a folder with narration structure
   * @param extension a file extension
   * @return segs with an appended segment ".extension" if there is no such segment yet
   */
  def narrationSegmentsAsFile(segs: FilePath, extension: String): FilePath = {
    if (segs.segments.nonEmpty && segs.segments.last.endsWith("." + extension)) segs
    else FilePath(segs.segments ::: List("." + extension))
  }

  /**
   * Inverse of narrationSegmentsAsFile
   * @param segs a path in a folder with narration structure that ends in ".extension"
   * @param extension a file extension
   * @return segs with a final segment ".extension" removed if there is one
   *
   *         This is inverse to narrationSegmentsAsFile if all files and no folders in the respective dimension end in ".extension"
   */
  def narrationSegmentsAsFolder(segs: FilePath, extension: String): FilePath = {
    if (segs.segments.nonEmpty && segs.segments.last == "." + extension) segs.dirPath
    else segs
  }

  /** returns a functions that filters by file name extension */
  def extensionIs(e: String): String => Boolean = _.endsWith("." + e)

  /** returns a trivial TraverseMode */
  def traverseIf(e: String): TraverseMode = TraverseMode(extensionIs(e), _ => true, parallel = false)
}
