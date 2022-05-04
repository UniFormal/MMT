package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils._

import scala.collection.mutable
import scala.util.Try

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
}

/** archive management
  *
  * Archive is a very big class, so most of its functionality is outsourced to various traits that are mixed in here
  *
  * @param root the root folder that contains the source folder
  * @param properties a key value map
  * @param report the reporting mechanism
  */
class Archive(val root: File, val properties: mutable.Map[String, String], val report: Report) extends ROArchive with Validate with ZipArchive {

  val rootString = root.toString
  val archString = root.up.getName + "/" + root.getName
  val id = properties("id")
  val narrationBase = properties.get("narration-base").map(utils.URI(_)).getOrElse(FileURI(root))
  /** the NamespaceMap built from the ns and ns-prefix properties */
  val ns = properties.get("ns").map(s => Path.parse(
    s,//if (s.last == '/') s.dropRight(1) else s,
    NamespaceMap.empty))
  val namespaceMap = {
    var nsMap = NamespaceMap.empty
    val Matcher = utils.StringMatcher("", "-", "")
    properties.foreach {
      case ("ns", uri) => nsMap = nsMap.base(uri)
      case (Matcher("ns", prefix), uri) => nsMap = nsMap.add(prefix, uri)
      case _ =>
    }
    nsMap
  }

  val foundation = properties.get("foundation").map(s => Path.parseM(s,namespaceMap))

  /** the absolute path to a given dimension */
  def /(dim: ArchiveDimension) = root / resolveDimension(dim)

  /** the relative path in the archive of a give dimension */
  def resolveDimension(dim: ArchiveDimension): FilePath = dim match {
    case r@RedirectableDimension(key, _) => properties.get(key) match {
      case Some(p) => FilePath(p)
      case None => resolveDimension(r.default)
    }
    case Dim(path@_*) => FilePath(path.toList)
  }

  def includeDir(n: String): Boolean = !List(".svn", ".mmt", ".git").contains(n)

  val narrationBackend = new ArchiveNarrationStorage(this, "desc")

  def load(p: Path)(implicit controller: Controller) {
    p match {
      case doc: DPath =>
         narrationBackend.load(doc)
      case mod: MPath =>
        val topmod = mod.doc ? mod.name.head
        val f = MMTPathToContentPath(topmod)
        if (!f.existsCompressed) throw NotApplicable("file not found")
        // dpath is a dummy URI to be used when creating the Document that contains the module mod
        val dpath = DPath(narrationBase / Archive.MMTPathToContentPath(topmod).segments)
        loadXML(mod.doc.uri, dpath, File.Reader(f))
      case m ?? _ => load(m)
    }
  }

  protected val custom: ArchiveCustomization = {
    properties.get("customization") match {
      case None => new DefaultCustomization
      case Some(c) => java.lang.Class.forName(c).asInstanceOf[java.lang.Class[ArchiveCustomization]].newInstance
    }
  }

  protected def deleteFile(f: File) {
    log("deleting " + f)
    f.delete
  }

  /** Get the disk path of the module in the content folder
    *
    * @param m the MPath of the module
    * @return the File descriptor of the destination  file in the content folder
    */
  def MMTPathToContentPath(m: MPath): File = this / content / Archive.MMTPathToContentPath(m)

  import scala.collection.parallel.CollectionConverters._

  /** traverses a dimension calling continuations on files and subdirectories */
  def traverse[A](dim: ArchiveDimension, in: FilePath, mode: TraverseMode, sendLog: Boolean = true, forClean: Boolean = false)
                 (onFile: Current => A, onDir: (Current, List[A]) => A = (_: Current, _: List[A]) => ()): Option[A] = {
    val TraverseMode(filter, filterDir, parallel) = mode
    def recurse(n: String): List[A] =
      traverse(dim, in / n, mode, sendLog)(onFile, onDir).toList

    lazy val reg = properties.get("ignore").map(_.replace(".","\\.").replace("*",".*").r)
    // if (reg.exists(_.matches("/" + inPath.toString))
    def regfilter(f : File) : Boolean = !reg.exists(_.matches("/" + (this / source).relativize(f).toString))
    val inFile = this / dim / in
    val inFileName = inFile.getName
    if (inFile.isDirectory) {
      if (includeDir(inFileName)) {
        if (sendLog) log("entering " + inFile)
        val children = inFile.list.sorted.toList
        val results = if (parallel) children.par flatMap recurse else children flatMap recurse
        val result = onDir(Current(inFile, in), results.toList)
        if (sendLog) log("leaving  " + inFile)
        Some(result)
      } else None
    } else if (filter(inFileName) && regfilter(inFile) && filterDir(inFile.up.getName)) {
      if (!forClean && !inFile.existsCompressed) {
        throw ArchiveError(id, "file does not exist: " + inFile)
      } else
        Some(onFile(Current(inFile, in)))
    } else {
      log("not an included file or directory: " + in)
      None
    }
  }

  /** Returns (#Theories,#Constants)**/
  def stats(implicit controller: Controller): SimpleStatistics = {
    // val arch = controller.backend.getArchive(a).get
    val ths = allContent.flatMap{mp =>
      Try(controller.get(mp).asInstanceOf[Theory]).toOption
    }
    val const = ths.flatMap(_.getConstants)
    SimpleStatistics(ths.length,const.length)
  }

  /**
    * Kinda hacky; can be used to get all Modules residing in this archive somewhat quickly
    * @return
    */
  @deprecated("inefficient and brittle; use getModules for this","")
  lazy val allContent : List[MPath] = {
    //TODO if it weren't for nested theories, we could simply use controller.getAs(classOf[Document], DPath(narrationBase)).getModules(controller.globalLookup)
    log("Reading Content " + id)
    var ret : List[MPath] = Nil
    if ((this / content).exists) {
      traverse(content, FilePath(""), TraverseMode(s => s.endsWith(".omdoc") || s.endsWith(".omdoc.xz"),_ => true,false)) { case Current(inFileU, inPath) =>
        val inFile = if (inFileU.toString.endsWith(".xz")) File(inFileU.toString.dropRight(3)) else inFileU
        log("in file " + inFile.name)
        /*
          utils.xml.readFile(inFile) match {
            case <omdoc>{mods @ _*}</omdoc> => mods.foreach(m => {
              val namespace = Path.parseD(xml.attr(m, "base"), NamespaceMap.empty)
              val name = LocalName.parse(xml.attr(m, "name"), NamespaceMap.empty)
              ret ::= namespace ? name
            })
          }
          */
        val thexp = "name=\"([^\"]+)\" base=\"([^\"]+)\"".r
        def getLine = {
          val reader = File.Reader(inFile)
          val str = Try(reader.readLine()).getOrElse("")
          reader.close()
          if (str == null) "" else str
        }
        // def mods(s : String) : Option[String] = Some(StringMatcher("<theory",">").findFirstIn(s).getOrElse(StringMatcher("<view",">").findFirstIn(s).getOrElse(return None)))
        thexp.findAllIn(getLine).toList foreach {
          case thexp(name, base) =>
            // Dirty fix for https://github.com/UniFormal/MMT/issues/439
            // until allContent is completely replaced.
            Path.parse(base, NamespaceMap.empty) match {
              case dPath: DPath => ret ::= dPath ? name
              case _ => /* do nothing */
            }
        }
      }
    }
    ret.distinct
  }

  /** gets the direct dependencies of an archive */
  def dependencies: List[String] = {
    stringToList(properties.getOrElse("dependencies", "").replace(",", " "))
  }

  /** returns the list of loaded transitive dependencies of this archive, including itself */
  def transitiveDependencies(backend: Backend): List[Archive] = {
    val handles = scala.collection.mutable.Set[Archive]()

    // keep a q of element to scan for dependencies
    // and keep picking one of them, until there are none left
    val q = scala.collection.mutable.Queue[Archive](this)
    while(q.nonEmpty) {
      val next = q.dequeue()
      if (!handles.contains(next)) {
        handles.add(next)
        q.enqueueAll(next.dependencies.flatMap(s => backend.getArchive(s)))
      }
    }

    handles.toList
  }

  def readRelational(in: FilePath, controller: Controller, kd: String) {
    log("Reading archive " + id)
    if ((this / relational).exists) {
      traverse(relational, in, Archive.traverseIf(kd)) { case Current(inFile, inPath) =>
        log("in file " + inFile.name)
        utils.File.ReadLineWise(inFile) { line =>
          try {
            val re = controller.relman.parse(line, NamespaceMap(DPath(narrationBase)))
            /* this made reading relational very inefficient; anyway a better way to load implicit moprhisms should be found 
            re match {
              case Relation(Includes, to: MPath, from: MPath) =>
                controller.library.addImplicit(OMMOD(from), OMMOD(to), OMIDENT(OMMOD(to)))
              case Relation(HasMeta, thy: MPath, meta: MPath) =>
                controller.library.addImplicit(OMMOD(meta), OMMOD(thy), OMIDENT(OMMOD(thy)))
              case _ =>
            }*/
            controller.depstore += re
          } catch { //TODO treat this as normal build target and report errors
            case e : Error => log(e.getMessage)
          }
        }
      }
    }
  }
}


object Archive {

  private val escaper = FileNameEscaping

  /**
   * scheme..authority / seg / ments / name.omdoc[.xz] ----> scheme :// authority / seg / ments ? name
   */
  def ContentPathToMMTPath(segs: FilePath): MPath = segs.segments match {
    case Nil => throw ImplementationError("")
    case hd :: tl =>
      val p = hd.indexOf("..")
      val fileNameNoExt = tl.length match {
        case 0 => throw ImplementationError("")
        case _ => tl.last.lastIndexOf(".omdoc") match {
          case -1 => tl.last
          case i => tl.last.substring(0, i)
        }
      }
      val scheme = hd.substring(0, p)
      val schemeAuthority = hd.substring(p + 2) match {
        // "NONE" only used in archives built with versions before devel 2022-03
        case "-" | "NONE" => URI(Some(scheme), None, Nil, true) // for absent authority, path may be relative, but we don't want that
        case "EMPTY" => URI(Some(scheme), Some(""))
        case s => URI(scheme, s)
      }
      DPath(schemeAuthority / tl.init) ? escaper.unapply(fileNameNoExt)
  }

  /** scheme..authority / seg / ments  ----> scheme :// authority / seg / ments
   *  file extensions are kept, to be used on folders only
   */
  def ContentPathToDPath(segs: FilePath): DPath = segs.segments match {
    case Nil => DPath(URI.empty)
    case hd :: tl =>
      val p = hd.indexOf("..")
      DPath(URI(hd.substring(0, p), hd.substring(p + 2)) / tl)
  }

  /** Get the disk path of the module in the content folder
    *
    * @param m the MPath of the module
    * @return the File descriptor of the destination  file in the content folder
    */
  def MMTPathToContentPath(m: MPath): FilePath = {
    // TODO: Use narrationBase instead of "NONE"?
    val uri = m.parent.uri
    val schemeString = uri.scheme.fold("")(_ + "..")
    val authString = uri.authority match {
      case Some("") => "EMPTY" // empty authority is rarer than absent authority, so gets awkward case
      case Some(s) => s
      case None => "-" // can't use empty string because paths ending in . are badly supported
    }
    FilePath(
      (schemeString + authString) :: uri.path :::
        List(escaper.apply(m.name.toPath) + ".omdoc"))
  }

  /** Makes sure that a path refers to a file, not to a folder, using .extension files to store information about folders
    *
    * @param segs a path in a folder with narration structure
    * @param extension a file extension
    * @return segs with an appended segment ".extension" if there is no such segment yet
    */
  def narrationSegmentsAsFile(segs: FilePath, extension: String): FilePath = {
    if (segs.segments.nonEmpty && segs.segments.last.endsWith("." + extension)) segs
    else FilePath(segs.segments ::: List("." + extension))
  }

  /** Inverse of narrationSegmentsAsFile
    *
    * This is inverse to narrationSegmentsAsFile if all files and no folders in the respective dimension end in ".extension"
    *
    * @param segs a path in a folder with narration structure that ends in ".extension"
    * @param extension a file extension
    * @return segs with a final segment ".extension" removed if there is one
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

/** very simple statistics implementation */
case class SimpleStatistics(theoryCount: Int, constantCount: Int) {
  def + (other: SimpleStatistics): SimpleStatistics = {
    SimpleStatistics(theoryCount + other.theoryCount, constantCount + other.constantCount)
  }
}
object SimpleStatistics {
  val empty: SimpleStatistics = SimpleStatistics(0, 0)
}