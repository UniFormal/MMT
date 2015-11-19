package info.kwarc.mmt.api.backend

import java.io.BufferedReader
import java.util.zip._

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils._

// local XML databases or query engines to access local XML files: baseX or Saxon

case class NotApplicable(message: String = "") extends java.lang.Throwable

/** An abstraction over physical storage units that hold MMT content */
abstract class Storage {
  protected def loadXML(u: URI, dpath: DPath, reader: BufferedReader)(implicit controller: Controller) {
    val ps = new parser.ParsingStream(u, dpath, NamespaceMap(dpath), "omdoc", reader)
    controller.read(ps, interpret = false)(ErrorThrower)
  }

  protected def getSuffix(base: utils.URI, uri: utils.URI): List[String] = {
    val b = base.pathNoTrailingSlash
    val u = uri.pathNoTrailingSlash
    if (uri.scheme == base.scheme && uri.authority == base.authority && u.startsWith(b))
      u.drop(b.length)
    else
      throw NotApplicable()
  }

  protected def virtDoc(entries: List[String], prefix: String) = {
    val s = <omdoc>
      {entries.map(n => <dref name={n} target={prefix + n}/>)}
    </omdoc>.toString
    new BufferedReader(new java.io.StringReader(s))
  }

  /** dereferences a path and sends the content to a reader
    *
    * a storage may send more/additional content, e.g., the containing file or a dependency closure,
    */
  def load(path: Path)(implicit controller: Controller)

  /** called to release all held resources, override as needed */
  def destroy() {}
}

/** a Storage that retrieves file URIs from the local system */
case class LocalSystem(base: URI) extends Storage {
  val localBase = URI(Some("file"), None, Nil, abs = true, None, None)

  def load(path: Path)(implicit controller: Controller) {
    val uri = base.resolve(path.doc.uri)
    val _ = getSuffix(localBase, uri)
    val file = new java.io.File(uri.toJava)
    loadXML(uri, path.doc, File.Reader(file))
  }
}

/** a Storage that retrieves repository URIs from the local working copy */
case class LocalCopy(scheme: String, authority: String, prefix: String, base: File) extends Storage {
  def localBase = URI(scheme + "://" + authority + prefix)

  def load(path: Path)(implicit controller: Controller) {
    val uri = path.doc.uri
    val target = base / getSuffix(localBase, uri)
    val reader = if (target.isFile) File.Reader(target)
    else if (target.isDirectory) {
      val entries = target.list.toList.sorted.diff(List(".svn"))
      val relativePrefix = if (uri.path.isEmpty) "" else uri.path.last + "/"
      virtDoc(entries, relativePrefix)
    } else throw BackendError("file/folder " + target + " not found or not accessible", path)
    loadXML(uri, path.doc, reader)
  }
}

/** loads a realization from a Java Class Loader and dynamically creates a [[uom.RealizationInScala]] for it */
class RealizationArchive(file: File, val loader: java.net.URLClassLoader) extends Storage {
  override def toString = "RealizationArchive for " + file

  def load(path: Path)(implicit controller: Controller) {
    val mp = path match {
      case mp: MPath => mp
      case GlobalName(objects.OMMOD(mp1), _) => mp1
      case _ => throw NotApplicable("no module path found")
    }
    val s = uom.GenericScalaExporter.mpathToScala(mp) + "$"
    controller.report("backend", "trying to load class " + s)
    val c = try {
      Class.forName(s, true, loader)
    }
    catch {
      case e: ClassNotFoundException =>
        throw NotApplicable("class " + s + " not found")
      case e: ExceptionInInitializerError =>
        throw BackendError("class for " + mp + " exists, but an error occurred when initializing it", mp).setCausedBy(e)
      case e: LinkageError =>
        throw BackendError("class for " + mp + " exists, but an error occurred when linking it", mp).setCausedBy(e)
      case e: Error =>
        throw BackendError("class for " + mp + " exists, but: " + e.getMessage, mp).setCausedBy(e)
    }
    val r = try {
      c.getField("MODULE$").get(null).asInstanceOf[uom.RealizationInScala]
    }
    catch {
      case e: java.lang.Exception =>
        throw BackendError("realization for " + mp + " exists, but an error occurred when creating it", mp).setCausedBy(e)
    }
    try {
      r.init
    }
    catch {
      case e: Error =>
        throw BackendError("body of " + mp + " ill-formed: " + e.getMessage, mp).setCausedBy(e)
    }
    controller.add(r)
  }
}

/** the backend of a [[Controller]]
  *
  * holds a list of [[Storage]]s and uses them to dereference MMT URIs,
  */
class Backend(extman: ExtensionManager, val report: info.kwarc.mmt.api.frontend.Report) extends Logger {
  /** the registered storages */
  private var stores: List[Storage] = Nil
  val logPrefix = "backend"

  /** adds a storage */
  def addStore(s: Storage*) {
    stores = stores ::: s.toList
    s.foreach { d =>
      log("adding storage " + d.toString)
    }
  }

  /** removes a Storage (if present) */
  def removeStore(s: Storage) {
    stores = stores.filter(_ != s)
    log("removing storage " + s.toString)
  }

  /** retrieves all Stores */
  def getStores: List[Storage] = stores

  /** releases all resources held by storages */
  def cleanup() {
    stores.foreach(_.destroy())
    stores = Nil
  }

  /** looks up a path in the first Storage that is applicable and sends the content to the reader
    *
    * the registered storages are searched in the order of registration
    *
    * throws [[NotApplicable]] if the resource is not known/available, [[BackendError]] if it is but something goes wrong
    */
  def load(p: Path)(implicit controller: Controller): Unit = {
    def loadStores(l: List[Storage], p: Path) {
      l match {
        case Nil => throw NotApplicable("no backend available that is applicable to " + p)
        case hd :: tl =>
          log("trying " + hd)
          try {
            hd.load(p)
          }
          catch {
            case NotApplicable(msg) =>
              log(hd.toString + " not applicable to " + p + (if (msg != "") s" ($msg)" else ""))
              loadStores(tl, p)
          }
      }
    }
    loadStores(stores, p)
  }

  private def manifestLocations(root: File) = List(root / "META-INF", root).map(_ / "MANIFEST.MF")

  private def manifestLocation(root: File): Option[File] =
    manifestLocations(root).find { f =>
      if (f.isFile) {
        val t1 = f.toString
        val t2 = f.getCanonicalPath
        if (t1 != t2) log("manifest: " + t1 + "\ndoes not match: " + t2)
        else log("manifest file: " + t1)
        t1 == t2
      }
      else false
    }

  /** opens archives: an archive folder, or a mar file, or any other folder recursively
    *
    * @param root the file/folder containing the archive(s)
    * @return the opened archives
    * @throws NotApplicable if the root is neither a folder nor a mar file
    */
  def openArchive(root: File): List[Archive] = {
    if (getArchive(root).isDefined)
       // already open
       Nil
    else if (root.isDirectory) {
      val manifestOpt = manifestLocation(root)
      manifestOpt match {
        case Some(manifest) =>
          val properties = File.readProperties(manifest)
          if (properties.isDefinedAt("id")) {
            log("adding archive defined by " + manifest)
          } else {
            val generatedId = root.up.getName + "/" + root.getName
            log(manifest + " does not contain id, creating " + generatedId)
            properties += (("id", generatedId))
          }
            val arch = new Archive(root, properties, report)
            addStore(arch)
            List(arch)
        case None =>
          log(root + " is not an archive - recursing")
          // folders beginning with . are skipped
          root.list.toList.sorted flatMap (n => if (n.startsWith(".")) Nil else openArchive(root / n))
      }
    } else if (root.isFile && root.getPath.endsWith(".mar")) {
      // a MAR archive file
      val folder = root.up
      val name = root.name
      val unpackedRoot = folder / (name + "-unpacked")
      // check if root is younger than manifest in unpackedRoot
      val extract = manifestLocation(unpackedRoot) match {
        case Some(unpackedManifest) =>
          val mod = Modification(root, unpackedManifest)
          if (mod == Modified) {
            unpackedRoot.deleteDir()
          }
          if (mod == Unmodified)
            log("skipping unpacked, unmodified archive " + unpackedRoot)
          List(Added, Modified) contains mod
        case None =>
          true
      }
      if (extract) {
        // unpack it
        extractMar(root, unpackedRoot)
      }
      // open the archive in newRoot
      openArchive(unpackedRoot)
    }
    else {
      log(root + " is not an archive or a folder containing archives")
      Nil
    }
  }

  /** unregisters an Archive with a given id */
  def closeArchive(id: String) {
    getArchive(id) foreach { arch =>
      removeStore(arch)
      removeStore(arch.narrationBackend)
    }
  }

  /** retrieve an [[Archive]] by its id */
  def getArchive(id: String): Option[Archive] = stores collectFirst {
    case a: Archive if a.properties.get("id").contains(id) => a
  }

  /** retrieve an [[Archive]] by its root folder */
  def getArchive(root: File): Option[Archive] = stores collectFirst {
     case a: Archive if a.root == root => a
  }

  /** retrieves all Archives */
  def getArchives: List[Archive] = stores collect { case a: Archive => a }

  /** find the archive of a module path
    *
    * @param p a module URI
    * @return an archive defining it (the corresponding file exists in content dimension)
    */
  def findOwningArchive(p: MPath): Option[Archive] = {
    val cp = Archive.MMTPathToContentPath(p)
    getArchives find { a =>
      (a / content / cp).exists
    }
  }

  /** splits a logical document URI into the Archive holding it and the relative path in that archive leading to it */
  def resolveLogical(uri: URI): Option[(Archive, List[String])] = {
    getArchives find { a =>
      a.narrationBase.^! == uri.^! && uri.path.startsWith(a.narrationBase.pathNoTrailingSlash)
    } map { a => (a, uri.path.drop(a.narrationBase.pathNoTrailingSlash.length)) }
  }

  /** splits a physical document URI into the Archive holding it and the relative path in that archive leading to it */
  def resolvePhysical(file: File): Option[(Archive, List[String])] = {
    val segments = file.segments
    getArchives find { a => segments.startsWith(a.root.segments) } map {
      a => (a, segments.drop(a.root.segments.length + 1)) // + 1 to drop "source" directory
    }
  }

  /** like resolvePhysical but irrespective of loaded archives
   *  @return archive root and relative path in archive
   */
  def resolveAnyPhysical(f: File): Option[(File, FilePath)] = {
    if (f.isDirectory && manifestLocation(f).isDefined)
      Some((f, EmptyPath))
    else {
       if (f.isRoot) None
       else {
         resolveAnyPhysical(f.up).map {case (root, fp) => (root, fp / f.name)}
       }
    }
  }

  /** creates and registers a RealizationArchive */
  def openRealizationArchive(file: File) {
    val loader = try {
      val optCl = Option(getClass.getClassLoader)
      // the class loader that loaded this class, may be null for bootstrap class loader
      optCl match {
        case None =>
          new java.net.URLClassLoader(Array(file.toURI.toURL)) // parent defaults to bootstrap class loader
        case Some(cl) =>
          // delegate to the class loader that loaded MMT - needed if classes to be loaded depend on MMT classes
          new java.net.URLClassLoader(Array(file.toURI.toURL), cl)
      }
    } catch {
      case _: Exception =>
        logError("could not create class loader for " + file.toString)
        return
    }
    val ra = new RealizationArchive(file, loader)
    addStore(ra)
  }

  //TODO this must be redesigned
  def copyStorages(newRev: Int = -1): List[Storage] = {
    stores.map {
      case s => s
    }
  }

  /** auxiliary function of openArchive */
  private def extractMar(file: File, newRoot: File) {
    log("unpacking archive " + file + " to " + newRoot)
    val mar = new ZipFile(file)
    var bytes = new Array[Byte](100000)
    var len = -1
    val enum = mar.entries
    while (enum.hasMoreElements) {
      val entry = enum.nextElement
      val outFile = newRoot / entry.getName
      outFile.getParentFile.mkdirs
      if (!entry.isDirectory) {
        val istream = mar.getInputStream(entry)
        val ostream = new java.io.FileOutputStream(outFile)
        while ( {
          len = istream.read(bytes, 0, bytes.length)
          len != -1
        })
          ostream.write(bytes, 0, len)
        ostream.close()
        istream.close()
      }
    }
  }
}
