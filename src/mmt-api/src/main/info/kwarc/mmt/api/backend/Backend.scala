package info.kwarc.mmt.api.backend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils._

/** the backend of a [[Controller]]
  *
  * holds a list of [[Storage]]s and uses them to dereference MMT URIs,
  */
class Backend(controller: Controller) extends Logger {
  protected val report = controller.report
  /** the registered storages */
  private var stores: List[Storage] = List(DefaultRealizationLoader)
  val logPrefix = "backend"

  /** adds a storage */
  def addStore(s: Storage*): Unit = {
    stores = stores ::: s.toList
    s.foreach { d =>
      log("adding storage " + d.toString)
    }
  }

  /** removes a Storage (if present) */
  def removeStore(s: Storage): Unit = {
    stores = stores.filter(_ != s)
    log("removing storage " + s.toString)
  }

  /** retrieves all Stores */
  def getStores: List[Storage] = stores

  def clear: Unit = {
    stores.foreach(_.clear)
  }

  /** releases all resources held by storages */
  def cleanup: Unit = {
    stores.foreach(_.destroy)
    stores = Nil
  }

  /** looks up a path in the first Storage that is applicable and sends the content to the reader
    *
    * the registered storages are searched in the order of registration
    *
    * throws [[NotApplicable]] if the resource is not known/available, [[BackendError]] if it is but something goes wrong
    */
  def load(nf: NotFound)(implicit controller: Controller): Unit = {
    val p = nf.path
    log("loading " + p)
    var messages: List[String] = Nil
    stores foreach {hd =>
      //log("trying " + hd)
      try {
        nf.found match {
          case None => hd.load(p)
          case Some(exPath) => hd.loadFragment(p, exPath)
        }
        return
      } catch {case NotApplicable(msg) =>
        messages ::= hd.toString + " not applicable to " + p + (if (msg != "") " ("+msg+")" else "")
      }
    }
    logGroup {
      //messages.reverse.foreach {m => log(m)}
      log("no backend applicable: " + nf)
    }
    val exMsg = nf.found match {
      case Some(exP) => exP.toString + " exists, but "
      case None => ""
    }
    throw NotApplicable(exMsg + "no backend applicable to " + p)
  }

  /** like load but tries to load a Rule (no side-effects) */
  def loadObjectO(p: MPath): Option[SemanticObject] = {
     stores.foreach {
       case rs: RealizationStorage =>
          try {
            return Some(rs.loadObject(p))
          } catch {
            case NotApplicable(_) =>
          }
       case _ =>
     }
     None
  }

  /** like load but tries to load a Java class */
  def loadClass(cls: String): Option[Class[_]] = {
     val p = SemanticObject.javaToMMT(cls)
     stores.foreach {
       case rs: RealizationStorage =>
          try {
            return Some(rs.loadClass(cls, p))
          } catch {
            case NotApplicable(_) =>
          }
       case _ =>
     }
     None
  }

  private def manifestLocations(root: File) = List(root / "META-INF", root).map(_ / "MANIFEST.MF")

  private def manifestLocation(root: File): Option[File] =
    manifestLocations(root).find { f =>
      if (f.isFile) {
        val t1 = f.toString
        val t2 = f.getCanonicalPath
        t1 == t2
      }
      else false
    }

  /**
    * like openArchiveIf but with trivial condition
    */
  def openArchive(root: File): List[Archive] = openArchiveIf(root, _ => true)
  /** opens archives: an archive folder, or a mar file, or any other folder recursively
    *
    * @param root the file/folder containing the archive(s)
    * @param cond condition on whether to add a found archive
    * @return the opened archives
    * throws NotApplicable if the root is neither a folder nor a mar file
    */
  def openArchiveIf(root: File, cond: Archive => Boolean): List[Archive] = {
    if (getArchive(root).isDefined) {
       // already open
       Nil
    } else if (root.isDirectory) {
      val manifestOpt = manifestLocation(root)
      manifestOpt match {
        case Some(manifest) =>
          val properties = File.readProperties(manifest)
          if (!properties.isDefinedAt("id")) {
            val generatedId = root.up.getName + "/" + root.getName
            properties += (("id", generatedId))
          }
          val arch = new Archive(root, properties, report)
          if (!cond(arch)) Nil else {
            log(s"adding archive ${arch.id} defined by $manifest")
            getArchive(arch.id) match {
              case Some(a) =>
                logError(s"an archive with id ${arch.id} already exists at location ${a.root}")
              case None =>
            }
            addStore(arch)
            val files = arch.classpath.map {cp =>
              val rF = root / cp
              log("loading realization archive" + rF)
              rF
            }
            val parent: Unit => Option[RealizationArchive] = arch.properties.get("scaladep") match {
              case None => _ => None
              case Some(a) =>
                _ => {
                  getArchive(a) match {
                    case Some(arch) =>
                      stores.collectFirst {
                        case ra: RealizationArchive if ra.files.exists(arch.root <= _) => ra
                      }
                    case _ => None
                  }
                }
            }
            val ra = new RealizationArchive(files,parent)
            addStore(ra)
            // try to open all dependencies as well by searching for them under the mathhub root
            val dependencyArchives = {
              val neededDeps = arch.dependencies.filter(id => getArchive(id).isEmpty)
              if (neededDeps.isEmpty) Nil else {
                controller.getMathHub match {
                  case None =>
                    log("cannot load dependencies because no mathhub root is set")
                    Nil
                  case Some(mh) =>
                    log("loading dependencies: " + neededDeps.mkString(", "))
                    openArchiveIf(mh.local,a => neededDeps contains a.id)
                }
              }
            }
            arch :: dependencyArchives
          }
        case None =>
          log(root.toString + " is not an archive - recursing")
          // folders beginning with . are skipped
          root.list.toList.sorted flatMap (n => if (n.startsWith(".")) Nil else openArchiveIf(root / n, cond))
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
            unpackedRoot.deleteDir
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
    } else {
      log(root.toString + " is not an archive or a folder containing archives")
      Nil
    }
  }

  /** unregisters an Archive with a given id */
  def closeArchive(id: String): Unit = {
    getArchive(id) foreach { arch =>
      removeStore(arch)
      removeStore(arch.narrationBackend)
    }
  }

  /** unregisters all archives */
  def closeAllArchives: Unit = {
    getArchives foreach {a => closeArchive(a.id)}
  }

  /** closes all archives, then opens them again */
  def reopenArchives: Unit = {
    val paths = getArchives.map(_.root)
    closeAllArchives
    paths foreach {p => openArchive(p)}
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
    val cp = Archive.MMTPathToContentPath(p.mainModule)
    getArchives find { a =>
      (a / content / cp).existsCompressed
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
  /** like resolveAnyPhysical but automatically loads and returns the archive */
  def resolveAnyPhysicalAndLoad(f: File): Option[(Archive, List[String])] = {
    resolveAnyPhysical(f) flatMap {
      case (root,rel) =>
        openArchive(root)
        getArchive(root) map {a => (a, rel.tail)}
    }
  }

  /** auxiliary function of openArchive */
  private def extractMar(file: File, newRoot: File): Unit = {
    log("unpacking archive " + file + " to " + newRoot)
    File.unzip(file, newRoot)
  }
}
