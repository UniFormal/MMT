package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import utils._
import frontend._
import archives._

import scala.xml._
import info.kwarc.mmt.api.utils.MyList.fromList
import java.util.zip._

import utils.File
import utils.FileConversion._

import org.tmatesoft.svn.core._
import org.tmatesoft.svn.core.io._
import org.tmatesoft.svn.core.auth._
import org.tmatesoft.svn.core.wc.SVNWCUtil

// local XML databases or query engines to access local XML files: baseX or Saxon

case class NotApplicable(message: String = "") extends java.lang.Throwable

/**
 * An abstraction over physical storage units that hold MMT content
 */
abstract class Storage {
   protected def loadXML(u: URI, n:NodeSeq)(implicit controller: Controller) {
      controller.xmlReader.readDocuments(DPath(u), n) {e => controller.add(e)}
   }
   protected def getSuffix(base : utils.URI, uri : utils.URI) : List[String] = {
      val b = base.pathNoTrailingSlash
      val u = uri.pathNoTrailingSlash
      if (uri.scheme == base.scheme && uri.authority == base.authority && u.startsWith(b))
         u.drop(b.length)
      else
         throw NotApplicable()
      }
   protected def virtDoc(entries : List[String], prefix : String) =
      <omdoc>{entries.map(n => <dref target={prefix + n}/>)}</omdoc>
   
   /**
    * dereferences a path and sends the content to a reader
    * 
    * a storage may send more/additional content, e.g., the containing file or a dependency closure,
    */
   def load(path : Path)(implicit controller: Controller)
}

/** a Storage that retrieves file URIs from the local system */
case class LocalSystem(base : URI) extends Storage {
   val localBase = URI(Some("file"), None, Nil, true, None, None)
   def load(path : Path)(implicit controller: Controller) {
      val uri = base.resolve(path.doc.uri)
      val _ = getSuffix(localBase, uri)
      val file = new java.io.File(uri.toJava)
      val N = utils.xml.readFile(file)
      loadXML(uri, N)
   }
}

/** a Storage that retrieves repository URIs from the local working copy */
case class LocalCopy(scheme : String, authority : String, prefix : String, base : File) extends Storage  {
   def localBase = URI(scheme + "://" + authority + prefix) 
   def load(path : Path)(implicit controller: Controller) {
      val uri = path.doc.uri
      val target = base / getSuffix(localBase,uri)
      val N = if (target.isFile) utils.xml.readFile(target)
        else if (target.isDirectory) {
          val entries = target.list().toList.filter(x => x != ".svn" && x != ".omdoc") //TODO: should be an exclude pattern
          val relativePrefix = if (uri.path.isEmpty) "" else uri.path.last + "/"
          virtDoc(entries, relativePrefix)
        } else throw BackendError("file/folder " + target + " not found or not accessible", path)
      loadXML(uri, N)
   }
}

/** a Storage that retrieves repository URIs over SVN connection */
case class SVNRepo(scheme : String, authority : String, prefix : String, repository : SVNRepository, defaultRev : Int = -1) extends Storage  {
   def localBase = URI(scheme + "://" + authority + prefix) 
   
   def load(path : Path)(implicit controller: Controller) {load(path, defaultRev)}
   def load(path : Path, rev: Int)(implicit controller: Controller) {
      val uri = path.doc.uri
      val target = getSuffix(localBase, uri).mkString("/")
      
      val revision = path.doc.version match {
        case None => rev
        case Some(s) => try {s.toInt} catch {case _ : Throwable => rev}
      }
      val N : scala.xml.Node = repository.checkPath(target, revision) match {
        case SVNNodeKind.FILE => 
          var fileProperties : SVNProperties = new SVNProperties()
          val  baos : java.io.ByteArrayOutputStream = new java.io.ByteArrayOutputStream()
          repository.getFile(target, revision, null, baos)
          scala.xml.Utility.trim(scala.xml.XML.loadString(baos.toString))
        case SVNNodeKind.DIR =>
          val coll : java.util.Collection[SVNDirEntry] = null
          val entries = repository.getDir(target, revision, null, coll)
          val prefix = if (target != "") target + "/" else ""
          var it = entries.iterator()
          var strEntries : List[SVNDirEntry] = Nil
          while (it.hasNext()) {
            it.next match {
              case e : SVNDirEntry => strEntries = e :: strEntries
              case _ => None
            }
          }
          virtDoc(strEntries.reverse.map(x => x.getURL.getPath), prefix) //TODO check if path is correct
        case SVNNodeKind.NONE => throw BackendError("not found or not accessible", path)
      }
      loadXML(uri, N)
   }
}

/**
 * loads a realization from a Java Class Loader and dynamically creates a [[modules.Realization]] for it
 */
class RealizationArchive(file: File, val loader: java.net.URLClassLoader) extends Storage {
   override def toString = "RealizationArchive for " + file
   def load(path : Path)(implicit controller: Controller) {
      val mp = path match {
         case mp: MPath => mp
         case GlobalName(objects.OMMOD(mp), _) => mp
         case _ => throw NotApplicable("no module path found")
      }
      val s = uom.GenericScalaExporter.mpathToScala(mp)
      controller.report("backend", "trying to load class " + s + "$")
      val c = try {Class.forName(s + "$", true, loader)}
         catch {
            case e: ClassNotFoundException =>
               throw NotApplicable("class not found")
            case e: ExceptionInInitializerError =>
               throw BackendError("class for " + mp + " exists, but an error occurred when initializing it", mp).setCausedBy(e)
            case e: LinkageError =>
               throw BackendError("class for " + mp + " exists, but an error occurred when linking it", mp).setCausedBy(e)
         }
      val r = try {c.getField("MODULE$").get(null).asInstanceOf[uom.RealizationInScala]}
           catch {
              case e : java.lang.Exception =>
               throw BackendError("realization for " + mp + " exists, but an error occurred when creating it", mp).setCausedBy(e)
           }
      r.init
      controller.add(r)
   }       
}

/**
 * the backend of a [[Controller]]
 * 
 * holds a list of [[Storage]]s and uses them to dereference MMT URIs,
 */
class Backend(extman: ExtensionManager, val report : info.kwarc.mmt.api.frontend.Report) extends Logger {
   /** the registered storages */
   private var stores : List[Storage] = Nil
   val logPrefix = "backend"
   /** adds a Storgage */
   def addStore(s : Storage*) {
      stores = stores ::: s.toList
      s.foreach {d =>
         log("adding storage " + d.toString)
      }
   }
   /** removes a Storage (if present) */
   def removeStore(s: Storage) {
      stores = stores.filter(_ != s)
      log("removing storage " + s.toString)
   }
   /** retrieves all Stores */
   def getStores : List[Storage] = stores

   /** releases all ressources held by storages */
   def cleanup = {
     stores.map(x => x match {
       case SVNRepo(scheme,authority,prefix,repo, rev) => repo.closeSession() // closes all svn sessions
       case _ => None
     })
   }
   
   /**
    * looks up a path in the first Storage that is applicable and sends the content to the reader
    * the registered storages are searched in the order of registration
    */
   def load(p : Path)(implicit controller: Controller) = {
      def getInList(l : List[Storage], p : Path) {l match {
         case Nil => throw BackendError("no applicable backend available", p)
         case hd :: tl =>
            log("trying " + hd)
             try {hd.load(p)}
            catch {case NotApplicable(msg) =>
               log(hd.toString + " not applicable to " + p + (if (msg != "") s" ($msg)" else ""))
               getInList(tl, p)
            }
      }}
      getInList(stores, p)
   }
   
   /** 
    * opens archives: an archive folder, or a mar file, or any other folder recursively
    * @param root the file/folder containing the archive(s)
    * @return the opened archives   
    * @throws NotApplicable if the root is neither a folder nor a mar file
    */
   def openArchive(root: java.io.File) : List[Archive] = {
      if (root.isDirectory) {
          val manifest = root / "META-INF" / "MANIFEST.MF"
          if (manifest.exists) {
             log("opening archive defined by " + manifest)
             val properties = new scala.collection.mutable.ListMap[String,String]
             var compsteps : Option[List[CompilationStep]] = Some(Nil) 
             if (manifest.isFile) {
                File.ReadLineWise(manifest) {case line =>
                   val tline = line.trim
                   if (! tline.startsWith("//") && tline != "") {
                      val p = tline.indexOf(":")
                      val key = tline.substring(0,p).trim
                      val value = tline.substring(p+1).trim
                      properties(key) = value
                   }
                }
             }
             val arch = new Archive(root, properties, report)
             addStore(arch)
             List(arch)
          } else {
             log(root + " is not an archive - recursing")
             root.list.toList flatMap (n => openArchive(root / n))
          }
      } else if (root.isFile && root.getPath.endsWith(".mar")) {    // a MAR archive file
          val folder = root.getParentFile
          val name = root.getName
          val newRoot = folder / (name + "-unpacked")
          // timestamp to remember last unpacking
          val ts = new Timestamps(folder, newRoot / "META-INF" / "timestamps")
          val mod = ts.modified(List(name)) 
          if (mod == Modified) {
             newRoot.deleteDir
          }
          if (List(Added, Modified) contains mod) {
             // unpack it
             extractMar(root, newRoot)
             ts.set(List(name))
          }
          if (mod == Unmodified)
             log("skipping unpacked, unmodified archive " + newRoot)
          // open the archive in newRoot
          openArchive(newRoot)
      }
      else {
         log(root + " is not an archive or a folder containing archives")
         Nil
      }
   }
   /** unregisters an Archive with a given id */
   def closeArchive(id: String) {
      getArchive(id) foreach {arch =>
         removeStore(arch)
         removeStore(arch.narrationBackend)
      }
   }
   /** retrieve an Archive by its id */
   def getArchive(id: String) : Option[Archive] = stores mapFind {
      case a: Archive => if (a.properties.get("id") == Some(id)) Some(a) else None
      case _ => None
   }
   /** retrieves all Archives */
   def getArchives : List[Archive] = stores mapPartial {
      case a: Archive => Some(a)
      case _ => None
   }
   /**
    * @param p a module URI
    * @return an archive defining it (the corresponding file exists in content dimension)
    */
   def findOwningArchive(p: MPath) : Option[Archive] = {
      val cp = Archive.MMTPathToContentPath(p)
      getArchives find {a => 
        (a/content/cp).exists
      }
   }
   /** splits a logical document URI into the Archive holding it and the relative path in that archive leading to it */
   def resolveLogical(uri: URI) : Option[(Archive, List[String])] = {
      getArchives find {a => 
        a.narrationBase.^! == uri.^! && uri.path.startsWith(a.narrationBase.pathNoTrailingSlash)
      } map {a => (a, uri.path.drop(a.narrationBase.pathNoTrailingSlash.length))}
   }
   /** splits a physcial document URI into the Archive holding it and the relative path in that archive leading to it */
   def resolvePhysical(file: File) : Option[(Archive, List[String])] = {
      val segments = file.segments
      getArchives find {a => segments.startsWith(a.root.segments)} map {
        a => (a, segments.drop(a.root.segments.length + 1)) // + 1 to drop "source" directory
      }
   }
   
   /** creates and registers a RealizationArchive */
   def openRealizationArchive(file: File) {
       val loader = try {
          val cl = getClass.getClassLoader // the class loader that loaded this class, may be null for bootstrap class loader
          if (cl == null)
             new java.net.URLClassLoader(Array(file.toURI.toURL)) // parent defaults to bootstrap class loader
          else
              // delegate to the class loader that loaded MMT - needed if classes to be loaded depend on MMT classes
             new java.net.URLClassLoader(Array(file.toURI.toURL), cl)
       } catch {
          case _:Exception => 
            logError("could not create class loader for " + file.toString)
            return
       }
       val ra = new RealizationArchive(file, loader)
       addStore(ra)
   }
   
   //TODO this must be redesigned
   def copyStorages(newRev : Int = -1) : List[Storage] = {
     stores.map(s => s match {
       case SVNRepo(sch, auth, pref, repo, rev) => new SVNRepo(sch,auth,pref, repo, newRev)
       case _ => s
     })
   }
  
   /** creates and registers an SVNArchive */
   def openArchive(url : String, rev : Int) : SVNArchive = {
     log("opening archive at " + url + ":" + rev)
     val repository = SVNRepositoryFactory.create( SVNURL.parseURIEncoded( url ) )
     val authManager : ISVNAuthenticationManager = SVNWCUtil.createDefaultAuthenticationManager()
     repository.setAuthenticationManager(authManager)
     repository.checkPath(".", rev) match {
       case SVNNodeKind.DIR =>
         val properties = new scala.collection.mutable.ListMap[String, String]
         val manifest = "META-INF/MANIFEST.MF"
         repository.checkPath(manifest, rev) match {
           case SVNNodeKind.FILE =>
             val  baos : java.io.ByteArrayOutputStream = new java.io.ByteArrayOutputStream()
             repository.getFile(manifest, rev, null, baos)
             val lines = baos.toString().split("\n").map(_.trim).filterNot(line => line.startsWith("//") || line.isEmpty)
             lines map { line =>
               val p = line.indexOf(":")
               val key = line.substring(0,p).trim
               val value = line.substring(p+1).trim
               properties(key) = value
               //TODO handle catalog key
             }
             val arch = new SVNArchive(repository, properties, report, rev)
             addStore(arch)
             arch

           case _ => throw NotApplicable()
         }
       case SVNNodeKind.FILE => throw NotApplicable() //TODO
       case _ => throw NotApplicable()
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
           if (! entry.isDirectory) {
               val istream = mar.getInputStream(entry)
               val ostream = new java.io.FileOutputStream(outFile)
               while({len = istream.read(bytes, 0, bytes.size); len != -1 })
               ostream.write(bytes, 0, len)
               ostream.close
               istream.close
           }
       }
   }
}