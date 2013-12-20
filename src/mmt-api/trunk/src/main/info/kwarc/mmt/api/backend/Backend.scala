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

case object NotApplicable extends java.lang.Throwable

/** Storage is an abstraction over backends that can provide MMT content
 * A storage declares a URI u and must answer to all URIs that start with u.
 */
abstract class Storage {
   /** dereferences a path and sends the content to a reader
    * a storage may send more content than the path references, e.g., the whole file or a dependency closure
    */
   def get(path : Path)(implicit cont: (URI,NodeSeq) => Unit)
}

/** convenience methods for backends */
object Storage {
   def virtDoc(entries : List[String], prefix : String) =
      <omdoc>{entries.map(n => <dref target={prefix + n}/>)}</omdoc>
   def getSuffix(base : utils.URI, uri : utils.URI) : String = {
      val b = base.pathAsString
      val u = uri.pathAsString
      if (uri.scheme == base.scheme && uri.authority == base.authority && u.startsWith(b))
         u.substring(b.length)
      else
         throw NotApplicable
      }
}

/** a trait for URL-addressed Storages that can serve MMT document fragments */
abstract class OMBase(scheme : String, authority : String, prefix : String, ombase : URI)
         extends Storage {
     def sendRequest(p : String, b : String) : NodeSeq = {
        val url = new java.net.URI(ombase.scheme.getOrElse(null), ombase.authority.getOrElse(null), ombase.pathAsString + p, null, null).toURL
        if (b == "") {
            //GET
	        val src = scala.io.Source.fromURL(url, "utf-8")
	        val N = scala.xml.parsing.ConstructingParser.fromSource(src, false).document()
            src.asInstanceOf[scala.io.BufferedSource].close
            N            
        } else {
            //POST with b as the body
	        val conn = url.openConnection()// returns java.net.HttpURLConnection if url is http
	        conn.setDoOutput(true);
	        val wr = new java.io.OutputStreamWriter(conn.getOutputStream())
	        wr.write(b)   // this automatically sets the request method to POST
	        wr.flush()
	        val src = scala.io.Source.fromInputStream(conn.getInputStream(), "utf-8")
	        val N = scala.xml.parsing.ConstructingParser.fromSource(src, false).document()
	        wr.close()
            src.asInstanceOf[scala.io.BufferedSource].close
            N
        }
     }
     def handleResponse(msg : => String, N : NodeSeq) : NodeSeq
     def get(path : Path)(implicit cont: (URI,NodeSeq) => Unit)
}

/** a Storage that retrieves file URIs from the local system */
case class LocalSystem(base : URI) extends Storage {
   val localBase = URI(Some("file"), None, Nil, true, None, None)
   def get(path : Path)(implicit cont: (URI,NodeSeq) => Unit) {
      val uri = base.resolve(path.doc.uri)
      val test = Storage.getSuffix(localBase, uri)
      val file = new java.io.File(uri.toJava)
      val N = utils.xml.readFile(file)
      cont(uri, N)
   }
}

/** a Storage that retrieves repository URIs from the local working copy */
case class LocalCopy(scheme : String, authority : String, prefix : String, base : java.io.File) extends Storage  {
   def localBase = URI(scheme + "://" + authority + prefix) 
   def get(path : Path)(implicit cont: (URI,NodeSeq) => Unit) {
      val uri = path.doc.uri
      val target = new java.io.File(base, Storage.getSuffix(localBase,uri))
      val N = if (target.isFile) utils.xml.readFile(target)
        else if (target.isDirectory) {
          val entries = target.list().toList.filter(x => x != ".svn" && x != ".omdoc") //TODO: should be an exclude pattern
          val relativePrefix = if (uri.path.isEmpty) "" else uri.path.last + "/"
          Storage.virtDoc(entries, relativePrefix)
        } else throw BackendError(path)
      cont(uri, N)
   }
}

/** a Storage that retrieves repository URIs over SVN connection */
case class SVNRepo(scheme : String, authority : String, prefix : String, repository : SVNRepository, defaultRev : Int = -1) extends Storage  {
   def localBase = URI(scheme + "://" + authority + prefix) 
   
   def get(path : Path)(implicit cont: (URI,NodeSeq) => Unit) {get(path, defaultRev)}
   def get(path : Path, rev: Int)(implicit cont: (URI,NodeSeq) => Unit) {
      val uri = path.doc.uri
      val target = Storage.getSuffix(localBase, uri)
      
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
          Storage.virtDoc(strEntries.reverse.map(x => x.getURL.getPath), prefix) //TODO check if path is correct
        case SVNNodeKind.NONE => throw BackendError(path)
      }
      cont(uri, N)
   }
}

/** a Backend holds a list of Storages and uses them to dereference Paths */
class Backend(extman: ExtensionManager, val report : info.kwarc.mmt.api.frontend.Report) extends Logger {
   private var stores : List[Storage] = Nil
   val logPrefix = "backend"
   def addStore(s : Storage*) {
      stores = stores ::: s.toList
      s.foreach {d =>
         log("adding storage " + d.toString)
      }
   }
   def removeStore(s: Storage) {
      stores = stores.filter(_ != s)
      log("removing storage " + s.toString)
   }
   
   //this must be redesigned
   def copyStorages(newRev : Int = -1) : List[Storage] = {
     stores.map(s => s match {
       case SVNRepo(sch, auth, pref, repo, rev) => new SVNRepo(sch,auth,pref, repo, newRev)
       case _ => s
     })
   }

  
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

           case _ => throw NotApplicable
         }
       case SVNNodeKind.FILE => throw NotApplicable //TODO
       case _ => throw NotApplicable
     }
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
   def closeArchive(id: String) {
      getArchive(id) foreach {arch =>
         removeStore(arch)
         removeStore(arch.narrationBackend)
      }
   }
   
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
   /** look up a path in the first Storage that is applicable and send the content to the reader */
   def get(p : Path)(implicit cont: (URI,NodeSeq) => Unit) = {
      def getInList(l : List[Storage], p : Path) {l match {
         case Nil => throw BackendError(p)
         case hd :: tl =>
            log("trying " + hd)
      	    try {hd.get(p)}
            catch {case NotApplicable =>
               log(hd.toString + " not applicable to " + p)
               getInList(tl, p)
            }
      }}
      getInList(stores, p)
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
   /** retrieves all Stores */
   def getStores : List[Storage] = stores
   
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
   /** closes all svn sessions */
   def cleanup = {
     stores.map(x => x match {
       case SVNRepo(scheme,authority,prefix,repo, rev) => repo.closeSession()
       case _ => None
     })
   }
}