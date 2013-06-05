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
   /** reads an OMBase description file and returns the described OMBases */
   def fromOMBaseCatalog(file : java.io.File) : List[OMBase] = {
      val N = utils.xml.readFile(file)
      N.child.toList mapPartial {
         case n @ <ombase>{c @ _*}</ombase> =>
            val scheme = xml.attr(n, "scheme")
            val authority = xml.attr(n, "authority")
            val prefix = xml.attr(n, "prefix")
            val ombase = URI(xml.attr(n, "ombase"))
            var dpats : List[OMQuery] = Nil
            var mpats : List[OMQuery] = null
            var spats : List[OMQuery] = null
            var ipats : List[OMQuery] = Nil
            c map {n =>
               val qs = n.child.toList.mapPartial(OMQuery.parse)
               n match {
                  case <document>{_*}</document> => dpats = qs
                  case <module>{_*}</module> => mpats = qs
                  case <symbol>{_*}</symbol> => spats = qs
                  case <init>{_*}</init> => ipats = qs
                  case scala.xml.Comment(_) => 
                  case n => throw ParseError("illegal child of ombase: " + n)
               }
            }
            if (mpats == null) mpats = dpats
            if (spats == null) spats = mpats
            Some(TNTBase(scheme, authority, prefix, ombase, dpats, mpats, spats, ipats))
         case scala.xml.Comment(_) => None
         case n => throw ParseError("illegal ombase: " + n)
      }
   }
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

object OMQuery {
   def parse(n : Node) : Option[OMQuery] = {
      val p = xml.attr(n, "path")
      val q = xml.attr(n, "body")
      n match {
         case <documents/> => Some(Doc(xml.attr(n, "base"),p, q))
         case <modules/> => Some(Mod(p, q))
         case scala.xml.Comment(_) => None
         case _ => throw ParseError("illegal query type: " + n)
      }
   }
   def replace(s : String, p : Path) : String = {
       val s1 = s.replace("%doc%", p.doc.toPath).replace("%path%", p.doc.uri.pathAsString).replace("%full%", p.toPath)
       p match {
          case p: DPath => s1 
          case p: MPath => s1.replace("%mod%", p.name.flat)
          case p: GlobalName => s1.replace("%mod%", p.module.toMPath.name.flat).replace("%name%", p.name.toPath)
          case _: CPath => s1 //TODO: check
       }
   }
}
sealed abstract class OMQuery(val path : String, val query : String)
case class Doc(base : String, p : String, q : String) extends OMQuery(p, q)
case class Mod(p : String, q : String) extends OMQuery(p, q)

/** a trait for URL-addressed Storages that can serve MMT document fragments */
abstract class OMBase(scheme : String, authority : String, prefix : String, ombase : URI,
                      val dpats : List[OMQuery], mpats : List[OMQuery], spats : List[OMQuery], ipats : List[OMQuery])
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
     def get(path : Path)(implicit cont: (URI,NodeSeq) => Unit) {
        val qs = path match {
           case p : DPath => dpats
           case p : MPath => mpats
           case p : GlobalName => spats
           case _ : CPath => Nil //TODO: check
        }
        qs.foreach {q =>
           val qpath = OMQuery.replace(q.path, path)
           val qbody = OMQuery.replace(q.query, path)
           val N = handleResponse("path " + qpath + " and body " + qbody, sendRequest(qpath,qbody))
           q match {
              case Doc(b, _, _) =>
                 val base = URI(OMQuery.replace(b, path))
                 cont(base, N)
              case Mod(_, _) => cont(path.doc.uri, N)
           }
        }
     }
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
      val suffix = Storage.getSuffix(localBase,uri)
      val target = new java.io.File(base, suffix)
      val N = if (target.isFile) utils.xml.readFile(target)
        else if (target.isDirectory) {
          val entries = target.list().toList.filter(_ != ".svn") //TODO: should be an exclude pattern
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


/** a Storage that retrieves content from a TNTBase database */
case class TNTBase(scheme : String, authority : String, prefix : String, ombase : URI,
                   dp : List[OMQuery], mp : List[OMQuery], sp : List[OMQuery], ip : List[OMQuery])
           extends OMBase(scheme, authority, prefix, ombase, dp, mp, sp, ip) {
   def handleResponse(msg : => String, N : NodeSeq) : NodeSeq = {
      N(0) match {
         case <results>{child @ _*}</results> => child map {
            case <error>{_*}</error> =>
               throw GetError("TNTBase returned error for query\n" + msg + "\n" + N)
            case <result>{r}</result> => r 
         }
         case <omdoc>{_*}</omdoc> => N(0)
         case <directory>{entries @ _*}</directory> =>
            var p = xml.attr(N(0), "path")
            if (p.endsWith("/")) p = p.substring(0,p.length - 1)
            val prefix = p.split("/").toList.last + "/" //prefix is directory name + "/" or "/" if root
            Storage.virtDoc(entries.toList.map(e => xml.attr(e, "name")), prefix)
      }
   }
}


/** a Backend holds a list of Storages and uses them to dereference Paths */
class Backend(extman: ExtensionManager, report : info.kwarc.mmt.api.frontend.Report) {
   private var stores : List[Storage] = Nil
   private def log(msg : => String) = report("backend", msg)
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
  
   /** @throws BackendError if the root file cannot be read
     * @throws NotApplicable if the root is neither a folder nor a MAR archive file */
   def openArchive(root: java.io.File) : Archive = {
      //TODO: check if "root" is meta-inf file, branch accordingly
      if (root.isDirectory) {
          val properties = new scala.collection.mutable.ListMap[String,String]
          val manifest = root / "META-INF" / "MANIFEST.MF"
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
          arch
      }
      else if (root.isFile && root.getPath.endsWith(".mar")) {    // a MAR archive file
          // unpack it
          val newRoot = root.getParentFile / (root.getName + "-unpacked")
          extractMar(root, newRoot)
          // open the archive in newRoot
          openArchive(newRoot)
      }
      else throw NotApplicable
   }
   def closeArchive(id: String) {
      getArchive(id) foreach {arch =>
         removeStore(arch)
         removeStore(arch.narrationBackend)
      }
   }
   
   private def extractMar(file: java.io.File, newRoot: java.io.File) {
       val mar = new ZipFile(file)
       var bytes =  new Array[Byte](100000)
       var len = -1
       val enum = mar.entries
       while (enum.hasMoreElements) {
           val entry = enum.nextElement
           if (entry.isDirectory)
                new java.io.File(newRoot, entry.getName).mkdirs
           else {
               val istream = mar.getInputStream(entry)
               val ostream = new java.io.FileOutputStream(new java.io.File(newRoot, entry.getName))
               while({ len = istream.read(bytes, 0, bytes.size); len != -1 })
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