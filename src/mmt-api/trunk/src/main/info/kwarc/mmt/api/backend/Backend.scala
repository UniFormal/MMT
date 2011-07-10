package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import scala.xml._
import info.kwarc.mmt.api.utils.MyList.fromList

import utils.File
import utils.FileConversion._

// local XML databases or query engines to access local XML files: baseX or Saxon

case object NotApplicable extends java.lang.Throwable
case class NotFound(p : Path) extends info.kwarc.mmt.api.Error("Cannot find ressource " + p.toString)

/** Storage is an abstraction over backends that can provide MMT content
 * A storage declares a URI u and must answer to all URIs that start with u.
 */
abstract class Storage {
   /** called automatically when added; a storage may send arbitrary content to the reader during intialization */
   def init(reader : Reader) {}
   /** dereferences a path and sends the content to a reader
    * a storage may send more content than the path references, e.g., the whole file or a dependency closure
    */
   def get(path : Path, reader : Reader)
}

/** convenience methods for backends */
object Storage {
   /** reads a locutor registry file and returns a list of Storages */
   def fromLocutorRegistry(file : java.io.File) : List[LocalCopy] = {
      val N = utils.xml.readFile(file)
      val l = for (R <- (N\\"registry"\\"repository").toList) yield {
   	     val repos = URI(xml.attr(R, "location"))
         for {wc <- R.child.toList if wc.label == "wc"} yield {
            val path = xml.attr(wc,"location")
            val localDir = new java.io.File(xml.attr(wc, "root"))
            LocalCopy(repos.scheme.getOrElse(null), repos.authority.getOrElse(null), repos.pathAsString + path, localDir)
         }
      }
	  l.flatten    
   }
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
         case <assertions/> => Some(Ass(p, q))
         case scala.xml.Comment(_) => None
         case _ => throw ParseError("illegal query type: " + n)
      }
   }
   def replace(s : String, p : Path) : String = {
       val s1 = s.replace("%doc%", p.doc.toPath).replace("%path%", p.doc.uri.pathAsString).replace("%full%", p.toPath)
       p match {
          case p: DPath => s1 
          case p: MPath => s1.replace("%mod%", p.name.flat)
          case p: GlobalName => s1.replace("%mod%", p.parent.toMPath.name.flat).replace("%name%", p.name.flat)
       }
   }
}
sealed abstract class OMQuery(val path : String, val query : String)
case class Doc(base : String, p : String, q : String) extends OMQuery(p, q)
case class Mod(p : String, q : String) extends OMQuery(p, q)
case class Ass(p : String, q : String) extends OMQuery(p, q)

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
     override def init(reader : Reader) {
    	ipats.foreach {q => 
           val N = handleResponse("path " + q.path + " and body " + q.query, sendRequest(q.path,q.query))
           q match {
              case Doc(b, _, _) =>
                 val base = DPath(URI(b))
                 reader.readDocuments(base, N)
              case Mod(_, _) => reader.readModules(mmt.mmtbase, None, N)
              case Ass(_, _) => reader.readAssertions(N)
           }
    	}
     }
     def get(path : Path, reader : Reader) {
        val qs = path match {
           case p : DPath => dpats
           case p : MPath => mpats
           case p : GlobalName => spats
        }
        qs.foreach {q =>
           val qpath = OMQuery.replace(q.path, path)
           val qbody = OMQuery.replace(q.query, path)
           val N = handleResponse("path " + qpath + " and body " + qbody, sendRequest(qpath,qbody))
           q match {
              case Doc(b, _, _) =>
                 val base = DPath(URI(OMQuery.replace(b, path)))
                 reader.readDocuments(base, N)
              case Mod(_, _) => reader.readModules(path.doc, None, N)
              case Ass(_, _) => reader.readAssertions(N)
           }
        }
     }
}

/** a Storage that retrieves file URIs from the local system */
case class LocalSystem(base : URI) extends Storage {
   val localBase = URI(Some("file"), None, Nil, true, None, None)
   def get(path : Path, reader : Reader) {
      val uri = base.resolve(path.doc.uri)
      val test = Storage.getSuffix(localBase, uri)
      val file = new java.io.File(uri.toJava)
      val N = utils.xml.readFile(file)
      reader.readDocuments(DPath(uri), N)
   }
}

/** a Storage that retrieves repository URIs from the local working copy */
case class LocalCopy(scheme : String, authority : String, prefix : String, base : java.io.File) extends Storage  {
   def localBase = URI(scheme + "://" + authority + prefix) 
   def get(path : Path, reader : Reader) {
      val uri = path.doc.uri
      val target = new java.io.File(base, Storage.getSuffix(localBase,uri))
      val N = if (target.isFile) utils.xml.readFile(target)
        else if (target.isDirectory) {
          val entries = target.list().toList.filter(_ != ".svn") //TODO: should be an exclude pattern
          val prefix = if (target != base) target.getName + "/" else ""
          Storage.virtDoc(entries, prefix)
      } else throw NotFound(path)
      reader.readDocuments(DPath(uri), N)
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
class Backend(reader : Reader, report : info.kwarc.mmt.api.frontend.Report) {
   private var stores : List[Storage] = Nil
   private var compilers : List[Compiler] = Nil
   private def log(msg : String) = report("backend", msg)
   def addStore(s : Storage*) {
      stores = stores ::: s.toList
      s.foreach {d =>
         log("adding storage " + d.toString)
         d.init(reader)
      }
   }
   def addCompiler(c: Compiler) {
       log("adding compiler " + c.toString)
       compilers ::= c
       c.init
   }
   def openArchive(root: java.io.File) : Archive = {
      //TODO: check if "file" is mar, folder, or meta-inf file, branch accordingly
      val properties = new scala.collection.mutable.ListMap[String,String]
      var compiler : Option[Compiler] = None 
      val manifest = root / "META-INF" / "MANIFEST.MF"
      if (manifest.isFile) {
         // read "key: value" list from "manifest" into properties
         val in = new java.io.BufferedReader(new java.io.FileReader(manifest))
         var line : String = null
         while ({line = in.readLine(); line != null}) {
            val p = line.indexOf(":")
            val key = line.substring(0,p).trim
            val value = line.substring(p+1).trim
            properties(key) = value
         }
         in.close
         properties.get("source") foreach (
            src => compilers.find(_.isApplicable(src)) match {
               case Some(c) => compiler = Some(c)
               case None => log("no compiler registered for source " + src)
            }
         )
      }
      val arch = new Archive(root, properties, compiler, report)
      compiler foreach {_.register(arch)}
      addStore(arch)
      arch
   }
   def cleanup {
      compilers.foreach(_.destroy)
      compilers = Nil
   } 
   /** look up a path in the first Storage that is applicable and send the content to the reader */
   def get(p : Path) = {
      def getInList(l : List[Storage], p : Path) {l match {
         case Nil => throw NotFound(p)
         case hd :: tl =>
            log("trying " + hd)
      	    try {hd.get(p, reader)}
            catch {case NotApplicable =>
               log(hd.toString + " not applicable to " + p)
               getInList(tl, p)
            }
      }}
      getInList(stores, p)
   }
   /** retrieve an Archive by its id */
   def getArchive(id: String) = stores find {
      case a: Archive => a.properties.get("id") == Some(id)
      case _ => false
   }
}