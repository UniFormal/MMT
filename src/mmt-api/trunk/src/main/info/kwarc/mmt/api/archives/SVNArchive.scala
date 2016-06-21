package info.kwarc.mmt.api.archives

/*
 * From Controller.scala
 	      case AddMathPathSVN(uri, rev, user, pass) =>
	         val repos = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(uri.toString))
            user foreach {u =>
              val authManager = SVNWCUtil.createDefaultAuthenticationManager(u, pass.getOrElse(""))
              repos.setAuthenticationManager(authManager)
            }
            val s = SVNRepo(uri.schemeNull, uri.authorityNull, uri.pathAsString, repos, rev)
            backend.addStore(s)
            
           case AddSVNArchive(url, rev) =>
           backend.openArchive(url, rev)
 */

/*
 * From Backend.scala
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

 */


/*
import info.kwarc.mmt.api._
import frontend._
import backend._
import utils._
import utils.FileConversion._

import java.io.{FileInputStream, FileOutputStream}
import java.util.zip._

import org.tmatesoft.svn.core._
import org.tmatesoft.svn.core.io._
import org.tmatesoft.svn.core.auth._
import org.tmatesoft.svn.core.wc.SVNWCUtil

import scala.collection.mutable._

class SVNArchive(repository : SVNRepository, val properties : Map[String, String], val report : Report, defRev : Int = -1) extends ROArchive {
  val location = URI(repository.getLocation.toString)
  val narrationBase = utils.URI(properties.getOrElse("narration-base", ""))
  
  val rootString = repository.getLocation.toString
  
  private val scheme = URI(location).schemeNull
  private val authority = URI(location).authorityNull
  private val prefix = URI(location).pathAsString

  val narrationBackend = new SVNRepo(scheme, authority, prefix, repository, defRev)

  val contentDir =  URI("content")
  def MMTPathToContentPath(m: MPath) : URI = contentDir / Archive.MMTPathToContentPath(m)

  /**
   *  generates a SVN archive set at a different revision
   *  \\TODO it assumes properties remain unchanged across revisions
   *  \\TODO carrying the report may not be the best idea
    */
  def generateSVNArchive(rev : Int, backend : Backend) : SVNArchive = backend.openArchive(repository.getLocation.toString, rev)

  def get(mod: MPath) : scala.xml.Node = {
    //utils.xml.readFile(MMTPathToContentPath(m)).child(0)
    val target = MMTPathToContentPath(mod)
    repository.checkPath("", defRev) match {
      case SVNNodeKind.FILE =>
        try {
          val  baos : java.io.ByteArrayOutputStream = new java.io.ByteArrayOutputStream()
          repository.getFile(target.toString, defRev, null, baos)
          scala.xml.Utility.trim(scala.xml.XML.loadString(baos.toString))
        } catch {
          case e : Exception => throw e
        }
      case SVNNodeKind.DIR => throw NotApplicable()
      case SVNNodeKind.NONE => throw NotFound(mod)
    }
  }
}

object SVNArchive {
    /**
     * Attempts to generate a new revision. Normally hasSVNArchive() should be called before
     * @param rev the revision
     * @param backend the backend
     * @return a new SVNArchive
     */
    def generate(archive: WritableArchive, backend: Backend, rev : Int = -1) : SVNArchive = {
      if (archive.properties.isDefinedAt("svnrepo")) {
        backend.openArchive(archive.properties("svnrepo"), rev)
      } else {
        throw NotApplicable()
      }
    }
}
*/