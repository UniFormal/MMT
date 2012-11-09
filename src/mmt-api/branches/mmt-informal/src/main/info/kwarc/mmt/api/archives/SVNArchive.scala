package info.kwarc.mmt.api.archives
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
      case SVNNodeKind.DIR => throw NotApplicable
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
        throw NotApplicable
      }
    }
}
