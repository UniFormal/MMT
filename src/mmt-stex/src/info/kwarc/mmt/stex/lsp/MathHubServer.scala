package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api.archives.{Archive, RedirectableDimension}
import info.kwarc.mmt.api.backend.LocalSystem
import info.kwarc.mmt.api.utils.JSONArray.toList
import info.kwarc.mmt.api.utils.{JSON, JSONArray, JSONObject, JSONString}
import org.eclipse.jgit.api.Git

import java.io.{FileOutputStream, PrintWriter, StringWriter, Writer}
import java.net.URL
import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._
import scala.util.Try

trait MathHubEntry {
  def isLocal : Boolean
}
class MathHubGroup extends MathHubEntry {
  var id: String = null
  var isLocal = false
  var children: java.util.List[MathHubEntry] = java.util.List.of()
  def getLocal : Boolean = children.asScala.exists(_.isLocal)
}
class MathHubRepo extends MathHubEntry {
  var id: String = null
  var deps = java.util.List.of[String]()
  var isLocal = false
  var localPath : String = ""
}

trait MathHubServer { this : STeXLSPServer =>

  protected var remoteServer = "https://mmt.beta.vollki.kwarc.info/:sTeX" //(controller.server.get.baseURI / ":sTeX").toString

  private sealed abstract class MHE {
    def id : String
  }
  private case class Arch(a : Archive) extends MHE {
    def id = a.id
  }
  private case class Remote(j : JSONObject) extends MHE {
    def id = j.getAsString("id")
    def deps = j.getAsList(classOf[JSONString],"deps").map(_.value)
    def gituri = j.getAsString("git")
  }

  private var allRemotes : List[Remote] = Nil

  def installArchives(ids : String) = {
    getAllRemotes
    getDeps(ids,new {var ls : List[Any] = Nil}).distinct.foreach(installArchiveI)
    client.client.updateMathHub()
  }

  private def getAllRemotes = {
    if (allRemotes.isEmpty) {
      val attempt = Try(io.Source.fromURL(remoteServer + "/allarchives")("ISO-8859-1"))
      if (attempt.isSuccess) JSON.parse(attempt.get.toBuffer.mkString) match {
        case JSONArray(vls @_*) if vls.forall(_.isInstanceOf[JSONObject]) =>
          allRemotes = vls.map(j => Remote(j.asInstanceOf[JSONObject])).toList.filterNot(j => controller.backend.getArchives.exists(_.id == j.id))
        case _ =>
      }
    }
  }

  private def getDeps(id : String,currs:{var ls : List[Any]}) : List[Remote] = allRemotes.collect{case r if (r.id == id || r.id.startsWith(id + "/")) && !currs.ls.contains(r) => r}.flatMap{r =>
    currs.ls ::= r
    r :: r.deps.flatMap(getDeps(_,currs))
  }

  private def installArchiveI(archive: Remote) = {
    (mathhub_top,controller.backend.getArchive(archive.id)) match {
      case (Some(mh),None) =>
        withProgress(archive,"Installing " + archive.id,"Cloning git repository"){update =>
          //Thread.sleep(1000)
          try {
            Git.cloneRepository().setDirectory(mh / archive.id).setURI(archive.gituri).call()
            controller.backend.openArchive(mh / archive.id)
          } catch {
            case t : Throwable =>
              client.log(t.getMessage)
              val writer = new StringWriter()
              t.printStackTrace(new PrintWriter(writer))
              writer.toString.split('\n').foreach(client.log)
              ((),"failed")
          }
          controller.backend.getArchive(archive.id) match {
            case Some(a) =>

              update(0,"Fetching File Index...")
              val attempt = Try(io.Source.fromURL(remoteServer + "/allarchfiles?" + archive.id)("ISO-8859-1"))
              if (attempt.isSuccess) JSON.parse(attempt.get.toBuffer.mkString) match {
                case JSONArray(vls @_*) =>
                  val files = vls.flatMap{j =>
                    val dim = j.asInstanceOf[JSONObject].getAsString("dim")
                    j.asInstanceOf[JSONObject].getAsList(classOf[JSONString],"files").map(js => (dim,js.value))
                  }
                  val max = files.length
                  files.zipWithIndex.foreach { case ((dim,f),i) =>
                    update(i.toDouble / max,"Downloading " + (i+1) + "/" + max + "... (" + dim + "/" + f + ")")

                    try {
                      val src = new URL(remoteServer + "/archfile?arch=" + archive.id + "&dim=" + dim + "&file=" + f).openStream()
                      val file = (a / RedirectableDimension(dim) / f)
                      if (!file.up.exists()) file.up.mkdirs()
                      file.createNewFile()
                      val target = new FileOutputStream(file.toString)
                      var c = 0
                      while ({c = src.read(); c!= -1}) {
                        target.write(c)
                      }
                      src.close()
                      target.close()
                    } catch {
                      case t : Throwable =>
                        println(t.getMessage)
                        print("")
                    }


                    //Thread.sleep(1000)
                  }
                  ((),"success")
                case _ =>
                  ((),"failed")
              } else ((),"failed")

            case _ => ((),"failed")
          }

        }
      case _ =>
    }
  }

  def getMathHubContentI() : java.util.List[MathHubEntry] = {
    getAllRemotes
    val archives = controller.backend.getArchives.filter(_.properties.get("format").map(_.toLowerCase).contains("stex")).map(Arch)
    val all = archives ::: allRemotes.filterNot(_.id == "MMT/urtheories")
    val ret = all.map(_.id.split('/').head).distinct.map{ s => makeEntries(all,List(s)) }
    ret.asJava
  }
  private def makeEntries(archives: List[MHE],path : List[String]) : MathHubEntry = {
    archives.find(_.id == path.mkString("/")) match {
      case Some(Arch(a)) =>
        val ret = new MathHubRepo
        ret.id = a.id.split('/').last
        ret.deps = a.dependencies.asJava
        ret.localPath = a.root.toString
        ret.isLocal = true
        ret
      case Some(Remote(j)) =>
        val ret = new MathHubRepo
        ret.id = j.getAsString("id").split("/").last
        ret.isLocal = false
        ret
      case _ =>
        val validarchs = archives.filter(_.id.startsWith(path.mkString("/") + "/"))
        val ret = new MathHubGroup
        ret.id = path.last
        val prefixes = validarchs.map(_.id.split('/').drop(path.length).head).distinct
        ret.children = prefixes.map(s => makeEntries(validarchs,path ::: s :: Nil)).asJava
        if (ret.getLocal) ret.isLocal = true
        ret
    }
  }


}
