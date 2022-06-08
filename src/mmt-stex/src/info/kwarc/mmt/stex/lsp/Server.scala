package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api.frontend.{Controller, Run}
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.lsp.{LSP, LSPClient, LSPServer, LSPWebsocket, LocalStyle, RunStyle, TextDocumentServer, WithAnnotations, WithAutocomplete}
import info.kwarc.mmt.stex.{RusTeX, STeXServer}
import info.kwarc.mmt.stex.xhtml.SemanticState
import org.eclipse.lsp4j.{InitializeParams, InitializeResult, InitializedParams}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest, JsonSegment}

import java.util.concurrent.CompletableFuture

class MainFileMessage {
  var mainFile: String = null
}

class HTMLUpdateMessage {
  var html: String = null
}

class MathHubMessage {
  var mathhub : String = null
  var remote: String = null
}

class ArchiveMessage {
  var archive : String = null
}


@JsonSegment("stex")
trait STeXClient extends LSPClient {
  @JsonRequest def updateHTML(msg: HTMLUpdateMessage): CompletableFuture[Unit]
  @JsonNotification def updateMathHub(): Unit
}
class STeXLSPWebSocket extends LSPWebsocket(classOf[STeXClient],classOf[STeXLSPServer])
class STeXLSP extends LSP(classOf[STeXClient],classOf[STeXLSPServer],classOf[STeXLSPWebSocket])("stex",5007,5008){
  override def newServer(style: RunStyle): STeXLSPServer = new STeXLSPServer(style)
}

class STeXLSPServer(style:RunStyle) extends LSPServer(classOf[STeXClient]) with MathHubServer
  with TextDocumentServer[STeXClient,sTeXDocument]
  with WithAutocomplete[STeXClient]
  with WithAnnotations[STeXClient,sTeXDocument]
 {
   override def newDocument(uri: String): sTeXDocument = {
     //val mf = client.client.getMainFile
     //val file = mf.join().mainFile
     //println("Here: " + file)
     new sTeXDocument(uri,this.client,this)
   }
   var mathhub_top : Option[File] = None

   override def completion(doc: String, line: Int, char: Int): List[Completion] = Nil

   override val scopes: List[String] = Nil
   override val modifiers: List[String] = Nil

   override def shutdown: Any = style match {
     case LocalStyle => scala.sys.exit()
     case _ =>
   }
   override def exit: Unit = shutdown

   lazy val stexserver = controller.extman.get(classOf[STeXServer]) match {
     case Nil =>
       this.synchronized {
         client.log("starting sTeX server...")
         val ss = new STeXServer
         controller.extman.addExtension(ss)
         client.log("done.")
         ss
       }
     case a :: _ =>
       a
   }

   @JsonNotification("sTeX/setMathHub")
   def setMathHub(msg:MathHubMessage) : Unit = {
     val mh = File(msg.mathhub)
     this.mathhub_top = Some(mh)
     this.remoteServer = msg.remote
     controller.handleLine("mathpath archive " + mh.toString)
     controller.handleLine("lmh root " + mh.toString)
     RusTeX.initializeBridge(mh / ".rustex")
     stexserver
     controller.backend.getArchive("MMT/urtheories") match {
       case None =>
         installArchives("MMT/urtheories")
       case _ =>
     }
     controller.backend.getArchive("sTeX/meta-inf") match {
       case None =>
         installArchives("sTeX/meta-inf")
       case _ =>
     }
   }

   @JsonRequest("sTeX/getMathHubContent")
   def getMathHubContent() : CompletableFuture[java.util.List[MathHubEntry]] = Completable {
     getMathHubContentI()
   }

   @JsonNotification("sTeX/installArchive")
   def installArchive(arch: ArchiveMessage) : Unit = installArchives(arch.archive)

   override def connect: Unit = {
     controller.extman.addExtension(lspdocumentserver)
     client.log("Connected to sTeX!")
   }

   override def didChangeConfiguration(params: List[(String, List[(String, String)])]): Unit = {
     params.collect {case (a,ls) if a == "stexide" =>
       ls.collect {case ("mathhub",v) if v.nonEmpty && File(v).exists() =>
         RusTeX.initializeBridge(File(v) / ".rustex")
         this.mathhub_top = Some(File(v))
       }
     }
   }

   override def didSave(docuri: String): Unit = this.documents.get(docuri) match {
     case Some(document) => document.build()
     case _ =>
   }

   val self = this

   lazy val lspdocumentserver = new ServerExtension("stexlspdocumentserver") {
     override def apply(request: ServerRequest): ServerResponse = request.path.lastOption match {
       case Some("document") =>
         request.query match {
           case "" =>
             ServerResponse("Empty Document path","txt")
           case s =>
             self.documents.get(s) match {
               case None =>
                 ServerResponse("Empty Document path","txt")
               case Some(d) =>
                 /*d.synchronized */ { d.html match {
                   case Some(html) => ServerResponse(html.toString,"application/xhtml+xml")
                   case None =>
                     ServerResponse("Document not yet built","txt")
                 } }
             }
         }
       case _ =>
         ServerResponse("Unknown key","txt")
     }
   }

}

object Main {

  @throws[InterruptedException]
  //@throws[ExecutionException]
  def main(args: Array[String]): Unit = {
    val controller = Run.controller
    /*controller.handleLine("log html /home/jazzpirate/mmtlog.html")
    controller.handleLine("log+ lsp")
    controller.handleLine("log+ lsp-stex")
    controller.handleLine("log+ lsp-stex-server")
    controller.handleLine("log+ lsp-stex-server-methodcall")*/
    val mathhub_dir = File(args.head)
    val port = args(1)
    if (mathhub_dir.exists()) {
      controller.handleLine("mathpath archive " + mathhub_dir.toString)
      controller.handleLine("lmh root " + mathhub_dir.toString)
    }
    controller.handleLine("server on " + port)
    val end = new STeXLSP
    controller.extman.addExtension(end)
    controller.backend.openArchive(File(args.head))
    end.runLocal
  }
}