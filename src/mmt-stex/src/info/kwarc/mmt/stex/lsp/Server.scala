package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.lsp.{LSP, LSPClient, LSPServer, LSPWebsocket, LocalStyle, RunStyle, TextDocumentServer, WithAnnotations, WithAutocomplete}
import org.eclipse.lsp4j.jsonrpc.services.{JsonRequest, JsonSegment}

import java.util.concurrent.CompletableFuture

class MainFileMessage {
  var mainFile: String = null
}

class HTMLUpdateMessage {
  var html: String = null
}

@JsonSegment("stex")
trait STeXClient extends LSPClient {
  @JsonRequest def getMainFile: CompletableFuture[MainFileMessage]
  @JsonRequest def updateHTML(msg: HTMLUpdateMessage): CompletableFuture[Unit]
}
class STeXLSPWebSocket extends LSPWebsocket(classOf[STeXClient],classOf[STeXLSPServer])
class STeXLSP extends LSP(classOf[STeXClient],classOf[STeXLSPServer],classOf[STeXLSPWebSocket])("stex",5007,5008) {
  override def newServer(style: RunStyle): STeXLSPServer = new STeXLSPServer(style)
}

class STeXLSPServer(style:RunStyle) extends LSPServer(classOf[STeXClient])
  with TextDocumentServer[STeXClient,sTeXDocument]
  with WithAutocomplete[STeXClient]
  with WithAnnotations[STeXClient,sTeXDocument]
 {
   override def newDocument(uri: String): sTeXDocument = {
     val mf = client.client.getMainFile
     val file = mf.join().mainFile
     println("Here: " + file)
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

   override def connect: Unit = client.log("Connected to sTeX!")

   override def didChangeConfiguration(params: List[(String, List[(String, String)])]): Unit = {
     params.collect {case (a,ls) if a == "stexide" =>
       ls.collect {case ("mathhub",v) if v.nonEmpty && File(v).exists() =>
         this.mathhub_top = Some(File(v))
       }
     }
   }

   override def didSave(docuri: String): Unit = this.documents.get(docuri) match {
     case Some(document) => document.build()
     case _ =>
   }

}