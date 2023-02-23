package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api.archives.{Archive, BuildManager, RedirectableDimension, TrivialBuildManager}
import info.kwarc.mmt.api.frontend.{Controller, Report, Run}
import info.kwarc.mmt.api.utils.time.Time
import info.kwarc.mmt.api.utils.{File, MMTSystem, URI}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.lsp.{LSP, LSPClient, LSPServer, LSPWebsocket, LocalStyle, RunStyle, SandboxedWebSocket, TextDocumentServer, WithAnnotations, WithAutocomplete}
import info.kwarc.mmt.stex.parsing.stex.STeXParser
import info.kwarc.mmt.stex.{RusTeX, STeXServer}
import info.kwarc.mmt.stex.xhtml.SemanticState
import org.eclipse.lsp4j.{InitializeParams, InitializeResult, InitializedParams, WorkspaceFoldersOptions, WorkspaceServerCapabilities, WorkspaceSymbol, WorkspaceSymbolParams}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest, JsonSegment}

import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.concurrent.{Await, Future}

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

class LSPSearchResult {
  var archive : String = null
  var sourcefile : String = null
  var local : Boolean = false
  var html : String = null
  var fileuri : String = null
}

class LSPSearchResults {
  var locals: java.util.List[LSPSearchResult] = java.util.List.of()
  var remotes: java.util.List[LSPSearchResult] = java.util.List.of()
}

class SearchParams {
  var query : String = null
  var defis = true
  var asserts = true
  var exs = true
}

class BuildMessage {
  var file:String = null
}


@JsonSegment("stex")
trait STeXClient extends LSPClient {
  @JsonRequest def updateHTML(msg: HTMLUpdateMessage): CompletableFuture[Unit]
  @JsonRequest def openFile(msg:HTMLUpdateMessage): CompletableFuture[Unit]
  @JsonRequest def ping(): CompletableFuture[String]
  @JsonNotification def updateMathHub(): Unit
}
final class STeXLSPWebSocket extends SandboxedWebSocket(classOf[STeXClient],classOf[STeXLSPServer]) {
  override def initialize: Unit = {
    val controller = new Controller()
    List("lsp"
      , "lsp-stex"
      , "lsp-stex-server-methodcall"
      , "lsp-stex-websocket"
      , "lsp-stex-server"
      , "fullstex").foreach(s => controller.handleLine("log+ " + s))
    controller.handleLine("log console")
    controller.handleLine("server on 8090")
    val lsp = new STeXLSP
    controller.extman.addExtension(lsp)
    controller.extman.get(classOf[BuildManager]).foreach(controller.extman.removeExtension)
    controller.extman.addExtension(new TrivialBuildManager)
    this._lsp = Some(lsp.asInstanceOf[LSP[STeXClient,STeXLSPServer,this.type]])
  }
}
class STeXLSP extends LSP(classOf[STeXClient],classOf[STeXLSPServer],classOf[STeXLSPWebSocket])("stex",5007,5008){
  override def newServer(style: RunStyle): STeXLSPServer = new STeXLSPServer(style)
}

class STeXLSPServer(style:RunStyle) extends LSPServer(classOf[STeXClient]) with MathHubServer
  with TextDocumentServer[STeXClient,sTeXDocument]
  with WithAutocomplete[STeXClient]
  with WithAnnotations[STeXClient,sTeXDocument]
 {
   override def presenter = stexserver.presenter
   override def newDocument(uri: String): sTeXDocument = {
     //val mf = client.client.getMainFile
     //val file = mf.join().mainFile
     //println("Here: " + file)
     new sTeXDocument(uri,this.client,this)
   }
   var mathhub_top : Option[File] = None
   lazy val parser = new STeXParser(controller)
   var localServer : URI = null

   override def completion(doc: String, line: Int, char: Int): List[Completion] = Nil

   override val scopes: List[String] = List("stex-module","stex-symdecl","stex-constant","stex-variable")
   override val modifiers: List[String] = List("deprecated")

   override def shutdown: Any = style match {
     case LocalStyle => scala.sys.exit()
     case _ =>
   }
   override def exit: Unit = shutdown

   lazy val stexserver = controller.extman.get(classOf[STeXServer]) match {
     case Nil =>
       this.synchronized {
         val ss = new STeXServer
         controller.extman.addExtension(ss)
         ss
       }
     case a :: _ =>
       a
   }

   @JsonNotification("sTeX/buildFile")
   def buildFile(a :BuildMessage) : Unit = {
     a.file = LSPServer.VSCodeToURI(a.file)
     val d = documents.synchronized{documents.getOrElseUpdate(a.file,newDocument(a.file))}
     d.buildFull()
   }
   private def do_manifest(s : String) =
     s"""
        |id:$s
        |ns:http://mathhub.info/$s
        |narration-base:http://mathhub.info/$s
        |url-base:http://mathhub.info/$s
        |format:stex
        |""".stripMargin

   private val default_stex =
     """
       |\documentclass{stex}
       |\libinput{preamble}
       |\begin{document}
       |% A first sTeX document
       |\end{document}
       |""".stripMargin

   private val gitignore = """*.sref
                             |*.pdf
                             |*.hd
                             |*.mw
                             |*.tst
                             |*.upa
                             |*.upb
                             |*.fls
                             |*.rel
                             |*.sms
                             |*.bbl
                             |*.blg
                             |*.out
                             |*.synctex.gz
                             |*.run.xml
                             |*.thm
                             |*.bak
                             |*.idx
                             |*.ind
                             |*.ilg
                             |*.log
                             |*.toc
                             |*.aux
                             |auto
                             |*.nav
                             |*.snmv
                             |*.vrb
                             |*.tmp
                             |*.glo
                             |*.gls
                             |*.deps
                             |*-blx.bib
                             |*.bcf
                             |_region_.tex
                             |.DS_Store
                             |*~
                             |*.fdb_latexmk""".stripMargin

   @JsonNotification("sTeX/initializeArchive")
   def initializeArchive(a : ArchiveMessage): Unit = safely {
     mathhub_top.foreach {mh =>
       val ndir = mh / a.archive
       (ndir / "META-INF").mkdirs()
       File.write(ndir / "META-INF" / "MANIFEST.MF",do_manifest(a.archive))
       (ndir / "lib").mkdirs()
       File.write(ndir / "lib" / "preamble.en.tex",s"% preamble code for ${a.archive}")
       (ndir / "source").mkdirs()
       File.write(ndir / "source" / "helloworld.tex", default_stex)
       File.write(ndir / ".gitignore",gitignore)
       controller.handleLine("mathpath archive " + ndir.toString)
       client.client.updateMathHub()
       val um = new HTMLUpdateMessage
       um.html = (ndir / "source" / "helloworld.tex").toString
       client.client.openFile(um)
     }
   }

   @JsonRequest("sTeX/search")
   def search(p : SearchParams) : CompletableFuture[LSPSearchResults] = Completable {
     val tps = if (p.defis) List("definition") else if (p.exs) List("example") else if (p.asserts) List("assertion") else Nil
     val (local,remote) = searchI(p.query,tps)
     val ret = new LSPSearchResults
     ret.locals = local
     ret.remotes = remote
     ret
   }

   @JsonNotification("sTeX/parseWorkspace")
   def parseWorkspace() : Unit = withProgress(this,"Quickparsing Workspace"){ update =>
     var allfiles : List[File] = Nil
     allfiles = workspacefolders.flatMap(f => if (f.exists()) f.descendants.filter(fi => fi.isFile && fi.getExtension.contains("tex")) else Nil)
       allfiles.zipWithIndex.foreach {
         case (f, i) => //((a, f), i) =>
           update(i.toFloat / allfiles.length.toFloat, "Parsing " + (i + 1) + "/" + allfiles.length + ": " + f.toString)
           //if (!parser.dict.previouslyread(f)) parser.apply(f, Some(a))
           val uri = if (f.toString.charAt(1) == ':') "file://" + f.toString.head.toLower + f.toString.drop(1) else "file://" + f.toString
           var needsdoing = false
           val d = documents.getOrElseUpdate(uri, {
             needsdoing = true
             newDocument(uri)
           })
           if (needsdoing) d.synchronized {
             d.archive match {
               case Some(a) =>
                 val reg = a.properties.get("ignore").map(_.replace(".","\\.").replace("*",".*").r)
                 def regfilter(f : File) : Boolean = !reg.exists(_.matches("/" + (a / info.kwarc.mmt.api.archives.source).relativize(f).toString))
                 if (regfilter(f)) d.init(File.read(f))
               case _ =>
                 d.init(File.read(f))
             }
           }
       }
     ((),"Done")
   }

   @JsonNotification("sTeX/setMathHub")
   def setMathHub(msg:MathHubMessage) : Unit = {
     bootToken.foreach {tk =>
       updateProgress(tk,0.5,"Setting MathHub")
     }
     val mh = File(msg.mathhub)
     this.mathhub_top = Some(mh)
     this.remoteServer = msg.remote
     controller.handleLine("mathpath archive " + mh.toString)
     controller.handleLine("lmh root " + mh.toString)
     Future {
       withProgress(RusTeX,"Initializing RusTeX"){tk =>
         RusTeX.initializeBridge(mh / ".rustex")
         ((),"Done.")
       }
     }(scala.concurrent.ExecutionContext.global)
     bootToken.foreach {tk =>
       updateProgress(tk,0.5,"Loading relational information")
     }
     stexserver
     bootToken.foreach {tk =>
       updateProgress(tk,0.5,"Checking for necessery archives")
     }
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
     parser.init()
     bootToken.foreach {tk =>
       updateProgress(tk,0,"Indexing tex files")
     }
     //}(scala.concurrent.ExecutionContext.global)
     //println(t._1)
     /*
     controller.backend.getArchives.foreach{
       case a if a.properties.get("format").contains("stex") =>
         (a / info.kwarc.mmt.api.archives.source).descendants.foreach {
           case f if f.getExtension.contains("tex") && !parser.dict.previouslyread(f) =>
             parser.applyFormally(f,Some(a))
           case _ =>
         }
       case _ =>
     }
      */
   }

   private var bootToken : Option[Int] = None
   def doPing: Unit = Future {
     Thread.sleep(30000)
     while (true) {
       Thread.sleep(1000)
       try {
         client.client.ping().get(30, TimeUnit.SECONDS)
       } catch {
         case _: java.util.concurrent.TimeoutException =>
           sys.exit()
       }
     }
   }(scala.concurrent.ExecutionContext.global)

   override def initialized(params: InitializedParams): Unit = {
     super.initialized(params)
     bootToken = Some(params.hashCode())
     controller.extman.addExtension(SearchResultServer)
     startProgress(bootToken.get,"Starting sTeX/MMT","Initializing...")
     //doPing
   }

   @JsonRequest("sTeX/getMathHubContent")
   def getMathHubContent() : CompletableFuture[java.util.List[MathHubEntry]] = Completable {
     bootToken.foreach {tk =>
       updateProgress(tk,0.5,"Querying remote MathHub")
     }
     val ret = getMathHubContentI()
     bootToken.foreach {tk =>
       finishProgress(tk,"Finished")
     }
     ret
   }

   private var workspacefolders : List[File] = Nil

   override def initialize(params: InitializeParams, result: InitializeResult): Unit = {
     import scala.jdk.CollectionConverters._
     super.initialize(params, result)
     val wfo = new WorkspaceFoldersOptions
     wfo.setSupported(true)
     wfo.setChangeNotifications(true)
     val wsc = new WorkspaceServerCapabilities(wfo)
     result.getCapabilities.setWorkspace(wsc)
     result.getCapabilities.setWorkspaceSymbolProvider(true)
     if (params.getWorkspaceFolders != null) params.getWorkspaceFolders.asScala.foreach {f =>
       val file = File({
         val str = f.getUri.drop(7)
         if (str.length > 2 && str(2) == ':') {
           str.take(2).toUpperCase + str.drop(2)
         } else str
       })
       if (file.exists()) workspacefolders ::= file else None
     }
   }

   @JsonNotification("sTeX/installArchive")
   def installArchive(arch: ArchiveMessage) : Unit = installArchives(arch.archive)

   @JsonNotification("sTeX/buildHTML")
   def buildHTML(a:BuildMessage): Unit = safely {
     a.file = LSPServer.VSCodeToURI(a.file)
     val d = documents.synchronized {
       documents.getOrElseUpdate(a.file, newDocument(a.file))
     }
     d.buildHTML()
   }

   override def workspaceSymbol(params: WorkspaceSymbolParams): List[WorkspaceSymbol] = {
     print("")
     super.workspaceSymbol(params)
   }

   override def connect: Unit = {
     controller.extman.addExtension(htmlserver.get)
     localServer = controller.server.get.baseURI
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

   val self = this

   override lazy val htmlserver = Some(new ServerExtension("stexlspdocumentserver") {
     override def apply(request: ServerRequest): ServerResponse = request.path.lastOption match {
       case Some("lsperror") =>
         var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
         html = html.replace("CONTENT_URL_PLACEHOLDER", (localServer / (":" + this.pathPrefix) / "geterror").toString + "?" + request.query)
         html = html.replace("BASE_URL_PLACEHOLDER", "")
         html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?None")
         ServerResponse(html, "text/html")
       case Some("geterror") =>
         val ue = client.diags(client.diags.length - (1 + request.query.toInt))._2.get
         val ret = self.presenter.doHistories(ue.cp,ue.histories.reverse :_*)
         ServerResponse("<body>" + ret + "</body>","text/html")
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
                   case Some(html) =>
                     ServerResponse(html.toString,"text/html")
                   case None =>
                     ServerResponse("Document not yet built","txt")
                 } }
             }
         }
       case Some("fulldocument") =>
         request.query match {
           case "" =>
             ServerResponse("Empty Document path", "txt")
           case s =>
             self.documents.get(s) match {
               case None =>
                 ServerResponse("Empty Document path", "txt")
               case Some(d) =>
                 var html = MMTSystem.getResourceAsString("mmt-web/stex/tabs.html")
                 html = html.replace("%%HTMLSOURCE%%", (localServer / (":" + this.pathPrefix) / "fullhtml").toString + "?" + s)
                 html = html.replace("%%OMDOCSOURCE%%", localServer.toString + "/:" + stexserver.pathPrefix + "/omdoc?archive=" + d.archive.map(_.id).getOrElse("NONE") + "&filepath=" + d.relfile.map(_.setExtension("omdoc")).getOrElse("NONE"))
                 html = html.replace("%%PDFSOURCE%%", localServer.toString + "/:" + stexserver.pathPrefix + "None")
                 ServerResponse(html, "text/html")
             }
         }
       case Some("fullhtml") =>
         request.query match {
           case "" =>
             ServerResponse("Empty Document path","txt")
           case s =>
             var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
             html = html.replace("CONTENT_URL_PLACEHOLDER",(localServer / (":" + this.pathPrefix) / "document").toString + "?" + s )
             html = html.replace("BASE_URL_PLACEHOLDER","")
             html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?" + s)
             ServerResponse(html, "text/html")
         }
       case Some("css") =>
         request.query match {
           case "" =>
             ServerResponse("Empty Document path", "txt")
           case s =>
             self.documents.get(s) match {
               case None =>
                 ServerResponse("Empty Document path", "txt")
               case Some(d) =>
                 val ret = d.archive match {
                   case None => ""
                   case Some(a) =>
                     stexserver.css(a.id)
                 }
                 ServerResponse(ret, "text/css")
             }
         }
       case _ =>
         ServerResponse("Unknown key","txt")
     }
   })
}

object SemanticHighlighting {
  val module = 0
  val declaration = 1
  val symbol = 2
  val variable = 3
}

object Socket {
  def main(args : Array[String]) : Unit = {
    val lsp = new STeXLSP {
      override def newServer(style: RunStyle): STeXLSPServer = new STeXLSPServer(style) {
        override def doPing: Unit = {}
      }
    }
    val controller = Run.controller
    List("lsp"
      , "lsp-stex"
      , "lsp-stex-server-methodcall"
      , "lsp-stex-socket"
      , "lsp-stex-server"
      , "fullstex").foreach(s => controller.handleLine("log+ " + s))
    controller.handleLine("log console")
    controller.handleLine("server on 8090")
    controller.extman.addExtension(lsp)
    controller.extman.get(classOf[BuildManager]).foreach(controller.extman.removeExtension)
    controller.extman.addExtension(new TrivialBuildManager)
    lsp.runSocketListener
  }
}

object WebSocket {
  def main(args : Array[String]) : Unit = {
    SandboxedWebSocket.runWebSocketListener[STeXClient,STeXLSPServer,STeXLSPWebSocket](classOf[STeXLSPWebSocket],5008)
    /*
    val lsp = new STeXLSP
    val controller = Run.controller
    List("lsp"
      , "lsp-stex"
      , "lsp-stex-server-methodcall"
      , "lsp-stex-websocket"
      , "lsp-stex-server"
      , "fullstex").foreach(s => controller.handleLine("log+ " + s))
    controller.handleLine("log console")
    controller.handleLine("server on 8090")
    controller.extman.addExtension(lsp)
    controller.extman.get(classOf[BuildManager]).foreach(controller.extman.removeExtension)
    controller.extman.addExtension(new TrivialBuildManager)
    lsp.runWebSocketListener
     */
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
    controller.extman.get(classOf[BuildManager]).foreach(controller.extman.removeExtension)
    controller.extman.addExtension(new TrivialBuildManager)
    controller.extman.addExtension(end)
    controller.backend.openArchive(File(args.head))
    end.runLocal
  }
}