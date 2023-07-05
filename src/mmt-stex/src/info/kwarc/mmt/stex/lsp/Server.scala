package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api.archives.{Archive, BuildChanged, BuildManager, RedirectableDimension, TrivialBuildManager, source}
import info.kwarc.mmt.api.frontend.{Controller, Report, Run}
import info.kwarc.mmt.api.utils.time.Time
import info.kwarc.mmt.api.utils.{File, JSON, JSONObject, JSONString, MMTSystem, URI}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.lsp.{LSP, LSPClient, LSPServer, LSPWebsocket, LocalStyle, RunStyle, SandboxedWebSocket, TextDocumentServer, WithAnnotations, WithAutocomplete}
import info.kwarc.mmt.stex.ml.Model
import info.kwarc.mmt.stex.parsing.stex.STeXParser
import info.kwarc.mmt.stex.{FullsTeX, RusTeX, STeXServer}
import info.kwarc.mmt.stex.xhtml.SemanticState
import org.eclipse.lsp4j.{InitializeParams, InitializeResult, InitializedParams, WorkspaceFoldersOptions, WorkspaceServerCapabilities, WorkspaceSymbol, WorkspaceSymbolParams}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest, JsonSegment}
import org.eclipse.lsp4j.services.LanguageClient

import java.io.IOException
import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.concurrent.{Await, Future}
import scala.util.Try

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

class LocalServerInterface {
  var url: String = null
}

class ArchiveMessage {
  var archive : String = null
}
class NewArchiveMessage {
  var archive: String = null
  var ns: String = null
  var urlbase : String = null
}

class LSPSearchResult {
  var archive : String = null
  var sourcefile : String = null
  var local : Boolean = false
  var html : String = null
  var fileuri : String = null
  var module: String = null
  var preview: String = ""
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
  var syms = true
}

class BuildMessage {
  var file:String = null
}

class BuildGroupMessage {
  var file:String = null
  var archive:String = null
}

class ExportMessage {
  var file: String = null
  var dir: String = null
}

class NERMessage {
  var jar: String = null
  var zip: String = null
}

class NEROnMessage {
  var on:Boolean = false
}

class ThresholdMessage { var threshold:Float = 0.0.toFloat }

@JsonSegment("stex")
trait STeXClient extends LSPClient {
  @JsonRequest def updateHTML(msg: HTMLUpdateMessage): CompletableFuture[Unit]
  @JsonRequest def openFile(msg:HTMLUpdateMessage): CompletableFuture[Unit]
  @JsonRequest def ping(): CompletableFuture[String]
  @JsonNotification def updateMathHub(): Unit
  @JsonNotification def setLocalServer(msg:LocalServerInterface): Unit
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
     while (bootToken.isDefined) Thread.sleep(100)
     new sTeXDocument(uri,this.client,this)
   }
   var mathhub_top : Option[File] = None
   lazy val parser = new STeXParser(controller)
   var localServer : URI = null

   override def completion(doc: String, line: Int, char: Int): List[Completion] = Nil


   override def shutdown: Any = style match {
     case LocalStyle => this.hashCode()
     case _ =>
   }
   override def exit: Unit = { sys.exit(0)}

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

   import scala.concurrent.ExecutionContext.Implicits._


   var nermodel = new {
     private var model : Option[Model] = None
     var ner_threshold = 0.4
     var on : Boolean = true
     def set(jar : File, zip : File) = {
       if (jar.exists() && zip.exists())
        model = Some(new Model(jar,zip,true))
     }
     def foreach(f : Model => Unit) = if (on) {
       model.foreach(f)
     }
   }//: Option[Model] = None
   @JsonNotification("sTeX/initializeNER")
   def initializeNER(msg: NERMessage): Unit = Future { safely {
     nermodel.set(File(msg.jar),File(msg.zip))
   }}

   @JsonNotification("sTeX/setNER")
   def setNER(msg: NEROnMessage): Unit = {
     nermodel.on = msg.on
   }

   @JsonNotification("sTeX/setThreshold")
   def setThreshold(msg: ThresholdMessage): Unit = {
     nermodel.ner_threshold = msg.threshold
   }

   @JsonNotification("sTeX/exportHTML")
   def exportHTML(msg: ExportMessage): Unit = Future { safely {
     msg.file = LSPServer.VSCodeToURI(msg.file)
     log("Exporting file " + msg.file)
     val d = documents.synchronized {
       documents.getOrElseUpdate(msg.file, newDocument(msg.file))
     }
     (d.archive,d.relfile) match {
       case (Some(a),Some(f)) =>
         stexserver.`export`(a,File(f),File(msg.dir))
         log("Exporting finished.")
       case _ =>
         log("Not found: Archive: " + d.archive.toString + " file path: " + d.relfile)
     }
   } }


   @JsonNotification("sTeX/exportSimpleHTML")
   def exportSimpleHTML(msg: ExportMessage): Unit = Future {
     safely {
       msg.file = LSPServer.VSCodeToURI(msg.file)
       log("Exporting file " + msg.file)
       val d = documents.synchronized {
         documents.getOrElseUpdate(msg.file, newDocument(msg.file))
       }
       withProgress(msg,"Exporting HTML...",""){update =>
         val pars = d.params(s => update(0,s))
         val ret = RusTeX.parseString(d.file.getOrElse(File(msg.file)), d.doctext,pars,envs = List(("STEX_USESMS","true")))
         File.write(File(msg.dir),ret)
         ((), "Done")
       }
     }
   }

   @JsonNotification("sTeX/buildArchive")
   def buildFile(msg: BuildGroupMessage): Unit = Future { withProgress(msg,"Building") { update =>
     log("Building file(s) " + msg.file + " in " + msg.archive)
     msg.file = msg.file.replace(".xhtml",".tex")
     controller.backend.getArchive(msg.archive) match {
       case Some(a) if msg.file.isEmpty =>
         val target = controller.extman.getOrAddExtension(classOf[FullsTeX], "fullstex").get
         val src = a / source
         val files = src.descendants.filter(f => f.getExtension.contains("tex") && !a.ignore(src.relativize(f).toFilePath)).map(src.relativize)
         val eh = STeXLSPErrorHandler(_ => {}, update)
         files.foreach{f =>
           update(0, "Building " + a.id + ": " + f)
           log("Building " + a.id + ": " + f)
           target.build(a,BuildChanged(),f.toFilePath,Some(eh))
         }
         a.readRelational(Nil,controller,"rel")
         ((),"Done")
       case Some(a) if File(a / source / msg.file).isDirectory =>
         val target = controller.extman.getOrAddExtension(classOf[FullsTeX], "fullstex").get
         val src = (a / source) / msg.file
         val files = src.descendants.filter(f => f.getExtension.contains("tex") && !a.ignore((a/source).relativize(f).toFilePath)).map(f => src.relativize(f))
         val eh = STeXLSPErrorHandler(_ => {}, update)
         files.foreach { f =>
           update(0, "Building " + a.id + ": " + f)
           log("Building " + a.id + ": " + f)
           target.build(a, BuildChanged(), f.toFilePath, Some(eh))
         }
         a.readRelational(Nil, controller, "rel")
         ((), "Done")
       case Some(a) =>
         val target = controller.extman.getOrAddExtension(classOf[FullsTeX], "fullstex").get
         val eh = STeXLSPErrorHandler(_ => {}, update)
         val relfile = File(msg.file).toFilePath
         update(0,"Building " + a.id + ": " + relfile)
         log("Building " + a.id + ": " + relfile)
         target.build(a, BuildChanged(), relfile, Some(eh))
         a.readRelational(Nil, controller, "rel")
         ((), "Done")
       case _ =>
         val archs = controller.backend.getArchives.filter(_.id.startsWith(msg.archive + "/"))
         val target = controller.extman.getOrAddExtension(classOf[FullsTeX], "fullstex").get
         val eh = STeXLSPErrorHandler(_ => {}, update)
         archs.foreach {a =>
           val src = a / source
           val files = src.descendants.filter(f => f.getExtension.contains("tex") && !a.ignore(src.relativize(f).toFilePath)).map(src.relativize)
           files.foreach {f =>
             update(0, "Building " + a.id + ": " + f)
             log("Building " + a.id + ": " + f)
             target.build(a, BuildChanged(), f.toFilePath, Some(eh))
           }
           a.readRelational(Nil, controller, "rel")
         }
         ((),"Archive not found")
     }
   }}

   @JsonNotification("sTeX/buildFile")
   def buildFile(a :BuildMessage) : Unit = {
     a.file = LSPServer.VSCodeToURI(a.file)
     log("Building file " + a.file)
     val d = documents.synchronized{documents.getOrElseUpdate(a.file,newDocument(a.file))}
     (d.archive,d.file) match {
       case (Some(a),Some(f)) if !(a / source <= f) =>
       case _ => d.buildFull()
     }
   }

   trait TemplateFileDir{
     val name: String
     def apply(f:File)(archive_id:String,namespace:String,url_base:String): Unit
   }
   private class TemplateDir(val name:String) extends TemplateFileDir {
     var children:List[TemplateFileDir] = Nil
     def apply(f:File)(archive_id:String,namespace:String,url_base:String) = {
       val path = f / name
       path.mkdirs()
       children.foreach(_.apply(path)(archive_id,namespace,url_base))
     }
   }
   private class TemplateFile(val name:String,val content:String) extends TemplateFileDir {
     def apply(f:File)(archive_id:String,namespace:String,url_base:String) = {
       val path = f / name
       File.write(path,content.replace("%%ARCHIVE%%",archive_id).replace("%%NAMESPACE%%",namespace).replace("%%URLBASE%%",url_base))
     }
   }
   private class Template(val id:String,val descr:String,val deps:List[String],val open:List[String],var files:List[TemplateFileDir]) {
     def apply(archive_id:String,namespace:String,url_base:String): Unit = {
       mathhub_top.foreach{mh =>
         val top = archive_id.split('/').foldLeft(mh)((f,s) => f / s)
         top.mkdirs()
         files.foreach(_.apply(top)(archive_id:String,namespace:String,url_base:String))
       }
     }
   }
   private object Template {
     def parse(j:JSONObject):Template = {
       val id = j.getAsString("id")
       val descr = j.getAsString("descr")
       val open = j.getAsList(classOf[JSONString],"open").map(_.value)
       val deps = Try(j.getAsList(classOf[JSONString],"dependencies").map(_.value)).getOrElse(Nil)
       val filejs = j.getAsList(classOf[JSONObject],"files")
       val files = filejs.map(parseFile)
       // TODO parameters
       new Template(id,descr,deps,open,files)
     }
     private def parseFile(js:JSONObject):TemplateFileDir = {
       val name = js.getAsString("name")
       js("content") match {
         case Some(str:JSONString) =>
           new TemplateFile(name,str.value)
         case None =>
           val chsjs = js.getAsList(classOf[JSONObject],"children")
           val dir = new TemplateDir(name)
           chsjs.foreach{j =>
             dir.children ::= parseFile(j)
           }
           dir
       }
     }
   }
   private def templates = {
     mathhub_top.toList.flatMap { mh =>
       val templatedir = mh / "sTeX" / "templates"
       if (templatedir.exists() && templatedir.isDirectory) {
         templatedir.descendants.flatMap{
           case f if f.getExtension.contains("json") =>
             Try(
               Template.parse(JSON.parse(File.read(f)).asInstanceOf[JSONObject])
             ).toOption
           case _ => None
         }
       } else Nil
     }
   }

   private def do_manifest(s : String,ns: Option[String],urlbase:Option[String]) =
     s"""
        |id:$s
        |ns:${ns.getOrElse("http://mathhub.info/"+s)}
        |narration-base:${ns.getOrElse("http://mathhub.info/"+s)}
        |url-base:${urlbase.getOrElse("https://stexmmt.mathhub.info/:sTeX")}
        |format:stex
        |""".stripMargin

   private val default_stex =
     """\documentclass{stex}
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
   def initializeArchive(a : NewArchiveMessage): Unit = Future { safely {
     log("New archive " + a.archive)
     mathhub_top.foreach {mh =>
       val ndir = mh / a.archive
       (ndir / "META-INF").mkdirs()
       File.write(ndir / "META-INF" / "MANIFEST.MF",do_manifest(a.archive,Some(a.ns),Some(a.urlbase)))
       (ndir / "lib").mkdirs()
       File.write(ndir / "lib" / "preamble.tex",s"% preamble code for ${a.archive}")
       (ndir / "source").mkdirs()
       File.write(ndir / "source" / "helloworld.tex", default_stex)
       File.write(ndir / ".gitignore",gitignore)
       controller.handleLine("mathpath archive " + ndir.toString)
       client.client.updateMathHub()
       val um = new HTMLUpdateMessage
       um.html = (ndir / "source" / "helloworld.tex").toString
       client.client.openFile(um)
     }
   }}

   @JsonRequest("sTeX/search")
   def search(p : SearchParams) : CompletableFuture[LSPSearchResults] = Completable {
     log("Search: Defis:" +p.defis + " Examples:" + p.exs + " Assertions:" + p.asserts + " Symbols:" + p.syms + " Query: " + p.query)
     var tps = if (p.defis) List("definition") else if (p.exs) List("example") else if (p.asserts) List("assertion") else Nil
     val (local,remote) = searchI(p.query,tps,p.syms)
     val ret = new LSPSearchResults
     ret.locals = local
     ret.remotes = remote
     log("Search results: " + local.size() + " locals and " + remote.size() + " remotes.")
     ret
   }

   @JsonNotification("sTeX/parseWorkspace")
   def parseWorkspace() : Unit = Future {withProgress(this,"Quickparsing Workspace"){ update =>
     log("Parsing workspace")
     var allfiles : List[File] = Nil
     allfiles = workspacefolders.flatMap(f => if (f.exists()) f.descendants.filter(fi => fi.isFile && fi.getExtension.contains("tex")) else Nil)
     log(allfiles.length.toString + " files")
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
               val relfile = (a / source).relativize(f).toFilePath
               if (!a.ignore(relfile)) d.init(File.read(f))
             case _ =>
               d.init(File.read(f))
           }
         }
     }
     ((),"Done")
   }}(scala.concurrent.ExecutionContext.global)

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
     client.client.updateMathHub()
     val lsi = new LocalServerInterface
     lsi.url = this.localServer.toString
     client.client.setLocalServer(lsi)

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
     Thread.sleep(60000)
     while (true) {
       Thread.sleep(1000)
       try {
         //log("Ping...")
         client.client.ping().get(30, TimeUnit.SECONDS)
         //log("Ping returned")
       } catch {
         case _: java.util.concurrent.TimeoutException =>
           log("First ping failed")
           try {
             client.client.ping().get(30, TimeUnit.SECONDS)
             log("Ping returned")
           } catch {
             case _: java.util.concurrent.TimeoutException =>
               log("Second ping failed")
               try {
                 client.client.ping().get(30, TimeUnit.SECONDS)
                 log("Ping returned")
               } catch {
                 case _: java.util.concurrent.TimeoutException =>
                   log("Third ping failed. Shutting down")
                   sys.exit()
               }
           }
       }
     }
   }(scala.concurrent.ExecutionContext.global)

   override def initialized(params: InitializedParams): Unit = {
     super.initialized(params)
     bootToken = Some(params.hashCode())
     controller.extman.addExtension(searchresultserver)
     startProgress(bootToken.get,"Starting sTeX/MMT","Initializing...")
     doPing
   }

   @JsonRequest("sTeX/getMathHubContent")
   def getMathHubContent() : CompletableFuture[java.util.List[MathHubEntry]] = Completable {
     bootToken.foreach {tk =>
       updateProgress(tk,0.5,"Querying remote MathHub")
     }
     val ret = getMathHubContentI()
     bootToken.foreach {tk =>
       finishProgress(tk,"Finished")
       bootToken = None
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
     if (params.getWorkspaceFolders != null) {
       params.getWorkspaceFolders.asScala.foreach {w =>
         val file = LSPServer.VSCodeToFile(w.getUri)
         if (file.exists(_.exists())) workspacefolders ::= file.get
       }
     }
   }

   @JsonNotification("sTeX/installArchive")
   def installArchive(arch: ArchiveMessage) : Unit = Future { installArchives(arch.archive) }

   @JsonNotification("sTeX/buildHTML")
   def buildHTML(a:BuildMessage): Unit = safely {
     a.file = LSPServer.VSCodeToURI(a.file)
     val d = documents.synchronized {
       documents.getOrElseUpdate(a.file, newDocument(a.file))
     }
     (d.archive, d.file) match {
       case (Some(a), Some(f)) if !(a / source <= f) =>
       case _ => d.buildHTML()
     }
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

   override def didChangeConfiguration(params: List[(String, List[(String, String)])]): Unit = Future {
     params.collect {case (a,ls) if a == "stexide" =>
       ls.collect {case ("mathhub",v) if v.nonEmpty && File(v).exists() =>
         RusTeX.initializeBridge(File(v) / ".rustex")
         this.mathhub_top = Some(File(v))
       }
     }
   }

   override def didSave(docuri: String): Unit = Future { documents.synchronized {
     documents.get(docuri) match {
       case Some(d) => d.synchronized {
         d.file match {
           case Some(f) if f.exists() =>
             d._doctext.set(File.read(f))
           case _ =>
         }
       }
       case _ =>
     }
   }}

   val self = this

   override lazy val htmlserver = Some(new ServerExtension("stexlspdocumentserver") {
     override def apply(request: ServerRequest): ServerResponse = request.path.lastOption match {
       case Some("lsperror") =>
         var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
         html = html.replace("CONTENT_URL_PLACEHOLDER", (localServer / (":" + this.pathPrefix) / "geterror").toString + "?" + request.query)
         html = html.replace("BASE_URL_PLACEHOLDER", "")
         html = html.replace("NO_FRILLS_PLACEHOLDER", "TRUE")
         html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?None")
         ServerResponse(html, "text/html")
       case Some("geterror") =>
         val ue = client.diags(client.diags.length - (1 + request.query.toInt))._2.get
         val ret = self.presenter.doHistories(ue.cp,ue.histories.reverse :_*)
         ServerResponse("<body>" + ret + "</body>","text/html")
       case Some("documentTop") =>
         request.query match {
           case "" =>
             ServerResponse("Empty Document path","txt")
           case s =>
             self.documents.get(s) match {
               case None =>
                 ServerResponse("Empty Document path","txt")
               case Some(d) =>
                 /*d.synchronized */ d.html match {
                   case Some(html) =>
                     ServerResponse(html.toString,"text/html")
                   case None =>
                     val req = request.copy(path = List(":" + stexserver.pathPrefix,"documentTop"),query = "?archive=" + d.archive.map(_.id).getOrElse("NONE") + "&filepath=" + d.relfile.map(_.setExtension("omdoc").toString.replace('\\','/')).getOrElse("NONE"))
                     stexserver(req)
                 }
             }
         }
       case Some("fulldocument") =>
         request.query match {
           case "" =>
             ServerResponse("Empty Document path", "txt")
           case s =>
             self.documents.get(s) match {
               case Some(d) if d.html.isEmpty =>
                 controller.backend.resolvePhysical(File(s)) match {
                   case Some((a, fp)) =>
                     stexserver.apply(request.copy(
                       path = List(":" + stexserver.pathPrefix, "fulldocument"),
                       query = "archive=" + a.id + "&filepath=" + fp.mkString("/").replace(".tex", ".xhtml")
                     ))
                   case _ =>
                     ServerResponse("Unknown Document", "txt")
                 }
               case None if File(s).exists() =>
                 controller.backend.resolvePhysical(File(s)) match {
                   case Some((a,fp)) =>
                     stexserver.apply(request.copy(
                       path = List(":" + stexserver.pathPrefix,"fulldocument"),
                       query = "archive=" + a.id + "&filepath=" + fp.mkString("/").replace(".tex",".xhtml")
                     ))
                   case _ =>
                     ServerResponse("Unknown Document", "txt")
                 }
               case None =>
                 ServerResponse("Empty Document path", "txt")
               case Some(d) =>
                 var html = MMTSystem.getResourceAsString("mmt-web/stex/tabsnopdf.html")
                 html = html.replace("%%HTMLSOURCE%%", (localServer / (":" + this.pathPrefix) / "fullhtml").toString + "?" + s)
                 html = html.replace("%%OMDOCSOURCE%%", localServer.toString + "/:" + stexserver.pathPrefix + "/omdoc?archive=" + d.archive.map(_.id).getOrElse("NONE") + "&filepath=" + d.relfile.map(_.setExtension("omdoc").toString.replace('\\','/')).getOrElse("NONE"))
                 ServerResponse(html, "text/html")
             }
         }
       case Some("fullhtml") =>
         request.query match {
           case "" =>
             ServerResponse("Empty Document path","txt")
           case s =>
             var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
             html = html.replace("CONTENT_URL_PLACEHOLDER",(localServer / (":" + this.pathPrefix) / "documentTop").toString + "?" + s )
             html = html.replace("BASE_URL_PLACEHOLDER","")
             html = html.replace("NO_FRILLS_PLACEHOLDER", "TRUE")
             html = html.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "false")
             html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?" + s)
             ServerResponse(html, "text/html")
         }
       case Some("css") =>
         request.query match {
           case "" =>
             ServerResponse("", "text/css")
           case s =>
             self.documents.get(s) match {
               case None =>
                 ServerResponse("", "text/css")
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

   override val scopes: List[String] = List("stex-module", "stex-symdecl", "stex-constant", "stex-variable","stex-file")
   override val modifiers: List[String] = List("deprecated")
}

object SemanticHighlighting {
  val module = 0
  val declaration = 1
  val symbol = 2
  val variable = 3
  val file = 4
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
    System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "off")
    val controller = Run.controller
    sys.env.get("HOME") match {
      case Some(f) =>
        val logf = File(f) / ".stex" / "log"
        logf.mkdirs()
        controller.handleLine(s"log html ${logf.toString}/lsplog.html")
        controller.handleLine("log+ lsp")
        controller.handleLine("log+ lsp-stex")
        controller.handleLine("log+ lsp-stex-server")
        controller.handleLine("log+ lsp-stex-server-methodcall")
      case _ =>
    }
    val mathhub_dir = File(args.head)
    var port = args(1).toInt
    if (mathhub_dir.exists()) {
      controller.handleLine("mathpath archive " + mathhub_dir.toString)
      controller.handleLine("lmh root " + mathhub_dir.toString)
    }
    controller.handleLine("server on " + port)
    while (controller.server.isEmpty) {
      port += 1
      controller.handleLine("server on " + port)
    }
    val end = new STeXLSP
    controller.extman.get(classOf[BuildManager]).foreach(controller.extman.removeExtension)
    controller.extman.addExtension(new TrivialBuildManager)
    controller.extman.addExtension(end)
    controller.backend.openArchive(mathhub_dir)
    end.runLocal
  }
}