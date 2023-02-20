package info.kwarc.mmt.lsp

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.checking.History
import info.kwarc.mmt.api.{CPath, GetError, Invalid, InvalidElement, InvalidObject, InvalidUnit, Level, SourceError}

import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.{CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem, CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams, CodeAction, CodeActionParams, CodeLens, CodeLensParams, ColorInformation, ColorPresentation, ColorPresentationParams, Command, CompletionItem, CompletionList, CompletionParams, CreateFilesParams, DeclarationParams, DefinitionParams, DeleteFilesParams, Diagnostic, DiagnosticCodeDescription, DiagnosticSeverity, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidChangeWorkspaceFoldersParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentColorParams, DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentFormattingParams, DocumentHighlight, DocumentHighlightParams, DocumentLink, DocumentLinkParams, DocumentOnTypeFormattingParams, DocumentRangeFormattingParams, DocumentSymbol, DocumentSymbolParams, ExecuteCommandParams, FoldingRange, FoldingRangeRequestParams, Hover, HoverParams, ImplementationParams, InitializeParams, InitializeResult, InitializedParams, InlayHint, InlayHintParams, InlineValue, InlineValueParams, LinkedEditingRangeParams, LinkedEditingRanges, Location, LocationLink, MessageParams, MessageType, Moniker, MonikerParams, Position, PrepareRenameDefaultBehavior, PrepareRenameParams, PrepareRenameResult, ProgressParams, PublishDiagnosticsParams, ReferenceParams, RenameFilesParams, RenameParams, SelectionRange, SelectionRangeParams, SemanticTokens, SemanticTokensDelta, SemanticTokensDeltaParams, SemanticTokensParams, SemanticTokensRangeParams, ServerCapabilities, SetTraceParams, SignatureHelp, SignatureHelpParams, SymbolInformation, TextDocumentPositionParams, TextEdit, TypeDefinitionParams, TypeHierarchyItem, TypeHierarchyPrepareParams, TypeHierarchySubtypesParams, TypeHierarchySupertypesParams, WillSaveTextDocumentParams, WorkDoneProgressBegin, WorkDoneProgressCancelParams, WorkDoneProgressCreateParams, WorkDoneProgressEnd, WorkDoneProgressNotification, WorkDoneProgressReport, WorkspaceDiagnosticParams, WorkspaceDiagnosticReport, WorkspaceEdit, WorkspaceSymbol, WorkspaceSymbolParams}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither, Either3 => JEither3}

import java.net.{ServerSocket, URLDecoder, URLEncoder}
import java.util
import java.util.logging.LogManager
import java.util.logging.Logger
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.presentation.Presenter
import info.kwarc.mmt.api.utils.time.Time
import info.kwarc.mmt.api.web.ServerExtension
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.jsr356.server.deploy.WebSocketServerContainerInitializer
import org.eclipse.lsp4j
import org.eclipse.lsp4j.adapters.{LocationLinkListAdapter, SemanticTokensFullDeltaResponseAdapter, WorkDoneProgressNotificationAdapter}
import org.eclipse.lsp4j.jsonrpc.json.ResponseJsonAdapter
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.websocket.WebSocketEndpoint

import java.io.{PrintStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets
import javax.websocket.{CloseReason, Session}
import javax.websocket.server.ServerEndpointConfig
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

/** To implement an LSP, extend the following four (abstract) classes, that work in tandem:
 * 1. trait ExampleClient extends [[LSPClient]] - can be a spurious extension, if you only need a server implementation
 *    *needs to be a trait!!*
 * 2. class ExampleServer extends [[LSPServer]](classOf[ExampleClient]) - does the actual work. An instance is created
 *    for every connection. Has a method for every LSP server request (e.g. didOpen() for textDocument/didOpen) that can
 *    be overridden. By default, every request returns the appropriate default response. Where the return type is
 *    (Option[A],Option[B]), only one of the option types should be returned.
 * 3. class ExampleWebsocket extends [[LSPWebsocket]](classOf[ExampleClient],classOf[ExampleServer]) - can also be spurious, but importantly,
 *    it needs to have a constructor with no arguments, and the class may not be declared in an object or class,
 *    because it is instantiated dynamically by Jetty.
 * 4. class ExampleLSP extends [[LSP]](classOf[ExampleClient],classOf[ExampleServer],classOf[ExampleWebsocket])(prefix,port,webport) {
 *      def newServer(style: RunStyle): ExampleServer = { implement this, should return a *new* ExampleServer }
 *    }
 *    provides methods runLocal, runSocketListener and runWebSocketListener, that run a local server or listen
 *    for a connection via sockets or websockets, respectively.
 *    @prefix: used for logging. The server logs at lsp-{prefix}-server, and additionally logs every request
 *      at lsp-{prefix}-server-methodcall.
 */

abstract class LSP[ClientClass <: LSPClient, ServerClass <: LSPServer[ClientClass], EndpointClass <: LSPWebsocket[ClientClass,ServerClass]](clc : Class[ClientClass],svct : Class[ServerClass],epc : Class[EndpointClass])(val prefix : String, port:Int, webport:Int) extends Extension {
  override def logPrefix: String = "lsp-" + prefix
  def newServer(style : RunStyle) : ServerClass
  private[lsp] def getController : Controller = controller

  override def log(s: => String, subgroup: Option[String] = None): Unit = super.log(s, subgroup)

  override def start(args: List[String]): Unit = {
    super.start(args)
    LSP.controller = controller
  }
  def runLocal = {
    LogManager.getLogManager.reset()
    val globalLogger = Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME)
    globalLogger.setLevel(java.util.logging.Level.OFF)
    // print("MMT Started")
    // startLocalServer(System.in, System.out)

    //val controller = new Controller()
    val server = newServer(LocalStyle)
    val end = new AbstractLSPServer(server,self) {}
    controller.extman.addExtension(end,"local" :: Nil)
    //val logFile = File("/home/jazzpirate/templog.txt").toJava//java.io.File.createTempFile("mmtlsp/log_","")
    //val wr = new PrintWriter(new BufferedWriter(new FileWriter(logFile)))
    val launcher = new Launcher.Builder().setLocalServices(Seq(end,server).asJava).setRemoteInterface(clc).setInput(System.in).
      setOutput(System.out).validateMessages(true)/*.traceMessages(wr)*/.setClassLoader(this.getClass.getClassLoader).create()
    val client = launcher.getRemoteProxy//.asInstanceOf[LanguageClient]
    end.connect(client)
    launcher.startListening()
  }

  private val self = this

  def runSocketListener = {
    lazy val ss = new ServerSocket(port)
    new Thread {
      override def run(): Unit = {
        while (true) /*try*/ {
          log("Waiting for connection...",Some("socket"))
          val conn = ss.accept()
          log("connected.",Some("socket"))
          //val logFile = java.io.File.createTempFile("mmtlsp/log_", "")
          //val wr = new PrintWriter(new BufferedWriter(new FileWriter(logFile)))
          //log(logFile.toString)
          val server = newServer(SocketStyle)
          val end = new AbstractLSPServer(server,self) {}
          controller.extman.addExtension(end, Nil)
          val launcher = new Launcher.Builder().setLocalServices(Seq(end,server).asJava).setRemoteInterface(clc).setInput(conn.getInputStream).
            setOutput(conn.getOutputStream).validateMessages(true).setClassLoader(this.getClass.getClassLoader)/*.traceMessages(wr)*/.create()
          end.connect(launcher.getRemoteProxy)
          launcher.startListening()
        } /* catch { ... } */
      }
    }.run()
  }

  def runWebSocketListener = {
    val server = new Server()
    val connector = new ServerConnector(server)
    connector.setPort(webport)
    connector.setHost("localhost")
    connector.setIdleTimeout(-1)
    server.setConnectors(Array(connector))

    val context = new ServletContextHandler
    context.setContextPath("/")
    server.setHandler(context)

    val container = WebSocketServerContainerInitializer.initialize(context)
    val endpointConfig = ServerEndpointConfig.Builder.create(epc, "/").build
    container.addEndpoint(endpointConfig)

    server.start()
  }
}

sealed class RunStyle
case object LocalStyle extends RunStyle
case object SocketStyle extends RunStyle
case object WebSocketStyle extends RunStyle

class UnitError(val cp: CPath, h: History) {
  var histories = List(h)
}

class ClientWrapper[+A <: LSPClient](val client : A,server:LSPServer[A]) {
  private def normalizeUri(s : String) : String = s.take(5) + s.drop(5).replace(":","%3A")
  def log(s : String) = client.logMessage(new MessageParams(MessageType.Info,s))
  def logError(s : String) = client.logMessage(new MessageParams(MessageType.Error,s))
  def resetErrors(uri:String) = this.synchronized {
    diags = Nil
    all_errors = Nil
    val params = new PublishDiagnosticsParams()
    params.setUri(normalizeUri(uri))
    params.setDiagnostics(Nil.asJava)
    client.publishDiagnostics(params)
  }
  var diags : List[(Diagnostic,Option[UnitError])] = Nil
  var all_errors : List[info.kwarc.mmt.api.Error] = Nil

  def republishErrors(uri:String) = {
    val params = new PublishDiagnosticsParams()
    params.setUri(uri)
    params.setDiagnostics(diags.map(_._1).asJava)
    client.publishDiagnostics(params)
  }

  def documentErrors(doc : LSPDocument[LSPClient,LSPServer[LSPClient]],errors : info.kwarc.mmt.api.Error*) = this.synchronized {if (errors.nonEmpty) {
    val controller = server.controller
    val params = new PublishDiagnosticsParams()
    params.setUri(normalizeUri(doc.uri))
    def get(sr : SourceRef,lvl:Level,msg : String) = {
      val d = new Diagnostic()
      val start = sr.region.start.offset
      val end = sr.region.end.offset + 1
      val (sl, sc) = doc._doctext.toLC(start)
      val (el, ec) = doc._doctext.toLC(end)
      d.setRange(new lsp4j.Range(new Position(sl, sc), new Position(el, ec)))
      d.setMessage(msg)
      d.setSeverity(lvl match {
        case Level.Info => DiagnosticSeverity.Information
        case Level.Error => DiagnosticSeverity.Error
        case Level.Warning => DiagnosticSeverity.Warning
        case Level.Fatal => DiagnosticSeverity.Error
      })
      d
    }
    errors.foreach{e => if (!all_errors.contains(e)) {
      all_errors ::= e
      e match {
        case ge:GetError =>
          diags::= (get(SourceRef.anonymous(""),Level.Warning,ge.shortMsg),None)
        case SourceError(_,ref,_,ems,_) =>
          diags ::= (get(ref,e.level,e.shortMsg + ems.mkString("\n","\n","")),None)
        case ie: InvalidElement =>
          val ref = SourceRef.get(ie.elem).getOrElse(SourceRef.anonymous(""))
          diags ::= (get(ref,Level.Warning,ie.shortMsg + "\n" + ie.extraMessage),None)
        case io : InvalidObject =>
          val ref = io.sourceRef.getOrElse(SourceRef.anonymous(""))
          diags ::= (get(ref, Level.Warning, io.shortMsg + "\n" + io.extraMessage), None)
        case iu:InvalidUnit =>
          val ref = iu.unit.component.map {
            c =>
              controller.getO(c).flatMap(SourceRef.get).getOrElse {
                controller.getO(c.parent).flatMap(SourceRef.get).getOrElse(SourceRef.anonymous(""))
              }
          }.getOrElse(SourceRef.anonymous(""))
          (server.htmlserver, iu.unit.component) match {
            case (Some(s), Some(comp)) =>
              diags.find(_._2.exists(_.cp == comp)) match {
                case Some((_,Some(p))) => p.histories ::= iu.history.narrowDownError
                case _ =>
                  val dcc = new DiagnosticCodeDescription()
                  dcc.setHref((controller.server.get.baseURI / (":" + s.pathPrefix) / ("lsperror?" + diags.length.toString)).toString)
                  val d = get(ref, Level.Warning, iu.shortMsg)
                  d.setCode("Invalid Unit")
                  d.setCodeDescription(dcc)
                  diags ::= (d, Some(new UnitError(comp, iu.history.narrowDownError)))
              }
            case _ =>
              diags ::= (get(ref,Level.Warning,iu.shortMsg + "\n" + iu.history.narrowDownError.present(server.presenter)), None)
          }
        case _ =>
          diags ::= (get(SourceRef.anonymous(""), e.level, e.shortMsg), None)
      }
    }}

    params.setDiagnostics(diags.map(_._1).asJava)
    client.publishDiagnostics(params)
  }}
}

trait LSPClient extends LanguageClient {}

class LSPWebsocket[ClientClass <: LSPClient, ServerClass <: LSPServer[ClientClass]](clct : Class[ClientClass],svct : Class[ServerClass]) extends WebSocketEndpoint[ClientClass] {
  override def configure(builder: Launcher.Builder[ClientClass]): Unit = {
    val lsp = LSP.controller.extman.get(classOf[LSP[ClientClass,ServerClass,this.type]]).headOption.getOrElse({
      ???
    })
    val server = lsp.newServer(WebSocketStyle)
    val end = new AbstractLSPServer(server,lsp) {}
    LSP.controller.extman.addExtension(end,Nil)
    builder.setLocalServices(Seq(end,server).asJava).setClassLoader(this.getClass.getClassLoader).setRemoteInterface(clct)
  }

  override def connect(localServices: util.Collection[AnyRef], remoteProxy: ClientClass): Unit = {
    localServices.asScala.collect{case lca : LanguageClientAware => lca}.foreach(_.connect(remoteProxy))
  }
}

abstract class SandboxedWebSocket[ClientClass <: LSPClient, ServerClass <: LSPServer[ClientClass]](clct : Class[ClientClass],svct : Class[ServerClass]) extends LSPWebsocket(clct,svct) {

  def initialize : Unit
  protected var _lsp : Option[LSP[ClientClass, ServerClass, this.type]] = None
  override def configure(builder: Launcher.Builder[ClientClass]): Unit = {
    initialize
    _lsp.foreach { lsp =>
      val server = lsp.newServer(WebSocketStyle)
      val end = new AbstractLSPServer(server, lsp) {}
      LSP.controller.extman.addExtension(end, Nil)
      builder.setLocalServices(Seq(end, server).asJava).setClassLoader(this.getClass.getClassLoader).setRemoteInterface(clct)
    }
  }

  override def onClose(session: Session, closeReason: CloseReason): Unit = {
    super.onClose(session, closeReason)
    _lsp.foreach{ lsp =>
      val ctrl = lsp.getController
      ctrl.server.foreach(_.stop)
      ctrl.extman.cleanup
    }
  }

  override def connect(localServices: util.Collection[AnyRef], remoteProxy: ClientClass): Unit = {
    localServices.asScala.collect { case lca: LanguageClientAware => lca }.foreach(_.connect(remoteProxy))
  }
}
object SandboxedWebSocket {
  def runWebSocketListener[ClientClass <: LSPClient, ServerClass <: LSPServer[ClientClass], EndpointClass <: SandboxedWebSocket[ClientClass,ServerClass]](epc:Class[EndpointClass],webport:Int) = {
    val server = new Server()
    val connector = new ServerConnector(server)
    connector.setPort(webport)
    connector.setHost("localhost")
    connector.setIdleTimeout(-1)
    server.setConnectors(Array(connector))

    val context = new ServletContextHandler
    context.setContextPath("/")
    server.setHandler(context)

    val container = WebSocketServerContainerInitializer.initialize(context)
    val endpointConfig = ServerEndpointConfig.Builder.create(epc, "/").build
    container.addEndpoint(endpointConfig)
    server.start()
  }
}

object LSP {
  private[lsp] var controller : Controller = null
}

class LSPServer[+ClientType <: LSPClient](clct : Class[ClientType]) {
  private var _log : (String,Option[String]) => Unit = null
  private var _controller : Controller = null
  private var _client : Option[ClientWrapper[LSPClient]] = None
  def presenter : Presenter = _controller.presenter
  def htmlserver : Option[ServerExtension] = None
  def controller = _controller
  protected def log(s : String, subgroup : Option[String] = None) = _log(s,subgroup)
  def client = _client.getOrElse({
    ???
  }).asInstanceOf[ClientWrapper[ClientType]]

  def safely[A](f : => A) : A = try {
    f
  } catch {
    case t: Throwable =>
      val sw = new StringWriter()
      t.printStackTrace(new PrintWriter(sw))
      t.printStackTrace()
      client.logError(sw.toString)
      throw t
  }

  private[lsp] def init(__log: (String,Option[String]) => Unit, ctrl : Controller) = {
    _log = __log
    _controller = ctrl
  }
  private[lsp] def connectI(cl : LSPClient) = {
    _client = Some(new ClientWrapper(cl.asInstanceOf[ClientType],this))
    connect
  }
  protected object Completable {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.compat.java8.FutureConverters._
    def apply[T](t : => T) = Future.apply(t).toJava.toCompletableFuture
    def list[T](t : => List[T]) : CompletableFuture[util.List[T]] = apply{ t.asJava }
  }

  def startProgress(token: Int,title:String,msg:String = ""): Unit = {
    val params = new WorkDoneProgressCreateParams
    params.setToken(token)
    client.client.createProgress(params)
    val nparams = new ProgressParams
    nparams.setToken(token)
    val nt = new WorkDoneProgressBegin
    nt.setMessage(msg)
    nt.setTitle(title)
    nt.setPercentage(0)
    nparams.setValue(JEither.forLeft(nt))
    client.client.notifyProgress(nparams)
  }
  def updateProgress(token:Int,progress:Double,msg:String) = {
    val np = new ProgressParams
    np.setToken(token)
    val nt = new WorkDoneProgressReport
    nt.setMessage(msg)
    nt.setPercentage(math.round(progress * 100).toInt)
    np.setValue(JEither.forLeft(nt))
    client.client.notifyProgress(np)
  }
  def finishProgress(token:Int,msg:String) = {
    val np = new ProgressParams
    np.setToken(token)
    val nt = new WorkDoneProgressEnd
    nt.setMessage(msg)
    np.setValue(JEither.forLeft(nt))
    client.client.notifyProgress(np)
  }


  def withProgress[A](payload: Any,title:String,msg:String = "")(f : ((Double,String) => Unit) => (A,String)) : A = {
    val token = payload.hashCode();
    startProgress(token,title,msg)
    val (ret,end) = try { f((i,s) => {
      updateProgress(token,i,s)
    }) } catch {
      case t: Throwable =>
        finishProgress(token, "")
        client.logError(t.getMessage + "\n" + t.getStackTrace.mkString("\n"))
        throw t
    }
    finishProgress(token, end)
    ret
  }

  def connect: Unit = {}
  def initialize(params: InitializeParams,result : InitializeResult) = {}
  def shutdown : Any = {}
  def exit : Unit = {}
  def initialized(params: InitializedParams): Unit = {}
  def didChangeConfiguration(params: List[(String,List[(String,String)])]): Unit = {}
  def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {}
  def workspaceSymbol(params: WorkspaceSymbolParams): List[WorkspaceSymbol] = Nil
  def executeCommand(params: ExecuteCommandParams): Any = {}
  def didOpen(params: DidOpenTextDocumentParams) : Unit = {}
  def didChange(params: DidChangeTextDocumentParams): Unit = {}
  def didClose(params: DidCloseTextDocumentParams): Unit = {}
  def didSave(docuri:String): Unit = {}
  def definition(position: DefinitionParams): (Option[List[Location]],Option[List[LocationLink]]) = (None,None)
  def typeDefinition(position: TypeDefinitionParams): (Option[List[Location]],Option[List[LocationLink]]) = (None,None)
  def implementation(position: ImplementationParams): List[Location] = Nil
  def declaration(params: DeclarationParams): List[Location] = Nil
  def hover(params: HoverParams): Hover = null
  def documentHighlights(params: TextDocumentPositionParams): List[DocumentHighlight] = Nil
  def documentSymbol(params: DocumentSymbolParams): List[(Option[SymbolInformation],Option[DocumentSymbol])] = Nil
  def formatting(params: DocumentFormattingParams): List[TextEdit] = Nil
  def rename(params: RenameParams): WorkspaceEdit = null
  def references(params: ReferenceParams): List[Location] = Nil
  def completion(position: CompletionParams): (Option[List[CompletionItem]],Option[CompletionList]) = (None,None)
  def signatureHelp(params: SignatureHelpParams): SignatureHelp = null
  def codeAction(params: CodeActionParams): List[CodeAction] = Nil
  def codeLens(params: CodeLensParams): List[CodeLens] = Nil
  def foldingRange(params: FoldingRangeRequestParams): List[FoldingRange] = Nil
  def semanticTokensFull(params: SemanticTokensParams) : SemanticTokens = null
  def semanticTokensFullDelta(params: SemanticTokensDeltaParams): (Option[SemanticTokens], Option[SemanticTokensDelta]) = (None,None)
  def semanticTokensRange(params: SemanticTokensRangeParams): SemanticTokens = null
  def inlayHint(params:InlayHintParams) : List[InlayHint] = Nil
  def setTrace(params: SetTraceParams): Unit = {}
}

object LSPServer {
  def URItoVSCode(s : String) : String = URLEncoder.encode(s.replace("+","%2B"),StandardCharsets.UTF_8)
  def VSCodeToURI(s : String) : String = {
    val dec = URLDecoder.decode(s,StandardCharsets.UTF_8)
    if (dec.startsWith("file:///") && dec(9) == ':') {
      dec.take(8) + dec(8).toUpper + dec.drop(9)
    } else dec
  }//s.replace("%3A",":")
}

class AbstractLSPServer[A <: LSPClient, B <: LSPServer[A], C <: LSPWebsocket[A,B]](val server : B,lsp:LSP[A,B,C])
  extends LanguageClientAware
    with LanguageServer
    with WorkspaceService
    with TextDocumentService
    with Extension {
  override def logPrefix: String = "lsp-" + lsp.prefix + "-server"

  override def start(args: List[String]): Unit = {
    super.start(args)
    server.init((s,g) => lsp.log(s,Some("-server" + g.map("-"+_).getOrElse(""))),controller)
  }
  private object Completable {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.compat.java8.FutureConverters._
    def apply[T](t : => T) = Future{
      //val (time,ret) = Time.measure {
        t
      /*}
      println("Took: " + time)
      ret*/
    }.toJava.toCompletableFuture
    def list[T](t : => List[T]) : CompletableFuture[util.List[T]] = apply{ t.asJava }
  }

  private def toEither[A,B](p : (Option[A],Option[B])) = p match {
    case (Some(l),_) =>
      JEither.forLeft(l)
    case (_,Some(l)) =>
      JEither.forRight(l)
    case _ => null
  }

  override def getTextDocumentService: TextDocumentService = this
  override def getWorkspaceService: WorkspaceService = this

  //@JsonNotification("connect")
  override def connect(clientO: LanguageClient): Unit = Completable {
    log("Connected: " + clientO.toString,Some("methodcall"))
    server.connectI(clientO.asInstanceOf[A])
  }

  //@JsonRequest("initialize")
  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    log("initialize", Some("methodcall"))
    Completable {
      val result = new InitializeResult(new ServerCapabilities)

      server.initialize(params,result)
      result
    }
  }

  //@JsonRequest("shutdown")
  override def shutdown(): CompletableFuture[Object] = {
    log("shutdown",Some("methodcall"))
    Completable{
      val r = server.shutdown
      this.controller.extman.removeExtension(this)
      r.asInstanceOf[Object]
    }
  }

  //@JsonNotification("exit")
  override def exit(): Unit = Completable {
    log("exit",Some("methodcall"))
    server.exit
    this.controller.extman.removeExtension(this)
  }

  //@JsonNotification("initialized")
  override def initialized(params: InitializedParams): Unit = Completable {
    log("initialized",Some("methodcall"))
    server.initialized(params)
  }

  /** see [[org.eclipse.lsp4j.services.WorkspaceService]]**/

  //@JsonNotification("workspace/didChangeConfiguration")
  override def didChangeConfiguration(
                              params: DidChangeConfigurationParams
                            ): Unit = Completable {
    log("workspace/didChangeConfiguration: " + params.toString,Some("methodcall"))
    params.getSettings match {
      case o: com.google.gson.JsonObject =>
        val ret = o.entrySet().asScala.toList.map {e =>
          (e.getKey,e.getValue match {
            case o: com.google.gson.JsonObject =>
              o.entrySet().asScala.toList.map {e =>
                (e.getKey,e.getValue match {
                  case o: com.google.gson.JsonPrimitive if o.isString => o.getAsString
                  case _ => ???
                })
              }
            case  _ => ???
          })
        }
        server.didChangeConfiguration(ret)
      case _ => ???
    }
  }

  //@JsonNotification("workspace/didChangeWatchedFiles")
  override def didChangeWatchedFiles(
                             params: DidChangeWatchedFilesParams
                           ): Unit = Completable {
    log("workspace/didChangeWatchedFiles: " + params.toString,Some("methodcall"))
    server.didChangeWatchedFiles(params)
  }

  //@JsonRequest("workspace/executeCommand")
  override def executeCommand(params: ExecuteCommandParams): CompletableFuture[Object] = {
    log("workspace/executeCommand: " + params.toString,Some("methodcall"))
    Completable { server.executeCommand(params).asInstanceOf[Object] }
  }

  /** see [[org.eclipse.lsp4j.services.TextDocumentService]]**/

  //@JsonRequest("textDocument/completion")
  override def completion(position: CompletionParams): CompletableFuture[JEither[util.List[CompletionItem], CompletionList]] = Completable {
    position.getTextDocument.setUri(LSPServer.VSCodeToURI(position.getTextDocument.getUri))
    log("textDocument/completion: " + position.getTextDocument.getUri + " at (" +
      position.getPosition.getLine + "," + position.getPosition.getCharacter + ")"
      ,Some("methodcall"))
    toEither{
      val ret = server.completion(position)
      (ret._1.map(_.asJava),ret._2)
    }.asInstanceOf[JEither[util.List[CompletionItem], CompletionList]]
  }

  //@JsonRequest("textDocument/hover")
  override def hover(params: HoverParams): CompletableFuture[Hover] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/hover: " + params.getTextDocument.getUri + ":(" + params.getPosition.getLine + "," + params.getPosition.getCharacter + ")",Some("methodcall"))
    server.hover(params)
  }

  //@JsonRequest("textDocument/signatureHelp")
  override def signatureHelp(
                     params: SignatureHelpParams
                   ): CompletableFuture[SignatureHelp] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/signatureHelp: " + params.toString,Some("methodcall"))
    server.signatureHelp(params)
  }

  //@JsonRequest("textDocument/references")
  override def references(
                  params: ReferenceParams
                ): CompletableFuture[util.List[_ <: Location]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/reference: " + params.toString,Some("methodcall"))
    server.references(params).asJava
  }

  //@JsonRequest("textDocument/documentHighlight")
  override def documentHighlight(
                          params: DocumentHighlightParams
                        ): CompletableFuture[util.List[_ <: DocumentHighlight]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/documentHighlight: " + params.toString,Some("methodcall"))
    server.documentHighlights(params).asJava
  }

  //@JsonNotification("textDocument/didOpen")
  override def didOpen(params: DidOpenTextDocumentParams): Unit = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/didOpen: " + params.getTextDocument.getUri + " (" + params.getTextDocument.getLanguageId + ")",Some("methodcall"))
    server.didOpen(params)
  }

  //@JsonNotification("textDocument/didChange")
  override def didChange(
                 params: DidChangeTextDocumentParams
               ): Unit = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/didChange: " + params.getContentChanges.asScala.map(c =>
      "(" + c.getRange.getStart.getLine + "," + c.getRange.getStart.getCharacter + "),(" +
        c.getRange.getEnd.getLine + "," + c.getRange.getEnd.getCharacter + ")=\"" + c.getText + "\""
    ).mkString(" "),Some("methodcall"))
    server.didChange(params)
  }


  //@JsonNotification("textDocument/didClose")
  override def didClose(params: DidCloseTextDocumentParams): Unit = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/didClose: " + params.toString,Some("methodcall"))
    server.didClose(params)
  }


  //@JsonNotification("textDocument/didSave")
  override def didSave(params: DidSaveTextDocumentParams): Unit = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/didSave: " + params.getTextDocument.getUri,Some("methodcall"))
    server.didSave(params.getTextDocument.getUri)
  }


  //@JsonRequest("textDocument/definition")
  override def definition(
                  params: DefinitionParams
                ): CompletableFuture[JEither[util.List[_ <: Location],util.List[_ <: LocationLink]]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/definition: " + params.toString,Some("methodcall"))
    toEither{
      val ret = server.definition(params)
      (ret._1.map(_.asJava),ret._2.map(_.asJava))
    }.asInstanceOf[JEither[util.List[_ <: Location],util.List[_ <: LocationLink]]]
  }


  //@JsonRequest("textDocument/typeDefinition")
  override def typeDefinition(
                      position: TypeDefinitionParams
                    ): CompletableFuture[JEither[util.List[_ <: Location],util.List[_ <: LocationLink]]] = Completable {
    position.getTextDocument.setUri(LSPServer.VSCodeToURI(position.getTextDocument.getUri))
    log("textDocument/typeDefinition: " + position.toString,Some("methodcall"))
    toEither{
      val ret = server.typeDefinition(position)
      (ret._1.map(_.asJava),ret._2.map(_.asJava))
    }.asInstanceOf[JEither[util.List[_ <: Location],util.List[_ <: LocationLink]]]
  }


  //@JsonRequest("textDocument/implementation")
  override def implementation(
                      position: ImplementationParams
                    ): CompletableFuture[JEither[util.List[_ <: Location],util.List[_ <: LocationLink]]] = Completable {
    position.getTextDocument.setUri(LSPServer.VSCodeToURI(position.getTextDocument.getUri))
    log("textDocument/implementation: " + position.toString,Some("methodcall"))
    toEither{(Some(server.implementation(position).asJava),None)}.asInstanceOf[JEither[util.List[_ <: Location],util.List[_ <: LocationLink]]]
  }

  override def declaration(params: DeclarationParams): CompletableFuture[JEither[util.List[_ <: Location], util.List[_ <: LocationLink]]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/implementation: " + params.toString,Some("methodcall"))
    val ret = server.declaration(params)
    toEither{(Some(ret.asJava),None)}.asInstanceOf[JEither[util.List[_ <: Location],util.List[_ <: LocationLink]]]
  }



  //@JsonRequest("textDocument/documentSymbol")
  override def documentSymbol(
                      params: DocumentSymbolParams
                    ): CompletableFuture[util.List[JEither[SymbolInformation,DocumentSymbol]]] = Completable {
  //CompletableFuture[JEither[util.List[DocumentSymbol], util.List[SymbolInformation]]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/documentSymbol: " + params.getTextDocument.getUri,Some("methodcall"))
    val sc = try {
      server.documentSymbol(params)
    } catch {
      case t : Throwable =>
        t.printStackTrace()
        return Completable(Nil.asJava)
    }
    print("")
    sc.map(p =>
      toEither(p).asInstanceOf[JEither[SymbolInformation,DocumentSymbol]]
    ).asJava
  }

  //@JsonRequest("textDocument/formatting")
  override def formatting(
                  params: DocumentFormattingParams
                ): CompletableFuture[util.List[_ <: TextEdit]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/formatting: " + params.toString,Some("methodcall"))
    server.formatting(params).asJava
  }


  //@JsonRequest("textDocument/rename")
  override def rename(
              params: RenameParams
            ): CompletableFuture[WorkspaceEdit] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/rename: " + params.toString,Some("methodcall"))
    server.rename(params)
  }

  //@JsonRequest("textDocument/codeAction")
  override def codeAction(
                  params: CodeActionParams
                ): CompletableFuture[util.List[JEither[Command,CodeAction]]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/codeAction: " + params.toString,Some("methodcall"))
    server.codeAction(params).map(e => toEither(None.asInstanceOf[Option[Command]],Some(e)).asInstanceOf[JEither[Command,CodeAction]]).asJava
  }

  //@JsonRequest("textDocument/codeLens")
  override def codeLens(
                params: CodeLensParams
              ): CompletableFuture[util.List[_ <: CodeLens]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/codeLens: " + params.getTextDocument.getUri,Some("methodcall"))
    server.codeLens(params).asJava
  }

  //@JsonRequest("textDocument/foldingRange")
  override def foldingRange(
                    params: FoldingRangeRequestParams
                  ): CompletableFuture[util.List[FoldingRange]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/foldingRange: " + params.getTextDocument.getUri,Some("methodcall"))
    server.foldingRange(params).asJava
  }

  //@JsonRequest(value = "textDocument/semanticTokens/full")
  override def semanticTokensFull(params: SemanticTokensParams) : CompletableFuture[SemanticTokens] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/semanticTokens/full: " + params.getTextDocument.getUri,Some("methodcall"))
    server.semanticTokensFull(params)
  }

  //@JsonRequest(value = "textDocument/semanticTokens/full/delta", useSegment = false)
  //@ResponseJsonAdapter(classOf[SemanticTokensFullDeltaResponseAdapter])
  override def semanticTokensFullDelta(params: SemanticTokensDeltaParams): CompletableFuture[JEither[SemanticTokens, SemanticTokensDelta]] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/semanticTokens/full/delta: " + params.toString,Some("methodcall"))
    toEither(server.semanticTokensFullDelta(params)).asInstanceOf[JEither[SemanticTokens, SemanticTokensDelta]]
  }

  //@JsonRequest(value = "textDocument/semanticTokens/range", useSegment = false)
  override def semanticTokensRange(params: SemanticTokensRangeParams): CompletableFuture[SemanticTokens] = Completable {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/semanticTokens/range: " + params.toString,Some("methodcall"))
    server.semanticTokensRange(params)
  }

  override def symbol(params: WorkspaceSymbolParams): CompletableFuture[JEither[util.List[_ <: SymbolInformation], util.List[_ <: WorkspaceSymbol]]] = Completable {
    toEither((None,Some(server.workspaceSymbol(params).asJava))).asInstanceOf[JEither[util.List[_ <: SymbolInformation], util.List[_ <: WorkspaceSymbol]]]
  }

  override def inlayHint(params: InlayHintParams): CompletableFuture[util.List[InlayHint]] = {
    params.getTextDocument.setUri(LSPServer.VSCodeToURI(params.getTextDocument.getUri))
    log("textDocument/inlayHint: " + params.getTextDocument.getUri + " Line " + params.getRange.getStart.getLine,Some("methodcall"))
    Completable { server.inlayHint(params).asJava }
  }

  override def setTrace(params: SetTraceParams): Unit = Completable { server.setTrace(params) }

  override def callHierarchyIncomingCalls(params: CallHierarchyIncomingCallsParams): CompletableFuture[util.List[CallHierarchyIncomingCall]] = super.callHierarchyIncomingCalls(params)
  override def callHierarchyOutgoingCalls(params: CallHierarchyOutgoingCallsParams): CompletableFuture[util.List[CallHierarchyOutgoingCall]] = super.callHierarchyOutgoingCalls(params)
  override def resolveCompletionItem(unresolved: CompletionItem): CompletableFuture[CompletionItem] = super.resolveCompletionItem(unresolved)
  override def resolveCodeAction(unresolved: CodeAction): CompletableFuture[CodeAction] = super.resolveCodeAction(unresolved)
  override def resolveCodeLens(unresolved: CodeLens): CompletableFuture[CodeLens] = super.resolveCodeLens(unresolved)
  override def resolveInlayHint(unresolved: InlayHint): CompletableFuture[InlayHint] = super.resolveInlayHint(unresolved)
  override def resolveWorkspaceSymbol(workspaceSymbol: WorkspaceSymbol): CompletableFuture[WorkspaceSymbol] = super.resolveWorkspaceSymbol(workspaceSymbol)
  override def rangeFormatting(params: DocumentRangeFormattingParams): CompletableFuture[util.List[_ <: TextEdit]] = super.rangeFormatting(params)
  override def documentLinkResolve(params: DocumentLink): CompletableFuture[DocumentLink] = super.documentLinkResolve(params)
  override def documentLink(params: DocumentLinkParams): CompletableFuture[util.List[DocumentLink]] = super.documentLink(params)
  override def typeHierarchySupertypes(params: TypeHierarchySupertypesParams): CompletableFuture[util.List[TypeHierarchyItem]] = super.typeHierarchySupertypes(params)
  override def prepareTypeHierarchy(params: TypeHierarchyPrepareParams): CompletableFuture[util.List[TypeHierarchyItem]] = super.prepareTypeHierarchy(params)
  override def typeHierarchySubtypes(params: TypeHierarchySubtypesParams): CompletableFuture[util.List[TypeHierarchyItem]] = super.typeHierarchySubtypes(params)
  override def diagnostic(params: WorkspaceDiagnosticParams): CompletableFuture[WorkspaceDiagnosticReport] = super.diagnostic(params)
  override def cancelProgress(params: WorkDoneProgressCancelParams): Unit = super.cancelProgress(params)
  override def didChangeWorkspaceFolders(params: DidChangeWorkspaceFoldersParams): Unit = super.didChangeWorkspaceFolders(params)
  override def willCreateFiles(params: CreateFilesParams): CompletableFuture[WorkspaceEdit] = super.willCreateFiles(params)
  override def didCreateFiles(params: CreateFilesParams): Unit = super.didCreateFiles(params)
  override def willRenameFiles(params: RenameFilesParams): CompletableFuture[WorkspaceEdit] = super.willRenameFiles(params)
  override def didRenameFiles(params: RenameFilesParams): Unit = super.didRenameFiles(params)
  override def willDeleteFiles(params: DeleteFilesParams): CompletableFuture[WorkspaceEdit] = super.willDeleteFiles(params)
  override def didDeleteFiles(params: DeleteFilesParams): Unit = super.didDeleteFiles(params)
  override def onTypeFormatting(params: DocumentOnTypeFormattingParams): CompletableFuture[util.List[_ <: TextEdit]] = super.onTypeFormatting(params)
  override def linkedEditingRange(params: LinkedEditingRangeParams): CompletableFuture[LinkedEditingRanges] = super.linkedEditingRange(params)
  override def willSave(params: WillSaveTextDocumentParams): Unit = super.willSave(params)
  override def willSaveWaitUntil(params: WillSaveTextDocumentParams): CompletableFuture[util.List[TextEdit]] = super.willSaveWaitUntil(params)
  override def documentColor(params: DocumentColorParams): CompletableFuture[util.List[ColorInformation]] = super.documentColor(params)
  override def colorPresentation(params: ColorPresentationParams): CompletableFuture[util.List[ColorPresentation]] = super.colorPresentation(params)
  override def prepareRename(params: PrepareRenameParams): CompletableFuture[JEither3[lsp4j.Range, PrepareRenameResult,PrepareRenameDefaultBehavior]] = super.prepareRename(params)
  override def prepareCallHierarchy(params: CallHierarchyPrepareParams): CompletableFuture[util.List[CallHierarchyItem]] = super.prepareCallHierarchy(params)
  override def selectionRange(params: SelectionRangeParams): CompletableFuture[util.List[SelectionRange]] = super.selectionRange(params)
  override def moniker(params: MonikerParams): CompletableFuture[util.List[Moniker]] = super.moniker(params)
  override def inlineValue(params: InlineValueParams): CompletableFuture[util.List[InlineValue]] = super.inlineValue(params)
  override def diagnostic(params: DocumentDiagnosticParams): CompletableFuture[DocumentDiagnosticReport] = super.diagnostic(params)
}