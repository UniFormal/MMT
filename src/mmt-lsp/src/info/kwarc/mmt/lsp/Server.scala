package info.kwarc.mmt.lsp

import info.kwarc.mmt.api.SourceError

import java.util.concurrent.CompletableFuture
import org.eclipse.lsp4j.{CodeAction, CodeActionParams, CodeLens, CodeLensParams, CompletionItem, CompletionList, CompletionParams, Diagnostic, DiagnosticSeverity, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, DocumentHighlight, DocumentSymbol, DocumentSymbolParams, ExecuteCommandParams, FoldingRange, FoldingRangeRequestParams, Hover, HoverParams, InitializeParams, InitializeResult, InitializedParams, Location, LocationLink, MessageParams, MessageType, Position, PublishDiagnosticsParams, ReferenceParams, RenameParams, SemanticTokens, SemanticTokensDelta, SemanticTokensDeltaParams, SemanticTokensParams, SemanticTokensRangeParams, ServerCapabilities, SignatureHelp, SignatureHelpParams, SymbolInformation, TextDocumentPositionParams, TextEdit, WorkspaceEdit, WorkspaceSymbolParams}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware}
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.jsonrpc.messages.{Either3, Either => JEither}

import java.net.ServerSocket
import java.util
import java.util.logging.LogManager
import java.util.logging.Logger
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.utils.File
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.websocket.jsr356.server.deploy.WebSocketServerContainerInitializer
import org.eclipse.lsp4j
import org.eclipse.lsp4j.adapters.SemanticTokensFullDeltaResponseAdapter
import org.eclipse.lsp4j.jsonrpc.json.ResponseJsonAdapter
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.websocket.WebSocketEndpoint

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import javax.websocket.server.ServerEndpointConfig
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
    val end = new AbstractLSPServer(newServer(LocalStyle),self) {}
    controller.extman.addExtension(end,"local" :: Nil)
    //val logFile = File("/home/jazzpirate/templog.txt").toJava//java.io.File.createTempFile("mmtlsp/log_","")
    //val wr = new PrintWriter(new BufferedWriter(new FileWriter(logFile)))
    val launcher = new Launcher.Builder().setLocalService(end).setRemoteInterface(clc).setInput(System.in).
      setOutput(System.out).validateMessages(true)/*.traceMessages(wr)*/.create()
    val client = launcher.getRemoteProxy//.asInstanceOf[LanguageClient]
    end.connect(client)
    launcher.startListening()
  }

  private val self = this

  def runSocketListener = {
    lazy val ss = new ServerSocket(port)
    new Thread {
      override def run(): Unit = {
        while (true) try {
          log("Waiting for connection...",Some("socket"))
          val conn = ss.accept()
          log("connected.",Some("socket"))
          //val logFile = java.io.File.createTempFile("mmtlsp/log_", "")
          //val wr = new PrintWriter(new BufferedWriter(new FileWriter(logFile)))
          //log(logFile.toString)
          val end = new AbstractLSPServer(newServer(SocketStyle),self) {}
          controller.extman.addExtension(end, Nil)
          val launcher = new Launcher.Builder().setLocalService(end).setRemoteInterface(clc).setInput(conn.getInputStream).
            setOutput(conn.getOutputStream).validateMessages(true)/*.traceMessages(wr)*/.create()
          end.connect(launcher.getRemoteProxy)
          launcher.startListening()
        }
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

class ClientWrapper[+A <: LSPClient](val client : A) {
  private def normalizeUri(s : String) : String = s.take(5) + s.drop(5).replace(":","%3A")
  def log(s : String) = client.logMessage(new MessageParams(MessageType.Info,s))
  def logError(s : String) = client.logMessage(new MessageParams(MessageType.Error,s))
  def resetErrors(uri:String) = {
    val params = new PublishDiagnosticsParams()
    params.setUri(normalizeUri(uri))
    params.setDiagnostics(Nil.asJava)
    client.publishDiagnostics(params)
  }
  def documentErrors(doc : String,uri : String,errors : info.kwarc.mmt.api.Error*) = if (errors.nonEmpty) {
    val params = new PublishDiagnosticsParams()
    params.setUri(normalizeUri(uri))
    val diags = errors.map{e =>
      val d = new Diagnostic
      e match {
        case SourceError(_,ref,_,_,_) =>
          val start = ref.region.start.offset
          val end = ref.region.end.offset + 1
          val (sl,sc) = LSPDocument.toLC(start,doc)
          val (el,ec) = LSPDocument.toLC(end,doc)
          d.setRange(new lsp4j.Range(new Position(sl,sc),new Position(el,ec)))
        case _ =>
      }
      import info.kwarc.mmt.api.Level
      d.setSeverity(e.level match {
        case Level.Info => DiagnosticSeverity.Information
        case Level.Error => DiagnosticSeverity.Error
        case Level.Warning => DiagnosticSeverity.Warning
        case Level.Fatal => DiagnosticSeverity.Error
      })
      d.setMessage(e.getMessage + "\n\n" + e.extraMessage)
      d
    }
    params.setDiagnostics(diags.asJava)
    client.publishDiagnostics(params)
  }
}

trait LSPClient extends LanguageClient
class LSPWebsocket[ClientClass <: LSPClient, ServerClass <: LSPServer[ClientClass]](clct : Class[ClientClass],svct : Class[ServerClass]) extends WebSocketEndpoint[ClientClass] {
  override def configure(builder: Launcher.Builder[ClientClass]): Unit = {
    val lsp = LSP.controller.extman.get(classOf[LSP[ClientClass,ServerClass,this.type]]).headOption.getOrElse({
      ???
    })
    val server = new AbstractLSPServer(lsp.newServer(WebSocketStyle),lsp) {}
    LSP.controller.extman.addExtension(server,Nil)
    builder.setLocalService(server).setRemoteInterface(clct)
  }

  override def connect(localServices: util.Collection[AnyRef], remoteProxy: ClientClass): Unit = {
    localServices.asScala.collect{case lca : LanguageClientAware => lca}.foreach(_.connect(remoteProxy))
  }
}

object LSP {
  private[lsp] var controller : Controller = null
}

class LSPServer[+ClientType <: LSPClient](clct : Class[ClientType]) {
  private var _log : (String,Option[String]) => Unit = null
  private var _controller : Controller = null
  private var _client : Option[ClientWrapper[LSPClient]] = None
  def controller = _controller
  protected def log(s : String, subgroup : Option[String] = None) = _log(s,subgroup)
  def client = _client.getOrElse({
    ???
  }).asInstanceOf[ClientWrapper[ClientType]]

  private[lsp] def init(__log: (String,Option[String]) => Unit, ctrl : Controller) = {
    _log = __log
    _controller = ctrl
  }
  private[lsp] def connectI(cl : LSPClient) = {
    _client = Some(new ClientWrapper(cl.asInstanceOf[ClientType]))
    connect
  }

  def connect: Unit = {}
  def initialize(params: InitializeParams,result : InitializeResult) = {}
  def shutdown : Any = {}
  def exit : Unit = {}
  def initialized(params: InitializedParams): Unit = {}
  def didChangeConfiguration(params: List[(String,List[(String,String)])]): Unit = {}
  def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {}
  def workspaceSymbol(params: WorkspaceSymbolParams): List[SymbolInformation] = Nil
  def executeCommand(params: ExecuteCommandParams): Any = {}
  def didOpen(params: DidOpenTextDocumentParams) : Unit = {}
  def didChange(params: DidChangeTextDocumentParams): Unit = {}
  def didClose(params: DidCloseTextDocumentParams): Unit = {}
  def didSave(docuri:String): Unit = {}
  def definition(position: TextDocumentPositionParams): (Option[List[Location]],Option[List[LocationLink]]) = (None,None)
  def typeDefinition(position: TextDocumentPositionParams): (Option[List[Location]],Option[List[LocationLink]]) = (None,None)
  def implementation(position: TextDocumentPositionParams): List[Location] = Nil
  def hover(params: HoverParams): Hover = null
  def documentHighlights(params: TextDocumentPositionParams): List[DocumentHighlight] = Nil
  def documentSymbol(params: DocumentSymbolParams): (Option[List[DocumentSymbol]], Option[List[SymbolInformation]]) = (None,None)
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
}

class AbstractLSPServer[A <: LSPClient, B <: LSPServer[A], C <: LSPWebsocket[A,B]](server : B,lsp:LSP[A,B,C]) extends LanguageClientAware with Extension {
  override def logPrefix: String = "lsp-" + lsp.prefix + "-server"

  override def start(args: List[String]): Unit = {
    super.start(args)
    server.init((s,g) => lsp.log(s,Some("-server" + g.map("-"+_).getOrElse(""))),controller)
  }
  private object Completable {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.compat.java8.FutureConverters._
    def apply[T](t : => T) = Future.apply(t).toJava.toCompletableFuture
    def list[T](t : => List[T]) : CompletableFuture[util.List[T]] = apply{ t.asJava }
  }

  private def toEither[A,B](p : (Option[A],Option[B])) = p match {
    case (Some(l),_) =>
      JEither.forLeft(l)
    case (_,Some(l)) =>
      JEither.forRight(l)
    case _ => null
  }

  private def normalizeUri(s:String) : String = s.replace("%3A",":")

  @JsonNotification("connect")
  override def connect(clientO: LanguageClient): Unit = {
    log("Connected: " + clientO.toString,Some("methodcall"))
    server.connectI(clientO.asInstanceOf[A])
  }

  @JsonRequest("initialize")
  def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    log("initialize", Some("methodcall"))
    Completable {
      val result = new InitializeResult(new ServerCapabilities)

      server.initialize(params,result)
      result
    }
  }

  @JsonRequest("shutdown")
  def shutdown(): CompletableFuture[Object] = {
    log("shutdown",Some("methodcall"))
    Completable{
      val r = server.shutdown
      this.controller.extman.removeExtension(this)
      r.asInstanceOf[Object]
    }
  }

  @JsonNotification("exit")
  def exit(): Unit = {
    log("exit",Some("methodcall"))
    server.exit
    this.controller.extman.removeExtension(this)
  }

  @JsonNotification("initialized")
  def initialized(params: InitializedParams): Unit = {
    log("initialized",Some("methodcall"))
    server.initialized(params)
  }

  @JsonNotification("workspace/didChangeConfiguration")
  def didChangeConfiguration(
                              params: DidChangeConfigurationParams
                            ): CompletableFuture[Unit] = {
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
        Completable{ server.didChangeConfiguration(ret) }
      case _ => ???
    }
  }

  @JsonNotification("workspace/didChangeWatchedFiles")
  def didChangeWatchedFiles(
                             params: DidChangeWatchedFilesParams
                           ): CompletableFuture[Unit] = {
    log("workspace/didChangeWatchedFiles: " + params.toString,Some("methodcall"))
    Completable{ server.didChangeWatchedFiles(params) }
  }


  @JsonRequest("workspace/symbol")
  def workspaceSymbol(
                       params: WorkspaceSymbolParams
                     ): CompletableFuture[util.List[SymbolInformation]] = {
    log("workspace/symbol: " + params.toString,Some("methodcall"))
    Completable { server.workspaceSymbol(params).asJava }
  }


  @JsonRequest("workspace/executeCommand")
  def executeCommand(params: ExecuteCommandParams): CompletableFuture[Object] = {
    log("workspace/executeCommand: " + params.toString,Some("methodcall"))
    Completable { server.executeCommand(params).asInstanceOf[Object] }
  }

  @JsonNotification("textDocument/didOpen")
  def didOpen(params: DidOpenTextDocumentParams): CompletableFuture[Unit] = {
    params.getTextDocument.setUri(normalizeUri(params.getTextDocument.getUri))
    log("textDocument/didOpen: " + params.getTextDocument.getUri + " (" + params.getTextDocument.getLanguageId + ")",Some("methodcall"))
    Completable { server.didOpen(params) }
  }

  @JsonNotification("textDocument/didChange")
  def didChange(
                 params: DidChangeTextDocumentParams
               ): CompletableFuture[Unit] = Completable {
    log("textDocument/didChange: " + params.getContentChanges.asScala.map(c =>
      "(" + c.getRange.getStart.getLine + "," + c.getRange.getStart.getCharacter + "),(" +
        c.getRange.getEnd.getLine + "," + c.getRange.getEnd.getCharacter + ")=\"" + c.getText + "\""
    ).mkString(" "),Some("methodcall"))
    server.didChange(params)
  }


  @JsonNotification("textDocument/didClose")
  def didClose(params: DidCloseTextDocumentParams): Unit = {
    log("textDocument/didClose: " + params.toString,Some("methodcall"))
    server.didClose(params)
  }


  @JsonNotification("textDocument/didSave")
  def didSave(params: DidSaveTextDocumentParams): CompletableFuture[Unit] = Completable {
    params.getTextDocument.setUri(normalizeUri(params.getTextDocument.getUri))
    log("textDocument/didSave: " + params.getTextDocument.getUri,Some("methodcall"))
    server.didSave(params.getTextDocument.getUri)
  }


  @JsonRequest("textDocument/definition")
  def definition(
                  position: TextDocumentPositionParams
                ): CompletableFuture[JEither[util.List[Location],util.List[LocationLink]]] = Completable {
    log("textDocument/definition: " + position.toString,Some("methodcall"))
    toEither{
      val ret = server.definition(position)
      (ret._1.map(_.asJava),ret._2.map(_.asJava))
    }.asInstanceOf[JEither[util.List[Location],util.List[LocationLink]]]
  }


  @JsonRequest("textDocument/typeDefinition")
  def typeDefinition(
                      position: TextDocumentPositionParams
                    ): CompletableFuture[JEither[util.List[Location],util.List[LocationLink]]] = Completable {
    log("textDocument/typeDefinition: " + position.toString,Some("methodcall"))
    toEither{
      val ret = server.typeDefinition(position)
      (ret._1.map(_.asJava),ret._2.map(_.asJava))
    }.asInstanceOf[JEither[util.List[Location],util.List[LocationLink]]]
  }


  @JsonRequest("textDocument/implementation")
  def implementation(
                      position: TextDocumentPositionParams
                    ): CompletableFuture[util.List[Location]] = Completable {
    log("textDocument/implementation: " + position.toString,Some("methodcall"))
    server.implementation(position).asJava
  }

  @JsonRequest("textDocument/hover")
  def hover(params: HoverParams): CompletableFuture[Hover] = Completable {
    params.getTextDocument.setUri(normalizeUri(params.getTextDocument.getUri))
    log("textDocument/hover: " + params.getTextDocument.getUri + ":(" + params.getPosition.getLine + "," + params.getPosition.getCharacter + ")",Some("methodcall"))
    server.hover(params)
  }

  @JsonRequest("textDocument/documentHighlight")
  def documentHighlights(
                          params: TextDocumentPositionParams
                        ): CompletableFuture[util.List[DocumentHighlight]] = Completable {
    log("textDocument/documentHighlight: " + params.toString,Some("methodcall"))
    server.documentHighlights(params).asJava
  }

  @JsonRequest("textDocument/documentSymbol")
  def documentSymbol(
                      params: DocumentSymbolParams
                    ): CompletableFuture[JEither[util.List[DocumentSymbol], util.List[SymbolInformation]]] = Completable {
    params.getTextDocument.setUri(normalizeUri(params.getTextDocument.getUri))
    log("textDocument/documentSymbol: " + params.getTextDocument.getUri,Some("methodcall"))
    toEither{
      val ret = server.documentSymbol(params)
      (ret._1,ret._2.map(_.asJava))
    }.asInstanceOf[JEither[util.List[DocumentSymbol], util.List[SymbolInformation]]]
  }

  @JsonRequest("textDocument/formatting")
  def formatting(
                  params: DocumentFormattingParams
                ): CompletableFuture[util.List[TextEdit]] = Completable {
    log("textDocument/formatting: " + params.toString,Some("methodcall"))
    server.formatting(params).asJava
  }


  @JsonRequest("textDocument/rename")
  def rename(
              params: RenameParams
            ): CompletableFuture[WorkspaceEdit] = Completable {
    log("textDocument/rename: " + params.toString,Some("methodcall"))
    server.rename(params)
  }

  @JsonRequest("textDocument/references")
  def references(
                  params: ReferenceParams
                ): CompletableFuture[util.List[Location]] = Completable {
    log("textDocument/reference: " + params.toString,Some("methodcall"))
    server.references(params).asJava
  }

  @JsonRequest("textDocument/completion")
  def completion(position: CompletionParams): CompletableFuture[JEither[util.List[CompletionItem], CompletionList]] = Completable {
    position.getTextDocument.setUri(normalizeUri(position.getTextDocument.getUri))
    log("textDocument/completion: " + position.getTextDocument.getUri + " at (" +
      position.getPosition.getLine + "," + position.getPosition.getCharacter + ")"
      ,Some("methodcall"))
    toEither{
      val ret = server.completion(position)
      (ret._1.map(_.asJava),ret._2)
    }.asInstanceOf[JEither[util.List[CompletionItem], CompletionList]]
  }

  @JsonRequest("textDocument/signatureHelp")
  def signatureHelp(
                     params: SignatureHelpParams
                   ): CompletableFuture[SignatureHelp] = Completable {
    log("textDocument/signatureHelp: " + params.toString,Some("methodcall"))
    server.signatureHelp(params)
  }

  @JsonRequest("textDocument/codeAction")
  def codeAction(
                  params: CodeActionParams
                ): CompletableFuture[util.List[CodeAction]] = Completable {
    log("textDocument/codeAction: " + params.toString,Some("methodcall"))
    server.codeAction(params).asJava
  }

  @JsonRequest("textDocument/codeLens")
  def codeLens(
                params: CodeLensParams
              ): CompletableFuture[util.List[CodeLens]] = Completable {
    log("textDocument/codeLens: " + params.toString,Some("methodcall"))
    server.codeLens(params).asJava
  }

  @JsonRequest("textDocument/foldingRange")
  def foldingRange(
                    params: FoldingRangeRequestParams
                  ): CompletableFuture[util.List[FoldingRange]] = Completable {
    params.getTextDocument.setUri(normalizeUri(params.getTextDocument.getUri))
    log("textDocument/foldingRange: " + params.getTextDocument.getUri,Some("methodcall"))
    server.foldingRange(params).asJava
  }

  @JsonRequest(value = "textDocument/semanticTokens/full")
  def semanticTokensFull(params: SemanticTokensParams) : CompletableFuture[SemanticTokens] = Completable {
    params.getTextDocument.setUri(normalizeUri(params.getTextDocument.getUri))
    log("textDocument/semanticTokens/full: " + params.getTextDocument.getUri,Some("methodcall"))
    server.semanticTokensFull(params)
  }

  @JsonRequest(value = "textDocument/semanticTokens/full/delta", useSegment = false)
  @ResponseJsonAdapter(classOf[SemanticTokensFullDeltaResponseAdapter])
  def semanticTokensFullDelta(params: SemanticTokensDeltaParams): CompletableFuture[JEither[SemanticTokens, SemanticTokensDelta]] = Completable {
    log("textDocument/semanticTokens/full/delta: " + params.toString,Some("methodcall"))
    toEither(server.semanticTokensFullDelta(params)).asInstanceOf[JEither[SemanticTokens, SemanticTokensDelta]]
  }

  @JsonRequest(value = "textDocument/semanticTokens/range", useSegment = false)
  def semanticTokensRange(params: SemanticTokensRangeParams): CompletableFuture[SemanticTokens] = Completable {
    log("textDocument/semanticTokens/range: " + params.toString,Some("methodcall"))
    server.semanticTokensRange(params)
  }
}