package info.kwarc.mmt.lsp

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.{ApplyWorkspaceEditParams, ApplyWorkspaceEditResponse, CodeAction, CodeActionParams, CodeLens, CodeLensParams, CompletionItem, CompletionList, CompletionOptions, CompletionParams, ConfigurationParams, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, DocumentHighlight, DocumentSymbol, DocumentSymbolParams, ExecuteCommandParams, FoldingRange, FoldingRangeRequestParams, Hover, HoverParams, InitializeParams, InitializeResult, InitializedParams, Location, LocationLink, MessageActionItem, MessageParams, MessageType, PublishDiagnosticsParams, ReferenceParams, RegistrationParams, RenameParams, SemanticHighlightingParams, SemanticHighlightingServerCapabilities, ServerCapabilities, ShowMessageRequestParams, SignatureHelp, SignatureHelpParams, SymbolInformation, TextDocumentPositionParams, TextDocumentSyncKind, TextEdit, UnregistrationParams, WorkspaceEdit, WorkspaceFolder, WorkspaceSymbolParams}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}
import org.eclipse.lsp4j.jsonrpc.{Endpoint, Launcher}
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import java.io.{BufferedWriter, FileWriter, InputStream, OutputStream, PrintWriter}
import java.net.ServerSocket
import java.util
import java.util.concurrent.ExecutionException
import java.util.logging.LogManager
import java.util.logging.Logger

import info.kwarc.mmt.api.frontend.{Controller, Extension, MMTConfig, ReportHandler, Run}
import info.kwarc.mmt.api.utils.File
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}

import scala.collection.JavaConverters._

object Local {
  @throws[InterruptedException]
  @throws[ExecutionException]
  def main(args: Array[String]): Unit = {
    LogManager.getLogManager.reset()
    val globalLogger = Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME)
    globalLogger.setLevel(java.util.logging.Level.OFF)
    // print("MMT Started")
    // startLocalServer(System.in, System.out)

    val controller = new Controller()
    val end = new ServerEndpoint
    controller.extman.addExtension(end,"local"::args.toList)
    val logFile = java.io.File.createTempFile("mmtlsp/log_","")
    val wr = new PrintWriter(new BufferedWriter(new FileWriter(logFile)))
    val launcher = new Launcher.Builder().setLocalService(end).setRemoteInterface(classOf[MMTClient]).setInput(System.in).
      setOutput(System.out).validateMessages(true).traceMessages(wr).create()
    val client = launcher.getRemoteProxy//.asInstanceOf[LanguageClient]
    end.connect(client)
    launcher.startListening()
  }
}

class Server extends Extension {
  override def logPrefix: String = "lsp"
  private var port = 5007
  lazy val ss = new ServerSocket(port)

  override def start(args: List[String]): Unit = {
    super.start(args)
    args match {
      case "port"::nr::_ =>
        port = nr.toInt
      case _ =>
    }
    thread.start()
  }

  lazy val thread = new Thread {
    override def run(): Unit = {
      while (true) try {
        log("Waiting for connection...")
        val conn = ss.accept()
        log("connected.")
        val logFile = java.io.File.createTempFile("mmtlsp/log_","")
        val wr = new PrintWriter(new BufferedWriter(new FileWriter(logFile)))
        log(logFile.toString)
        val end = new ServerEndpoint
        controller.extman.addExtension(end,Nil)
        val launcher = new Launcher.Builder().setLocalService(end).setRemoteInterface(classOf[MMTClient]).setInput(conn.getInputStream).
          setOutput(conn.getOutputStream).validateMessages(true).traceMessages(wr).create()

        // val launcher = LSPLauncher.createServerLauncher(server,conn.getInputStream,conn.getOutputStream,true,wr)
        end.connect(launcher.getRemoteProxy)
        launcher.startListening()
      }
    }
  }
}

class ServerEndpoint extends LanguageClientAware with Workspace with TextDocument with Extension {
  override def logPrefix: String = "lsp"
  private var _client : MMTClient = null
  def client = _client
  private var _path = ""

  override def start(args: List[String]): Unit = {
    super.start(args)
    args match {
      case "local" :: path :: Nil =>
        _path = path
      case _ =>
    }
  }

  override def log(s: => String, subgroup: Option[String]): Unit = {
    super.log(s, subgroup)
    if (_client != null) {
      val msg = if (subgroup.isDefined) subgroup.get + ": " + s else s
      _client.logMessage(new MessageParams(MessageType.Log,msg))
    }
  }

  protected object Completable {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.compat.java8.FutureConverters._

    def apply[T](t : => T) = Future.apply(t).toJava.toCompletableFuture
    def list[T](t : => List[T]) : CompletableFuture[util.List[T]] = apply{ t.asJava }
  }

  @JsonRequest("initialize")
  def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    log("Initialize",Some("methodcall"))
    Completable {
      val result = new InitializeResult(new ServerCapabilities)

      val completion = new CompletionOptions()
      completion.setTriggerCharacters(List("j").asJava)
      result.getCapabilities.setCompletionProvider(completion)

      val sh = new SemanticHighlightingServerCapabilities()
      sh.setScopes(Colors.scopes)
      result.getCapabilities.setSemanticHighlighting(sh)

      result.getCapabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)

      _report = Some(new ReportHandler("LSP " + _client.hashCode()) {
        override def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = {
          _client.logMessage(new MessageParams(if (group=="error") MessageType.Error else MessageType.Info,msgParts.mkString("\n")))
        }
      })
      controller.report.addHandler(_report.get)

      if (_path != "") initController

      result
    } //.completedFuture(new InitializeResult(new ServerCapabilities))
  }

  @JsonRequest("shutdown")
  def shutdown(): CompletableFuture[Object] = {
    log("shutdown",Some("methodcall"))
    Completable{
      exit()
      "exit"
    }
  }

  private var _report: Option[ReportHandler] = None

  @JsonNotification("connect")
  override def connect(clientO: LanguageClient): Unit = {
    log("Connected: " + _client.toString,Some("methodcall"))
    _client = clientO.asInstanceOf[MMTClient]
    _client.logMessage(new MessageParams(MessageType.Info,"Connected to MMT!"))
  }

  private def initController: Unit = {
    val home = File(_path)
    log("Project directory: " + File(_path).toString)

    /** Options */
    val mslf = home / "startup.msl"
    if (mslf.toJava.exists())
      controller.runMSLFile(mslf, None)
    else {
      mslf.createNewFile()
      File.append(mslf, "extension info.kwarc.mmt.odk.Plugin")
    }

    val rc = home / "mmtrc"
    if (!rc.toJava.exists()) {
      rc.createNewFile()
      File.append(rc, "\n", "#backends\n", "lmh .")
    }

    controller.loadConfig(MMTConfig.parse(rc), false)

    /** MathHub Folder */
    controller.setHome(home)
    controller.addArchive(home)
  }

  @JsonNotification("exit")
  def exit(): Unit = {
    log("Exit")
    _report.foreach(r => controller.report.removeHandler(r.id))
    this.controller.extman.removeExtension(this)
  }

  @JsonNotification("initialized")
  def initialized(params: InitializedParams): Unit = {
    log("Initialized",Some("methodcall"))
  }

  @JsonNotification("workspace/didChangeConfiguration")
  def didChangeConfiguration(
                              params: DidChangeConfigurationParams
                            ): CompletableFuture[Unit] = a_WSP2.didChangeConfiguration(params)

  @JsonNotification("workspace/didChangeWatchedFiles")
  def didChangeWatchedFiles(
                             params: DidChangeWatchedFilesParams
                           ): CompletableFuture[Unit] = a_WSP2.didChangeWatchedFiles(params)


  @JsonRequest("workspace/symbol")
  def workspaceSymbol(
                       params: WorkspaceSymbolParams
                     ): CompletableFuture[util.List[SymbolInformation]] = a_WSP.workspaceSymbol(params)


  @JsonRequest("workspace/executeCommand")
  def executeCommand(params: ExecuteCommandParams): CompletableFuture[Object] = a_WSP.executeCommand(params)

  @JsonNotification("textDocument/didOpen")
  def didOpen(params: DidOpenTextDocumentParams): CompletableFuture[Unit] = Completable { a_TD.didOpen(params) }

  @JsonNotification("textDocument/didChange")
  def didChange(
                 params: DidChangeTextDocumentParams
               ): CompletableFuture[Unit] = Completable { a_TD.didChange(params) }


  @JsonNotification("textDocument/didClose")
  def didClose(params: DidCloseTextDocumentParams): Unit = a_TD.didClose(params)


  @JsonNotification("textDocument/didSave")
  def didSave(params: DidSaveTextDocumentParams): CompletableFuture[Unit] = Completable { a_TD.didSave(params) }


  @JsonRequest("textDocument/definition")
  def definition(
                  position: TextDocumentPositionParams
                ): CompletableFuture[JEither[util.List[Location],util.List[LocationLink]]] = a_TD2.definition(position)


  @JsonRequest("textDocument/typeDefinition")
  def typeDefinition(
                      position: TextDocumentPositionParams
                    ): CompletableFuture[JEither[util.List[Location],util.List[LocationLink]]] = a_TD2.typeDefinition(position)


  @JsonRequest("textDocument/implementation")
  def implementation(
                      position: TextDocumentPositionParams
                    ): CompletableFuture[util.List[Location]] = a_TD2.implementation(position)

  @JsonRequest("textDocument/hover")
  def hover(params: HoverParams): CompletableFuture[Hover] = a_TD.hover(params)

  @JsonRequest("textDocument/documentHighlight")
  def documentHighlights(
                          params: TextDocumentPositionParams
                        ): CompletableFuture[util.List[DocumentHighlight]] = a_TD2.documentHighlights(params)

  @JsonRequest("textDocument/documentSymbol")
  def documentSymbol(
                      params: DocumentSymbolParams
                    ): CompletableFuture[
    JEither[util.List[DocumentSymbol], util.List[SymbolInformation]]
    ] = a_TD2.documentSymbol(params)

  @JsonRequest("textDocument/formatting")
  def formatting(
                  params: DocumentFormattingParams
                ): CompletableFuture[util.List[TextEdit]] = a_TD2.formatting(params)


  @JsonRequest("textDocument/rename")
  def rename(
              params: RenameParams
            ): CompletableFuture[WorkspaceEdit] = a_TD.rename(params)

  @JsonRequest("textDocument/references")
  def references(
                  params: ReferenceParams
                ): CompletableFuture[util.List[Location]] = a_TD2.references(params)

  @JsonRequest("textDocument/completion")
  def completion(position: CompletionParams): CompletableFuture[JEither[util.List[CompletionItem], CompletionList]] = a_TD.completion(position)

  @JsonRequest("textDocument/signatureHelp")
  def signatureHelp(
                     params: SignatureHelpParams
                   ): CompletableFuture[SignatureHelp] = a_TD.signatureHelp(params)

  @JsonRequest("textDocument/codeAction")
  def codeAction(
                  params: CodeActionParams
                ): CompletableFuture[util.List[CodeAction]] = a_TD2.codeAction(params)

  @JsonRequest("textDocument/codeLens")
  def codeLens(
                params: CodeLensParams
              ): CompletableFuture[util.List[CodeLens]] = a_TD2.codeLens(params)

  @JsonRequest("textDocument/foldingRange")
  def foldingRange(
                    params: FoldingRangeRequestParams
                  ): CompletableFuture[util.List[FoldingRange]] = a_TD.foldingRange(params)

}

trait MMTClient extends LanguageClient

object Colors {
  val keyword = 0
  val comment = 1
  val scomment = 2
  val name = 3
  val md = 4
  val dd = 5
  val od = 6
  val terminit = 7
  val termchecked = 8
  val termerrored = 9
  val notation = 10

  val scopesO = List("keyword.other","comment.block","comment.block.documentation","constant.language","mmt.md"
    ,"mmt.dd"
    ,"mmt.od"
    ,"mmt.terminit"
    ,"mmt.termchecked"
    ,"mmt.termerrored"
    ,"mmt.notation")
  val scopes = scopesO.map(_.split('.').toList.asJava).asJava
}