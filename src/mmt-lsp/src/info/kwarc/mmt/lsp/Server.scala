package info.kwarc.mmt.lsp

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.{ApplyWorkspaceEditParams, ApplyWorkspaceEditResponse, CodeAction, CodeActionParams, CodeLens, CodeLensParams, CompletionList, CompletionParams, ConfigurationParams, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, DocumentHighlight, DocumentSymbol, DocumentSymbolParams, ExecuteCommandParams, FoldingRange, FoldingRangeRequestParams, Hover, InitializeParams, InitializeResult, InitializedParams, Location, MessageActionItem, MessageParams, MessageType, PublishDiagnosticsParams, ReferenceParams, RegistrationParams, RenameParams, SemanticHighlightingParams, ServerCapabilities, ShowMessageRequestParams, SignatureHelp, SymbolInformation, TextDocumentPositionParams, TextEdit, UnregistrationParams, WorkspaceEdit, WorkspaceFolder, WorkspaceSymbolParams}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}
import org.eclipse.lsp4j.jsonrpc.{Endpoint, Launcher}
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import java.io.InputStream
import java.io.OutputStream
import java.util
import java.util.concurrent.ExecutionException
import java.util.logging.LogManager
import java.util.logging.Logger

import org.eclipse.lsp4j.jsonrpc.services.{JsonDelegate, JsonNotification, JsonRequest}
import org.eclipse.lsp4j.websocket.{WebSocketEndpoint, WebSocketLauncherBuilder}

object Main {
  @throws[InterruptedException]
  @throws[ExecutionException]
  def main(args: Array[String]): Unit = {
    LogManager.getLogManager.reset()
    val globalLogger = Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME)
    globalLogger.setLevel(java.util.logging.Level.OFF)
    println("MMT Started")
    startLocalServer(System.in, System.out)
  }

  @throws[InterruptedException]
  @throws[ExecutionException]
  def startLocalServer(in: InputStream, out: OutputStream): Unit = {
    val server = new Server
    val launcher = Launcher.createLauncher(server, classOf[MMTClient], in, out)
    val client = launcher.getRemoteProxy//.asInstanceOf[LanguageClient]
    server.connect(client)
    val startListening = launcher.startListening
    println("Server started")
    startListening.get
  }

}

object Remote {
  @throws[InterruptedException]
  @throws[ExecutionException]
  def main(args: Array[String]): Unit = {
    LogManager.getLogManager.reset()
    val globalLogger = Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME)
    globalLogger.setLevel(java.util.logging.Level.OFF)
    println("MMT Started")
    startRemoteServer
  }

  def startRemoteServer: Unit = {
    val server = new Server
    val launcher = new MMTEndpoint
  }

  class MMTEndpoint extends WebSocketEndpoint[MMTClient] {
    override def configure(builder: Launcher.Builder[MMTClient]): Unit = {
      builder.setInput(System.in)
      builder.setOutput(System.out)
    }

    override def connect(localServices: util.Collection[Object], remoteProxy: MMTClient): Unit = {
      ???
    }
  }
}

class Server extends LanguageClientAware with Workspace with TextDocument {

  object Completable {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.compat.java8.FutureConverters._
    import scala.collection.JavaConverters._

    def apply[T](t : => T) = Future.apply(t).toJava.toCompletableFuture
    def list[T](t : => List[T]) : CompletableFuture[util.List[T]] = apply{ t.asJava }
  }

  @JsonRequest("initialize")
  def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    println("Initialize test")
    Completable {
      new InitializeResult(new ServerCapabilities)
    } //.completedFuture(new InitializeResult(new ServerCapabilities))
  }

  /* {_ =>
    new InitializeResult(new ServerCapabilities())
  }

   */

  @JsonRequest("shutdown")
  def shutdown(): CompletableFuture[Object] = {
    println("shutdown")

    Completable{ "shutdown"}
  }



  @JsonNotification("workspace/didChangeConfiguration")
  def didChangeConfiguration(
                              params: DidChangeConfigurationParams
                            ): CompletableFuture[Unit] = Completable {}

  @JsonNotification("workspace/didChangeWatchedFiles")
  def didChangeWatchedFiles(
                             params: DidChangeWatchedFilesParams
                           ): CompletableFuture[Unit] = Completable {}


  @JsonRequest("workspace/symbol")
  def workspaceSymbol(
                       params: WorkspaceSymbolParams
                     ): CompletableFuture[util.List[SymbolInformation]] = Completable.list(Nil)


  @JsonRequest("workspace/executeCommand")
  def executeCommand(params: ExecuteCommandParams): CompletableFuture[Object] = Completable { null }

  @JsonNotification("textDocument/didOpen")
  def didOpen(params: DidOpenTextDocumentParams): CompletableFuture[Unit] = Completable {}

  @JsonNotification("textDocument/didChange")
  def didChange(
                 params: DidChangeTextDocumentParams
               ): CompletableFuture[Unit] = Completable {}


  @JsonNotification("textDocument/didClose")
  def didClose(params: DidCloseTextDocumentParams): Unit = {}


  @JsonNotification("textDocument/didSave")
  def didSave(params: DidSaveTextDocumentParams): CompletableFuture[Unit] = Completable {}


  @JsonRequest("textDocument/definition")
  def definition(
                  position: TextDocumentPositionParams
                ): CompletableFuture[util.List[Location]] = Completable.list(Nil)


  @JsonRequest("textDocument/typeDefinition")
  def typeDefinition(
                      position: TextDocumentPositionParams
                    ): CompletableFuture[util.List[Location]] = Completable.list(Nil)


  @JsonRequest("textDocument/implementation")
  def implementation(
                      position: TextDocumentPositionParams
                    ): CompletableFuture[util.List[Location]] = Completable.list(Nil)


  @JsonRequest("textDocument/hover")
  def hover(params: TextDocumentPositionParams): CompletableFuture[Hover] = Completable {null}


  @JsonRequest("textDocument/documentHighlight")
  def documentHighlights(
                          params: TextDocumentPositionParams
                        ): CompletableFuture[util.List[DocumentHighlight]] = Completable.list(Nil)

  @JsonRequest("textDocument/documentSymbol")
  def documentSymbol(
                      params: DocumentSymbolParams
                    ): CompletableFuture[
    JEither[util.List[DocumentSymbol], util.List[SymbolInformation]]
    ] = Completable {null}


  @JsonRequest("textDocument/formatting")
  def formatting(
                  params: DocumentFormattingParams
                ): CompletableFuture[util.List[TextEdit]] = Completable.list(Nil)

  @JsonRequest("textDocument/rename")
  def rename(
              params: RenameParams
            ): CompletableFuture[WorkspaceEdit] = Completable {null}


  @JsonRequest("textDocument/references")
  def references(
                  params: ReferenceParams
                ): CompletableFuture[util.List[Location]] = Completable.list(Nil)


  @JsonRequest("textDocument/completion")
  def completion(params: CompletionParams): CompletableFuture[CompletionList] = Completable { null }


  @JsonRequest("textDocument/signatureHelp")
  def signatureHelp(
                     params: TextDocumentPositionParams
                   ): CompletableFuture[SignatureHelp] = Completable { null }


  @JsonRequest("textDocument/codeAction")
  def codeAction(
                  params: CodeActionParams
                ): CompletableFuture[util.List[CodeAction]] = Completable.list(Nil)


  @JsonRequest("textDocument/codeLens")
  def codeLens(
                params: CodeLensParams
              ): CompletableFuture[util.List[CodeLens]] = Completable.list(Nil)


  @JsonRequest("textDocument/foldingRange")
  def foldingRange(
                    params: FoldingRangeRequestParams
                  ): CompletableFuture[util.List[FoldingRange]] = Completable.list(Nil)

  @JsonNotification("connect")
  // @JsonRequest("connect")
  override def connect(client: LanguageClient): Unit = {
    println("Connected: " + client.toString)
    client.logMessage(new MessageParams(MessageType.Info,"Connected Info"))
  }


  @JsonNotification("exit")
  def exit(): Unit = {
    println("Exit")
  }

  @JsonNotification("initialized")
  def initialized(params: InitializedParams): Unit = {
    println("Initialized")
  }

}

trait MMTClient extends LanguageClient

/*
trait MMTClient {
  @JsonRequest("workspace/applyEdit") def applyEdit(params: ApplyWorkspaceEditParams): CompletableFuture[ApplyWorkspaceEditResponse] = throw new UnsupportedOperationException

  @JsonRequest("client/registerCapability") def registerCapability(params: RegistrationParams): CompletableFuture[Void] = throw new UnsupportedOperationException

  @JsonRequest("client/unregisterCapability") def unregisterCapability(params: UnregistrationParams): CompletableFuture[Void] = throw new UnsupportedOperationException

  @JsonNotification("telemetry/event") def telemetryEvent(var1: Any): Unit

  @JsonNotification("textDocument/publishDiagnostics") def publishDiagnostics(var1: PublishDiagnosticsParams): Unit

  @JsonNotification("window/showMessage") def showMessage(var1: MessageParams): Unit

  @JsonRequest("window/showMessageRequest") def showMessageRequest(var1: ShowMessageRequestParams): CompletableFuture[MessageActionItem]

  @JsonNotification("window/logMessage") def logMessage(var1: MessageParams): Unit

  @JsonRequest("workspace/workspaceFolders") def workspaceFolders: CompletableFuture[util.List[WorkspaceFolder]] = throw new UnsupportedOperationException

  @JsonRequest("workspace/configuration") def configuration(configurationParams: ConfigurationParams): CompletableFuture[util.List[Any]] = throw new UnsupportedOperationException

  @JsonNotification("textDocument/semanticHighlighting")
  def semanticHighlighting(params: SemanticHighlightingParams): Unit = {
    throw new UnsupportedOperationException
  }
}
*/