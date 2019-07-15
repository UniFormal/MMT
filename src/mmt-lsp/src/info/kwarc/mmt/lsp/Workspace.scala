package info.kwarc.mmt.lsp

import java.util
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeWatchedFilesParams, ExecuteCommandParams, SymbolInformation, WorkspaceSymbolParams}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.services.WorkspaceService

trait Workspace { self : Server =>
  protected object a_WSP extends WorkspaceService {
    override def didChangeWatchedFiles(didChangeWatchedFilesParams: DidChangeWatchedFilesParams): Unit = {}

    override def didChangeConfiguration(didChangeConfigurationParams: DidChangeConfigurationParams): Unit = {}

    def workspaceSymbol(
                         params: WorkspaceSymbolParams
                       ): CompletableFuture[util.List[SymbolInformation]] = Completable.list(Nil)

    override def executeCommand(params: ExecuteCommandParams): CompletableFuture[Object] = Completable {null}

    // def executeCommand(params: ExecuteCommandParams): CompletableFuture[Object] = Completable { null }
  }
  protected object a_WSP2 {
    def didChangeConfiguration(
                                params: DidChangeConfigurationParams
                              ): CompletableFuture[Unit] = Completable {a_WSP.didChangeConfiguration(params)}

    def didChangeWatchedFiles(
                               params: DidChangeWatchedFilesParams
                             ): CompletableFuture[Unit] = Completable {a_WSP.didChangeWatchedFiles(params)}
  }
}
