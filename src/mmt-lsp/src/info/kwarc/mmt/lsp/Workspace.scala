package info.kwarc.mmt.lsp

import java.util
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeWatchedFilesParams, ExecuteCommandParams, SymbolInformation, WorkspaceSymbolParams}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}

trait Workspace { self : Server =>
  /*
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

   */
}
