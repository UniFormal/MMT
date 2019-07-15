package info.kwarc.mmt.lsp

import java.util
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.{CodeAction, CodeActionParams, CodeLens, CodeLensParams, CompletionList, CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, DocumentHighlight, DocumentSymbol, DocumentSymbolParams, FoldingRange, FoldingRangeRequestParams, Hover, Location, LocationLink, ReferenceParams, RenameParams, SignatureHelp, SymbolInformation, TextDocumentPositionParams, TextEdit, WorkspaceEdit}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import org.eclipse.lsp4j.services.TextDocumentService

trait TextDocument { self : Server =>
    protected object a_TD extends TextDocumentService {
      override def didSave(params: DidSaveTextDocumentParams): Unit = {
        log("didSave",Some("methodcall-textDocument"))
      }

      override def didClose(params: DidCloseTextDocumentParams): Unit = {
        log("didClose",Some("methodcall-textDocument"))
      }

      override def didOpen(params: DidOpenTextDocumentParams): Unit = {
        log("didOpen",Some("methodcall-textDocument"))
      }

      override def didChange(
                              params: DidChangeTextDocumentParams
                            ): Unit = {
        log("didChange",Some("methodcall-textDocument"))
      }
    }

  protected object a_TD2 {

    def definition(
                             position: TextDocumentPositionParams
                           ): CompletableFuture[JEither[util.List[Location],util.List[LocationLink]]] = {
      log("definition",Some("methodcall-textDocument"))
      null
    }

    def typeDefinition(
                                 position: TextDocumentPositionParams
                               ): CompletableFuture[JEither[util.List[Location],util.List[LocationLink]]] = {
      log("typeDefinition",Some("methodcall-textDocument"))
      null
    }

    def implementation(
                                 position: TextDocumentPositionParams
                               ): CompletableFuture[util.List[Location]] = {
      log("implementation",Some("methodcall-textDocument"))
      Completable.list(Nil)
    }

    def hover(params: TextDocumentPositionParams): CompletableFuture[Hover] = Completable {
      log("hover",Some("methodcall-textDocument"))
      null
    }

    def documentHighlights(
                            params: TextDocumentPositionParams
                          ): CompletableFuture[util.List[DocumentHighlight]] = {
      log("documentHighlights",Some("methodcall-textDocument"))
      Completable.list(Nil)
    }

    def documentSymbol(
                                 params: DocumentSymbolParams
                               ): CompletableFuture[JEither[util.List[DocumentSymbol], util.List[SymbolInformation]]] = Completable {
      log("documentSymbol",Some("methodcall-textDocument"))
      null
    }

    def formatting(
                             params: DocumentFormattingParams
                           ): CompletableFuture[util.List[TextEdit]] = {
      log("formatting",Some("methodcall-textDocument"))
      Completable.list(Nil)
    }

    def rename(
                         params: RenameParams
                       ): CompletableFuture[WorkspaceEdit] = Completable {
      log("rename",Some("methodcall-textDocument"))
      null
    }

    def references(
                             params: ReferenceParams
                           ): CompletableFuture[util.List[Location]] = {
      log("references",Some("methodcall-textDocument"))
      Completable.list(Nil)
    }

    def completion(params: CompletionParams): CompletableFuture[CompletionList] = Completable {
      log("completion",Some("methodcall-textDocument"))
      null
    }

    def signatureHelp(
                                params: TextDocumentPositionParams
                              ): CompletableFuture[SignatureHelp] = Completable {
      log("signatureHelp",Some("methodcall-textDocument"))
      null
    }

    def codeAction(
                             params: CodeActionParams
                           ): CompletableFuture[util.List[CodeAction]] = {
      log("codeAction",Some("methodcall-textDocument"))
      Completable.list(Nil)
    }

    def codeLens(
                           params: CodeLensParams
                         ): CompletableFuture[util.List[CodeLens]] = {
      log("codeLens",Some("methodcall-textDocument"))
      Completable.list(Nil)
    }

    def foldingRange(
                               params: FoldingRangeRequestParams
                             ): CompletableFuture[util.List[FoldingRange]] = {
      log("foldingRange",Some("methodcall-textDocument"))
      Completable.list(Nil)
    }

  }
}
