package info.kwarc.mmt.lsp

import java.util
import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.{CodeAction, CodeActionParams, CodeLens, CodeLensParams, CompletionList, CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, DocumentHighlight, DocumentSymbol, DocumentSymbolParams, FoldingRange, FoldingRangeRequestParams, Hover, Location, ReferenceParams, RenameParams, SignatureHelp, SymbolInformation, TextDocumentPositionParams, TextEdit, WorkspaceEdit}
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}

import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}

trait TextDocument { self : Server =>
  /*
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

   */
}
