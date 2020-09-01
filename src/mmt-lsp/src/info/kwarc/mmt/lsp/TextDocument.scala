package info.kwarc.mmt.lsp

import java.util
import java.util.concurrent.CompletableFuture

import info.kwarc.mmt.api.parser.{SourcePosition, SourceRegion}
import info.kwarc.mmt.api.utils.{File, MMTSystem}
import org.eclipse.lsp4j.{CodeAction, CodeActionParams, CodeLens, CodeLensParams, CompletionItem, CompletionList, CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, DocumentHighlight, DocumentSymbol, DocumentSymbolParams, FoldingRange, FoldingRangeRequestParams, Hover, Location, LocationLink, ReferenceParams, RenameParams, SemanticHighlightingInformation, SemanticHighlightingParams, SignatureHelp, SymbolInformation, TextDocumentItem, TextDocumentPositionParams, TextEdit, VersionedTextDocumentIdentifier, WorkspaceEdit}
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.util.SemanticHighlightingTokens

import scala.collection.JavaConverters._


trait TextDocument { self : ServerEndpoint =>

  private var documents : List[LSPDocument] = Nil

  /*
  case class Highlight(offset:Int,length:Int,cls:Int) {
    lazy val (line,char) = toLC(offset)
    lazy val token = new SemanticHighlightingTokens.Token(char,length,cls)
  }
   */

    protected object a_TD extends TextDocumentService {
      override def didSave(params: DidSaveTextDocumentParams): Unit = {
        log("didSave",Some("methodcall-textDocument"))
      }


      override def didClose(params: DidCloseTextDocumentParams): Unit = {
        log("didClose",Some("methodcall-textDocument"))
      }

      override def didOpen(params: DidOpenTextDocumentParams): Unit = {
        log("didOpen",Some("methodcall-textDocument"))
        val document = params.getTextDocument
        val file = File(document.getUri.drop(7))
        log("File: " + file,Some("didOpen"))
        documents.find(_.uri == document.getUri).getOrElse {
          val d = new LSPDocument(document.getUri,self.client,self.controller)
          documents ::= d
          d.setVersion(document.getVersion)
          d.init(document.getText)
        }
        // log("document: " + document,Some("didOpen"))
        // val ls = List(Highlight(0,0,"namespace".length,Colors.keyword))//List(new SemanticHighlightingTokens.Token(0,"namespace".length,0))
        // semanticHighlight(document,ls)
      }

      override def didChange(
                              params: DidChangeTextDocumentParams
                            ): Unit = {
        log("didChange",Some("methodcall-textDocument"))
        val uri = params.getTextDocument.getUri
        val file = File(uri.drop(7))
        // log("File: " + file,Some("didChange"))
        val doc = documents.find(_.uri == uri).getOrElse {
          log("Document not found: " + uri)
          return
        }
        params.getContentChanges.asScala.foreach {change =>
          doc.update(change.getRange,change.getText)
        }
        /*
        val changes = params.getContentChanges
        changes.asScala foreach { change =>
          val range = Range(change.getRange,file)
          range match {
            case p@Position(_,_,_) => // Insertion
              val inserted = change.getText
              log("Inserted \"" + inserted + "\" at Position " + p,Some("didChange"))
            case r@Range(_,_) =>
              val newText = change.getText
              log("Changed " + r + " to \"" + newText + "\"",Some("didChange"))
          }
        }
         */
      }

      lazy val completionls : CompletionList = {
        val cl = new CompletionList()
        val pairstrings = (MMTSystem.getResourceAsString("unicode/unicode-latex-map") + "\n" +
          MMTSystem.getResourceAsString("unicode/unicode-ascii-map")).split("\n")
        val pairs: List[(String, String)] = pairstrings.collect { case s if s.nonEmpty && !s.trim.startsWith("//") =>
          val ps = s.splitAt(s.lastIndexOf('|'))
          (ps._1.trim,ps._2.trim.drop(1))
        }.toList
        val list = pairs.map {
          case (a,b) =>
            val item = new CompletionItem()
            item.setFilterText(a)
            item.setInsertText(b)
            item.setLabel(a + " | " + b)
            item
        }
        cl.setItems(list.asJava)
        cl
      }

      override def completion(position: CompletionParams): CompletableFuture[JEither[util.List[CompletionItem], CompletionList]] = Completable {
        log("completion",Some("methodcall-textDocument"))
        log("context: " + position.getContext,Some("completion"))
        JEither.forRight(completionls)
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