package info.kwarc.mmt.lsp

import org.eclipse.lsp4j.{CompletionItem, CompletionItemKind, CompletionItemTag, CompletionList, CompletionOptions, CompletionParams, DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams, InitializeResult, InsertTextFormat, InsertTextMode, TextDocumentSyncKind, TextEdit, VersionedTextDocumentIdentifier, WorkspaceEdit}


import scala.collection.JavaConverters._
import scala.collection.mutable

trait TextDocumentServer[ClientType <: LSPClient,DocumentType <: LSPDocument[ClientType,LSPServer[ClientType]]] { self : LSPServer[ClientType] =>

  def newDocument(uri : String) : DocumentType
  protected val documents = mutable.Map.empty[String,DocumentType]

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    documents.getOrElseUpdate(document.getUri,{
      val d = newDocument(document.getUri)
      d.init(document.getText)
      d
    })
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    val doc = documents.getOrElse(uri,{
      log("Document not found: " + uri)
      return
    })
    params.getContentChanges.asScala.foreach {change =>
      doc.update(change.getRange,change.getText)
    }
  }

  override def initialize(params: InitializeParams, result: InitializeResult): Unit = {
    result.getCapabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
  }

    protected object a_TD {


/*

      override def semanticTokensFull(params: SemanticTokensParams): CompletableFuture[SemanticTokens] = Completable {
        log("semanticTokensFull",Some("methodcall-textDocument"))
        log("doc: " + params.getTextDocument,Some("completion"))


        val uri = params.getTextDocument.getUri
        // log("File: " + file,Some("didChange"))
        val doc = documents.find(_.uri == uri).getOrElse {
          log("Document not found: " + uri)
          return Completable(new SemanticTokens(Nil.asJava))
        }
        val ls = doc.highlight.asJava.asInstanceOf[util.List[Integer]]
        //startTimer
        new SemanticTokens(ls)//(List(1,2,3).asJava)
      }

      private def startTimer: Unit = Future {
        Thread.sleep(20000)
        client.refreshSemanticTokens()
      }(scala.concurrent.ExecutionContext.global)

      override def semanticTokensFullDelta(params: SemanticTokensDeltaParams): CompletableFuture[JEither[SemanticTokens, SemanticTokensDelta]] = Completable {
        log("semanticTokensFullDelta",Some("methodcall-textDocument"))
        log("doc: " + params.getTextDocument,Some("completion"))
        JEither.forRight(new SemanticTokensDelta(Nil.asJava))
      }

      override def semanticTokensRange(params: SemanticTokensRangeParams): CompletableFuture[SemanticTokens] = Completable {
        log("semanticTokensRange",Some("methodcall-textDocument"))
        log("doc: " + params.getTextDocument,Some("completion"))
        new SemanticTokens(Nil.asJava)
      }

 */
    }

}

trait WithAutocomplete[ClientType <: LSPClient] extends LSPServer[ClientType] {
  val triggerChar : Char = '\\'

  case class Completion(
                         label : String,
                         filterText : String = null,
                         insertText : String = null,
                         kind : CompletionItemKind = CompletionItemKind.Text,
                         detail : String = null,
                         doc : String = null,
                         deprecated : Boolean = false,
                         preselect : Boolean = false,
                         sortText : String =null,
                         insertTextFormat : InsertTextFormat = null,
                         insertTextMode : InsertTextMode = null
                       ) {
    def toLSP = {
      val item = new CompletionItem()
      item.setLabel(label)
      item.setKind(kind)
      if (detail != null) item.setDetail(detail)
      if (doc != null) item.setDocumentation(doc)
      if (deprecated) item.setTags(List(CompletionItemTag.Deprecated).asJava)
      item.setPreselect(preselect)
      if (sortText != null) item.setSortText(sortText)
      if (filterText == null) item.setFilterText(label) else item.setFilterText(filterText)
      if (insertText == null) item.setInsertText(label) else item.setInsertText(insertText)
      if (insertTextFormat != null) item.setInsertTextFormat(insertTextFormat)
      if (insertTextMode != null) item.setInsertTextMode(insertTextMode)
      // TODO textEdit
      // TODO additionalTextEdit
      // TODO command
      // TODO data
      item
    }
  }

  def completion(doc : String, line : Int, char : Int) : List[Completion]

  override def completion(position: CompletionParams): (Option[List[CompletionItem]], Option[CompletionList]) = {
    val ls = completion(position.getTextDocument.getUri,position.getPosition.getLine,position.getPosition.getCharacter)
    if (ls.isEmpty) (None,None) else {
      val ret = new CompletionList()
      ret.setItems(ls.map(_.toLSP).asJava)
      (None,Some(ret))
    }
  }

  override def initialize(params: InitializeParams, result: InitializeResult): Unit = {
    super.initialize(params, result)
    val completion = new CompletionOptions()
    completion.setTriggerCharacters(List(triggerChar.toString).asJava)
    result.getCapabilities.setCompletionProvider(completion)
  }

}