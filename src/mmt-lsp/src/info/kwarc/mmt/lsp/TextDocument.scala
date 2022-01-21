package info.kwarc.mmt.lsp

import info.kwarc.mmt.api.parser.SourceRegion
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{CompletionItem, CompletionItemKind, CompletionItemTag, CompletionList, CompletionOptions, CompletionParams, DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbol, DocumentSymbolOptions, DocumentSymbolParams, FoldingRange, FoldingRangeRequestParams, Hover, HoverOptions, HoverParams, InitializeParams, InitializeResult, InsertTextFormat, InsertTextMode, MarkupContent, MarkupKind, Position, SemanticTokens, SemanticTokensLegend, SemanticTokensParams, SemanticTokensWithRegistrationOptions, SymbolInformation, TextDocumentSyncKind, TextEdit, VersionedTextDocumentIdentifier, WorkDoneProgressCreateParams, WorkspaceEdit, WorkspaceSymbolOptions}

import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable

trait TextDocumentServer[ClientType <: LSPClient,DocumentType <: LSPDocument[ClientType,LSPServer[ClientType]]] { self : LSPServer[ClientType] =>

  def newDocument(uri : String) : DocumentType
  protected val documents = mutable.Map.empty[String,DocumentType]

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    val d = documents.synchronized{documents.getOrElseUpdate(document.getUri,newDocument(document.getUri))}
    d.init(document.getText)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    val doc = documents.synchronized{documents.getOrElse(uri,{
      log("Document not found: " + uri)
      return
    })}
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
      if (doc != null) item.setDocumentation(new MarkupContent(MarkupKind.MARKDOWN,doc))
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

trait WithAnnotations[ClientType <: LSPClient,DocumentType <: AnnotatedDocument[ClientType,LSPServer[ClientType]]]
  extends LSPServer[ClientType] with TextDocumentServer[ClientType,DocumentType] {

  val scopes : List[String]
  val modifiers : List[String]

  override def initialize(params: InitializeParams, result: InitializeResult): Unit = {
    super.initialize(params, result)
    val sh = new SemanticTokensWithRegistrationOptions(new SemanticTokensLegend(scopes.asJava,modifiers.asJava))
    sh.setFull(true)
    result.getCapabilities.setSemanticTokensProvider(sh)
    result.getCapabilities.setHoverProvider(true)
    result.getCapabilities.setDocumentSymbolProvider(true)
    result.getCapabilities.setWorkspaceSymbolProvider(true)
    result.getCapabilities.setFoldingRangeProvider(true)
  }


  override def semanticTokensFull(params: SemanticTokensParams): SemanticTokens = {
    documents.synchronized{documents.get(params.getTextDocument.getUri)} match {
      case Some(doc : DocumentType) =>
        doc.synchronized{ doc.Annotations.getAll.flatMap(_.getHighlights) match {
          case Nil =>
            null
          case ls =>
            val ret = computeHighlights(ls.map(is => Highlight(is._1,is._2,is._3,is._4,is._5)))
            log("textDocument/semanticTokens/full return: " + ret.length)
            new SemanticTokens(ret.asJava)
        }}
      case _ => null
    }
  }

  override def hover(params: HoverParams): Hover = {
    documents.synchronized{documents.get(params.getTextDocument.getUri)} match {
      case Some(doc : DocumentType) =>
        doc.synchronized{ doc.Annotations.getAll.flatMap(_.getHovers) match {
          case Nil =>
            null
          case ls =>
            val pos = LSPDocument.toOffset(params.getPosition.getLine,params.getPosition.getCharacter,doc.doctext)
            val fOs = ls.collect{case (s,e,f) if s <= pos && pos <= e => (e-s,f)}
            fOs.sortBy(_._1).headOption.map { case (_,f) =>
              val h = new Hover()
              h.setContents(new MarkupContent(MarkupKind.MARKDOWN,f(())))
              h
            }.orNull
        }}
      case _ => null
    }
  }

  override def foldingRange(params: FoldingRangeRequestParams): List[FoldingRange] = {
    documents.synchronized{documents.get(params.getTextDocument.getUri)} match {
      case Some(doc: DocumentType) =>
        doc.synchronized {
          val annots = doc.Annotations.getAll
          if (annots.isEmpty) null
          else {
            annots.collect{
              case a if a.foldable =>
                val (start,_) = LSPDocument.toLC(a.offset,doc.doctext)
                val (end,_) = LSPDocument.toLC(a.end,doc.doctext)
                new FoldingRange(start,end)
            }
          }
        }
      case _ => Nil
    }
  }

  override def documentSymbol(params: DocumentSymbolParams): (Option[List[DocumentSymbol]], Option[List[SymbolInformation]]) = {
    documents.synchronized{documents.get(params.getTextDocument.getUri)} match {
      case Some(doc: DocumentType) =>
        doc.synchronized {
          val annots = doc.Annotations.getAll
          if (annots.isEmpty) (None,None)
          else {
            var dones: List[doc.Annotation] = Nil

            def toSymbol(a: doc.Annotation) = {
              val (sl, sc) = LSPDocument.toLC(a.offset, doc.doctext)
              val (el, ec) = LSPDocument.toLC(a.end, doc.doctext)
              val range = new lsp4j.Range(new Position(sl, sc), new Position(el, ec))
              new DocumentSymbol(a.symbolname, a.symbolkind, range, range, "")
            }

            def inSymbol(a: DocumentSymbol, b: doc.Annotation): Unit = {
              dones ::= b
              b.children.foreach { c =>
                if (c.symbolkind != null && c.symbolname != "") {
                  val n = toSymbol(c)
                  if (a.getChildren != null)
                    a.setChildren((a.getChildren.asScala.toList ::: n :: Nil).asJava)
                  else a.setChildren(List(n).asJava)
                  inSymbol(n, c)
                } else inSymbol(a, c)
              }
            }

            val ret = annots.flatMap {
              case a if !dones.contains(a) && a.symbolkind != null && a.symbolname != "" =>
                val s = toSymbol(a)
                inSymbol(s, a)
                List(s)
              case _ => Nil
            }
            (Some(ret), None)
          }
        }
      case _ => (None,None)
    }
  }

  private case class Highlight(line: Int,char:Int,length:Int,cls:Int,mod:List[Int]) {
    def <=(o : Highlight) = {
      if (line < o.line) true else if (line == o.line) {
        char <= o.char
      } else false
    }
  }

  private def computeHighlights(highs : List[Highlight]): List[Integer] = {
    var lines = highs.sortBy(_.line)
    var result : List[List[Highlight]] = Nil
    while (lines.nonEmpty) {
      var index = lines.indexWhere(_.line != lines.head.line)
      if (index == -1) index = lines.length
      val split = lines.take(index)
      lines = lines.drop(index)
      result ::= split.sortBy(_.char)
    }
    var lastline = 0
    var lastchar = 0
    result.flatten.sortWith((p,q) => p<=q).flatMap {
      case Highlight(line,char,length,cls,mod) =>
        val nchar = if (line == lastline) (char - lastchar) else char
        val r = List((line-lastline).asInstanceOf[java.lang.Integer],nchar.asInstanceOf[java.lang.Integer],length.asInstanceOf[java.lang.Integer],cls.asInstanceOf[java.lang.Integer],{
          mod.filter(_ != -1).distinct.foldLeft(0)((r,i) => r + math.pow(2,i).toInt).asInstanceOf[java.lang.Integer]
        })
        lastline = line
        lastchar = char
        r
    }
  }


}