package info.kwarc.mmt.lsp

import info.kwarc.mmt.api.parser.SourceRegion
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{CodeLens, CodeLensOptions, CodeLensParams, Command, CompletionItem, CompletionItemKind, CompletionItemTag, CompletionList, CompletionOptions, CompletionParams, DeclarationParams, DefinitionParams, DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentLinkOptions, DocumentSymbol, DocumentSymbolOptions, DocumentSymbolParams, FoldingRange, FoldingRangeRequestParams, Hover, HoverOptions, HoverParams, ImplementationParams, InitializeParams, InitializeResult, InlayHint, InlayHintParams, InsertTextFormat, InsertTextMode, Location, LocationLink, MarkupContent, MarkupKind, Position, SemanticTokens, SemanticTokensLegend, SemanticTokensParams, SemanticTokensWithRegistrationOptions, SymbolInformation, TextDocumentIdentifier, TextDocumentSyncKind, TextEdit, VersionedTextDocumentIdentifier, WorkDoneProgressCreateParams, WorkspaceEdit, WorkspaceSymbolOptions}

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
    result.getCapabilities.setInlayHintProvider(true)
    //result.getCapabilities.setDocumentLinkProvider(new DocumentLinkOptions(true))
    result.getCapabilities.setImplementationProvider(true)
    result.getCapabilities.setDeclarationProvider(true)
    result.getCapabilities.setDefinitionProvider(true)
    result.getCapabilities.setCodeLensProvider(new CodeLensOptions(true))
  }

  def getAnnotations(doc : TextDocumentIdentifier, range: lsp4j.Range) = {
    documents.synchronized{documents.get(doc.getUri)} match {
      case Some(doc:DocumentType) =>
        val start = range.getStart
        val end = range.getEnd
        val soff = LSPDocument.toOffset(start.getLine,start.getCharacter,doc.doctext)
        val eoff = LSPDocument.toOffset(end.getLine,end.getCharacter,doc.doctext)
        (Some(doc),doc.synchronized{ doc.Annotations.getAll.collect{
          case a if soff <= a.offset && a.offset + a.length <= eoff => a
        }})
      case _ => (None,Nil)
    }
  }

  override def inlayHint(params: InlayHintParams): List[InlayHint] = {
    val (doc,as) = getAnnotations(params.getTextDocument,params.getRange)
    as.flatMap(_.getInlays).map { case (label,tooltip,kind,offset,pl,pr) =>
      val ret = new InlayHint
      ret.setLabel(label)
      if (tooltip != "") ret.setTooltip(tooltip)
      kind.foreach(ret.setKind)
      val pos = LSPDocument.toLC(offset,doc.get.doctext)
      ret.setPosition(new Position(pos._1,pos._2))
      ret.setPaddingLeft(pl)
      ret.setPaddingRight(pr)
      ret
    }
  }

  def getAnnotations(doc : TextDocumentIdentifier, pos: lsp4j.Position) = {
    documents.synchronized{documents.get(doc.getUri)} match {
      case Some(doc:DocumentType) =>
        val off = LSPDocument.toOffset(pos.getLine,pos.getCharacter,doc.doctext)
        (Some(doc),doc.synchronized{ doc.Annotations.getAll.collect{
          case a if a.offset <= off && off <= a.offset + a.length => a
        }})
      case _ => (None,Nil)
    }
  }
  def getAnnotations(doc : TextDocumentIdentifier) = {
    documents.synchronized{documents.get(doc.getUri)} match {
      case Some(doc:DocumentType) =>
        (Some(doc),doc.synchronized{ doc.Annotations.getAll})
      case _ => (None,Nil)
    }
  }

  override def codeLens(params: CodeLensParams): List[CodeLens] = {
    val (doc,as) = getAnnotations(params.getTextDocument)
    as.flatMap(_.getCodeLenses).map{case (title,commandname,arguments,start,end) =>
      val range = new lsp4j.Range({
        val (l,c) = LSPDocument.toLC(start,doc.get.doctext)
        new Position(l,c)
      },{
        val (l,c) = LSPDocument.toLC(end,doc.get.doctext)
        new Position(l,c)
      })
      val cl = new CodeLens(range)
      cl.setCommand(new Command(title,commandname,arguments match {
        case Nil => null
        case ls => ls.asJava
      }))
      cl
    }
  }


  override def implementation(params: ImplementationParams): List[Location] = {
    val (doc,as) = getAnnotations(params.getTextDocument,params.getPosition)
    as.flatMap(_.getImplementation).map { case (s,i,j) =>
      new Location(s,new lsp4j.Range({
        val (l,p) = LSPDocument.toLC(i,doc.get.doctext)
        new Position(l,p)
      },{
        val (l,p) = LSPDocument.toLC(j,doc.get.doctext)
        new Position(l,p)
      }))
    }
  }

  override def declaration(params: DeclarationParams): List[Location] = {
    val (doc,as) = getAnnotations(params.getTextDocument,params.getPosition)
    as.flatMap(_.getDeclaration).map { case (s,i,j) =>
      new Location(s,new lsp4j.Range({
        val (l,p) = LSPDocument.toLC(i,doc.get.doctext)
        new Position(l,p)
      },{
        val (l,p) = LSPDocument.toLC(j,doc.get.doctext)
        new Position(l,p)
      }))
    }
  }

  override def definition(params: DefinitionParams): (Option[List[Location]],Option[List[LocationLink]]) = {
    val (doc,as) = getAnnotations(params.getTextDocument,params.getPosition)
    val ls = as.flatMap(_.getDefinitions).map { case (s,i,j) =>
      new Location(s,new lsp4j.Range({
        val (l,p) = LSPDocument.toLC(i,doc.get.doctext)
        new Position(l,p)
      },{
        val (l,p) = LSPDocument.toLC(j,doc.get.doctext)
        new Position(l,p)
      }))
    }
    (Some(ls),None)
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


  override def documentSymbol(params: DocumentSymbolParams): List[(Option[SymbolInformation],Option[DocumentSymbol])] = {
    documents.synchronized{documents.get(params.getTextDocument.getUri)} match {
      case Some(doc: DocumentType) =>
        doc.synchronized {
          val annots = doc.Annotations.getAll
          if (annots.isEmpty) Nil
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
            ret.map(e => (None,Some(e)))
          }
        }
      case _ => Nil
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