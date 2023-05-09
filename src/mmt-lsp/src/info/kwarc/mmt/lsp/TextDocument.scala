package info.kwarc.mmt.lsp

import info.kwarc.mmt.api.parser.SourceRegion
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{CodeAction, CodeActionParams, CodeLens, CodeLensOptions, CodeLensParams, Command, CompletionItem, CompletionItemKind, CompletionItemTag, CompletionList, CompletionOptions, CompletionParams, DeclarationParams, DefinitionParams, DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentLinkOptions, DocumentSymbol, DocumentSymbolOptions, DocumentSymbolParams, FoldingRange, FoldingRangeRequestParams, Hover, HoverOptions, HoverParams, ImplementationParams, InitializeParams, InitializeResult, InlayHint, InlayHintParams, InsertTextFormat, InsertTextMode, Location, LocationLink, MarkupContent, MarkupKind, Position, SemanticTokens, SemanticTokensLegend, SemanticTokensParams, SemanticTokensWithRegistrationOptions, SymbolInformation, TextDocumentEdit, TextDocumentIdentifier, TextDocumentSyncKind, TextEdit, VersionedTextDocumentIdentifier, WorkDoneProgressCreateParams, WorkspaceEdit, WorkspaceSymbolOptions}

import java.util
import java.util.Map
import scala.jdk.CollectionConverters._
import scala.collection.{immutable, mutable}

trait TextDocumentServer[ClientType <: LSPClient,DocumentType <: LSPDocument[ClientType,LSPServer[ClientType]]] { self : LSPServer[ClientType] =>

  def newDocument(uri : String) : DocumentType
  protected val documents = mutable.Map.empty[String,DocumentType]

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    documents.synchronized{documents.getOrElseUpdate(document.getUri,{
      val d = newDocument(document.getUri)
      d.init(document.getText)
      d
    })}
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    val doc = documents.synchronized{documents.getOrElse(uri,{
      log("Document not found: " + uri)
      return
    })}
    doc.synchronized {
      params.getContentChanges.asScala.foreach { change =>
        doc.update(change.getRange, change.getText)
      }
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

sealed trait CodeActionTrait {
  val title:String
}
case class CodeActionEdit(title:String,kind:String,edits: List[(String,List[(Int,Int,Int,Int,String)])]) extends CodeActionTrait


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
    result.getCapabilities.setCodeActionProvider(true)
    //result.getCapabilities.setDocumentLinkProvider(new DocumentLinkOptions(true))
    result.getCapabilities.setImplementationProvider(true)
    result.getCapabilities.setDeclarationProvider(true)
    result.getCapabilities.setDefinitionProvider(true)
    result.getCapabilities.setCodeLensProvider(new CodeLensOptions(true))
  }

  def getAnnotations(doc : TextDocumentIdentifier, range: lsp4j.Range) = {
    documents.synchronized{documents.get(doc.getUri)} match { // <- for some reason the URI seems escaped here
      case Some(doc:DocumentType@unchecked) =>
        val start = range.getStart
        val end = range.getEnd
        val soff = doc._doctext.toOffset(start.getLine,start.getCharacter)
        val eoff = doc._doctext.toOffset(end.getLine,end.getCharacter)
        (Some(doc),doc.synchronized{ doc.Annotations.getAll.collect{
          case a if soff <= a.offset && a.offset + a.length <= eoff => a
        }})
      case _ => (None,Nil)
    }
  }

  override def codeAction(params: CodeActionParams): List[CodeAction] = {
    //import scala.jdk.CollectionConverters._
    val (doc, as) = getAnnotations(params.getTextDocument, params.getRange)
    as.flatMap(_.getCodeActions).map {
      case CodeActionEdit(title,kind,edits) =>
        val ret = new CodeAction(title)
        ret.setKind(kind)
        val edit = new WorkspaceEdit(immutable.Map.from(edits.map{ p =>
          (p._1,p._2.map{e =>
            val edt = new TextEdit()
            edt.setNewText(e._5)
            edt.setRange(new lsp4j.Range(new Position(e._1,e._2),new Position(e._3,e._4)))
            edt
          }.asJava)
        }).asJava)
        ret.setEdit(edit)
        ret
    }

  }
  override def inlayHint(params: InlayHintParams): List[InlayHint] = {
    val (doc,as) = getAnnotations(params.getTextDocument,params.getRange)
    as.flatMap(_.getInlays).map { case (label,tooltip,kind,offset,pl,pr) =>
      val ret = new InlayHint
      ret.setLabel(label)
      if (tooltip != "") ret.setTooltip(tooltip)
      kind.foreach(ret.setKind)
      val pos = doc.get._doctext.toLC(offset)
      ret.setPosition(new Position(pos._1,pos._2))
      ret.setPaddingLeft(pl)
      ret.setPaddingRight(pr)
      ret
    }
  }

  def getAnnotations(doc : TextDocumentIdentifier, pos: lsp4j.Position) = {
    documents.synchronized{documents.get(doc.getUri)} match { // <- for some reason the URI seems escaped here
      case Some(doc:DocumentType@unchecked) =>
        val off = doc._doctext.toOffset(pos.getLine,pos.getCharacter)
        (Some(doc),doc.synchronized{ doc.Annotations.getAll.collect{
          case a if a.offset <= off && off <= a.offset + a.length => a
        }})
      case _ => (None,Nil)
    }
  }
  def getAnnotations(doc : TextDocumentIdentifier) = {
    documents.synchronized{documents.get(doc.getUri)} match { // <- for some reason the URI seems escaped here
      case Some(doc:DocumentType@unchecked) =>
        (Some(doc),doc.synchronized{ doc.Annotations.getAll})
      case _ => (None,Nil)
    }
  }

  override def codeLens(params: CodeLensParams): List[CodeLens] = {
    val (doc,as) = getAnnotations(params.getTextDocument)
    as.flatMap(_.getCodeLenses).map{case (title,commandname,arguments,start,end) =>
      val range = new lsp4j.Range({
        val (l,c) = doc.get._doctext.toLC(start)
        new Position(l,c)
      },{
        val (l,c) = doc.get._doctext.toLC(end)
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
        val (l,p) = doc.get._doctext.toLC(i)
        new Position(l,p)
      },{
        val (l,p) = doc.get._doctext.toLC(j)
        new Position(l,p)
      }))
    }
  }

  override def declaration(params: DeclarationParams): List[Location] = {
    val (_,as) = getAnnotations(params.getTextDocument,params.getPosition)
    as.flatMap(_.getDeclaration).map { case (s,i,j) =>
      val sdoc = if (s.startsWith("file:///")) s else if (s.startsWith("file:/")) "file:///" + s.drop(6) else s
      val sd = documents.getOrElse(sdoc,newDocument(sdoc))
      new Location(sdoc, new lsp4j.Range({
        val (l, p) = sd._doctext.toLC(i)
        new Position(l, Math.max(p, 0))
      }, {
        val (l, p) = sd._doctext.toLC(j)
        new Position(l, Math.max(p, 0))
      }))
    }
  }

  override def definition(params: DefinitionParams): (Option[List[Location]],Option[List[LocationLink]]) = {
    val (_,as) = getAnnotations(params.getTextDocument,params.getPosition)
    val ls = as.flatMap(_.getDefinitions).map { case (s,i,j) =>
      val sdoc = if (s.startsWith("file:///")) s else if (s.startsWith("file:/")) "file:///" + s.drop(6) else s
      val sd = documents.getOrElse(sdoc, newDocument(sdoc))
      new Location(sdoc, new lsp4j.Range({
        val (l, p) = sd._doctext.toLC(i)
        new Position(l, Math.max(p, 0))
      }, {
        val (l, p) = sd._doctext.toLC(j)
        new Position(l, Math.max(p, 0))
      }))
    } ::: as.flatMap(_.getDefinitionsLC)
    (Some(ls),None)
  }

  override def semanticTokensFull(params: SemanticTokensParams): SemanticTokens = {
    documents.synchronized{documents.get(params.getTextDocument.getUri)} match {
      case Some(doc : DocumentType@unchecked) =>
        doc.synchronized{ doc.Annotations.getAll.flatMap(_.getHighlights) } match {
          case Nil =>
            null
          case ls =>
            val ret = computeHighlights(ls.map(is => Highlight(is._1,is._2,is._3,is._4,is._5)))
            log("textDocument/semanticTokens/full return: " + ret.length)
            new SemanticTokens(ret.asJava)
        }
      case _ => null
    }
  }

  override def hover(params: HoverParams): Hover = {
    documents.synchronized{documents.get(params.getTextDocument.getUri)} match {
      case Some(doc : DocumentType@unchecked) =>
        doc.synchronized{ doc.Annotations.getAll.flatMap(_.getHovers)} match {
          case Nil =>
            null
          case ls =>
            val pos = doc._doctext.toOffset(params.getPosition.getLine,params.getPosition.getCharacter)
            val fOs = ls.collect{case (s,e,f) if s <= pos && pos <= e => (e-s,f)}
            fOs.sortBy(_._1).headOption.map { case (_,f) =>
              val h = new Hover()
              h.setContents(new MarkupContent(MarkupKind.MARKDOWN,f(())))
              h
            }.orNull
        }
      case _ => null
    }
  }

  override def foldingRange(params: FoldingRangeRequestParams): List[FoldingRange] = {
    documents.synchronized{documents.get(params.getTextDocument.getUri)} match {
      case Some(doc: DocumentType@unchecked) =>
        val annots = doc.synchronized { doc.Annotations.getAll }
        if (annots.isEmpty) null
        else {
          annots.collect{
            case a if a.foldable =>
              val (start,_) = doc._doctext.toLC(a.offset)
              val (end,_) = doc._doctext.toLC(a.end)
              new FoldingRange(start,end)
          }
        }
      case _ => Nil
    }
  }


  override def documentSymbol(params: DocumentSymbolParams): List[(Option[SymbolInformation],Option[DocumentSymbol])] = {
    documents.synchronized{documents.get(params.getTextDocument.getUri)} match {
      case Some(doc: DocumentType@unchecked) =>
        val annots = doc.synchronized { doc.Annotations.getAll }
        if (annots.isEmpty) Nil
        else {
          var dones: List[doc.DocAnnotation] = Nil

          def toSymbol(a: doc.DocAnnotation) = {
            val (sl, sc) = doc._doctext.toLC(a.offset)
            val (el, ec) = doc._doctext.toLC(a.end)
            val range = new lsp4j.Range(new Position(sl, sc), new Position(el, ec))
            new DocumentSymbol(a.symbolname, a.symbolkind, range, range, "")
          }

          def inSymbol(a: DocumentSymbol, b: doc.DocAnnotation): Unit = {
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