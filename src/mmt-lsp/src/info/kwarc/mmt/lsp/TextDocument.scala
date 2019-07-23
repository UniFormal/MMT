package info.kwarc.mmt.lsp

import java.util
import java.util.concurrent.CompletableFuture

import info.kwarc.mmt.api.parser.SourceRegion
import info.kwarc.mmt.api.utils.{File, MMTSystem}
import org.eclipse.lsp4j.{CodeAction, CodeActionParams, CodeLens, CodeLensParams, CompletionItem, CompletionList, CompletionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, DocumentHighlight, DocumentSymbol, DocumentSymbolParams, FoldingRange, FoldingRangeRequestParams, Hover, Location, LocationLink, ReferenceParams, RenameParams, SemanticHighlightingInformation, SemanticHighlightingParams, SignatureHelp, SymbolInformation, TextDocumentItem, TextDocumentPositionParams, TextEdit, VersionedTextDocumentIdentifier, WorkspaceEdit}
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.util.SemanticHighlightingTokens

import scala.collection.JavaConverters._


trait TextDocument { self : ServerEndpoint =>
  object Documents {

  }
  private var doctext :String = ""

  private def toLC(offset:Int) = {
    val before = doctext.take(offset)
    val lines = before.split('\n')
    (lines.length-1,lines.last.length)
  }

  /*
  case class Highlight(offset:Int,length:Int,cls:Int) {
    lazy val (line,char) = toLC(offset)
    lazy val token = new SemanticHighlightingTokens.Token(char,length,cls)
  }
   */
  case class Highlight(line: Int,char:Int,length:Int,cls:Int) {
    lazy val token = new SemanticHighlightingTokens.Token(char,length,cls)
  }

  object Highlight {
    def apply(sr:SourceRegion,cls:Int):Highlight = {
      val (line,char) = if (sr.start.line>=0 && sr.start.column>=0) (sr.start.line,sr.start.column) else toLC(sr.start.offset)
      Highlight(line,char,sr.end.offset-sr.start.offset,cls)
    }
    def apply(offset:Int,length:Int,cls:Int):Highlight = {
      val (line,char) = toLC(offset)
      Highlight(line,char,length,cls)
    }
  }

    protected object a_TD extends TextDocumentService {
      override def didSave(params: DidSaveTextDocumentParams): Unit = {
        log("didSave",Some("methodcall-textDocument"))
      }

      def semanticHighlight(doc:TextDocumentItem, ls : List[Highlight]):Unit = {
        val tdi = new VersionedTextDocumentIdentifier(doc.getUri,doc.getVersion)
        semanticHighlight(tdi,ls)
      }

      def semanticHighlight(doc:VersionedTextDocumentIdentifier, highs : List[Highlight]):Unit = {
        var lines = highs.sortBy(_.line)
        var result : List[List[Highlight]] = Nil
        while (lines.nonEmpty) {
          var index = lines.indexWhere(_.line != lines.head.line)
          if (index == -1) index = lines.length
          val split = lines.take(index)
          lines = lines.drop(index)
          result ::= split.sortBy(_.char)
        }
        val highlights = result.reverse.map(ls => new SemanticHighlightingInformation(ls.head.line,SemanticHighlightingTokens.encode(ls.map(_.token).asJava)))
        // val highlights = new SemanticHighlightingInformation(line,SemanticHighlightingTokens.encode(ls.asJava))
        val params = new SemanticHighlightingParams()
        params.setTextDocument(doc)
        params.setLines(highlights.asJava)
        self.client.semanticHighlighting(params)
      }


      override def didClose(params: DidCloseTextDocumentParams): Unit = {
        log("didClose",Some("methodcall-textDocument"))
      }

      override def didOpen(params: DidOpenTextDocumentParams): Unit = {
        log("didOpen",Some("methodcall-textDocument"))
        val document = params.getTextDocument
        val file = File(document.getUri.drop(7))
        log("File: " + file,Some("didOpen"))
        // log("document: " + document,Some("didOpen"))
        val ls = List(Highlight(0,0,"namespace".length,Colors.keyword))//List(new SemanticHighlightingTokens.Token(0,"namespace".length,0))
        semanticHighlight(document,ls)
      }

      override def didChange(
                              params: DidChangeTextDocumentParams
                            ): Unit = {
        log("didChange",Some("methodcall-textDocument"))
        val file = File(params.getTextDocument.getUri.drop(7))
        log("File: " + file,Some("didChange"))
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

case class MyRange(start:MyPosition,end:MyPosition) {
  assert(start.file==end.file)
  override def toString: String = this match {
    case Position(i,j,f) => Position(i,j,f).toString
    case _ => start.file.name + ":" + start.line + "." + start.column + ":" +
      end.line + "." + end.column
  }
}
case class MyPosition(line:Int,column:Int,file:File) {

  override def toString: String = file.name + ":" + line + "." + column
}

object Position {
  def apply(r : org.eclipse.lsp4j.Position,f:File) = {
    MyPosition(r.getLine,r.getCharacter,f)
  }
  def apply(line:Int,column:Int,file:File) = MyPosition(line,column,file)

  def unapply(arg: Any): Option[(Int,Int,File)] = arg match {
    case MyPosition(l,c,f) => Some((l,c,f))
    case MyRange(s,e) if s==e => Some((s.line,s.column,s.file))
    case _ => None
  }

}

object Range {
  def apply(r:org.eclipse.lsp4j.Range,f:File) = {
    MyRange(Position(r.getStart,f),Position(r.getEnd,f))
  }
  def apply(start:MyPosition,end:MyPosition) = MyRange(start,end)

  def unapply(myRange: MyRange) = MyRange.unapply(myRange)
}