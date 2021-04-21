package info.kwarc.mmt.lsp

import info.kwarc.mmt.api
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.{ContainerElement, DPath, ErrorHandler, StructuralElement}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.{AnnotatedComment, AnnotatedCommentToken, AnnotatedKeyword, AnnotatedName, AnnotatedOpaque, AnnotatedPath, AnnotatedTermToken, AnnotatedText, DeclarationDelimiter, ErrorText, KeywordBasedParser, ModuleDelimiter, NotationBasedParser, ObjectDelimiter, ObjectParser, ParsingStream, SourceRegion, StructureParserContinuations}
import info.kwarc.mmt.api.utils.{File, URI}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, PublishDiagnosticsParams, TextDocumentItem, VersionedTextDocumentIdentifier}


import scala.collection.JavaConverters._
import scala.concurrent.Future

class LSPDocument(val uri : String,client:MMTClient,controller:Controller) {
  private var _doctext : String = ""
  private var version = 0

  private lazy val file = File(uri.drop(7))

  /* object Timer {
    private var timer = 0
    private var timerthread: Option[Future[Unit]] = None

    def reset = {
      synchronized {
        timer = 0
      }
      if (synchronized {
        timerthread.isEmpty
      }) {
        synchronized {
          timerthread = Some(Future {
            while (synchronized {
              timer < 5
            }) {
              Thread.sleep(100)
              synchronized {
                timer += 1
              }
            }
            synchronized { timerthread = None }
            updateNow
          }(scala.concurrent.ExecutionContext.global))
        }
      }
    }
  } */

  private var _changes : List[(org.eclipse.lsp4j.Range,String)] = Nil

  def update(range: org.eclipse.lsp4j.Range ,text:String) = {
    synchronized { _changes ::= (range,text) }
    //Timer.reset
    updateNow
  }

  private def updateNow: Unit = {
    val ch = synchronized {
      val r = _changes
      _changes = Nil
      r.reverse
    }
    synchronized {
      ch.foreach { case (range,text) =>
        val start = toOff(range.getStart.getLine, range.getStart.getCharacter)
        val prev = _doctext take start
        val end = toOff(range.getEnd.getLine, range.getEnd.getCharacter)
        _doctext = prev + text + _doctext.drop(end)
      }
    }
    //computeHighlight
    // Timer.reset
  }

  def setVersion(v:Int) = version = v

  def computeHighlight = {
    //client.publishDiagnostics(new PublishDiagnosticsParams(uri,List().asJava))
    parse
    var hls : List[Highlight] = Nil
    synchronized{annotated.asSequence}.foreach {
      case a:AnnotatedKeyword =>
        hls ::= Highlight(a.region,Colors.keyword)
      case n:AnnotatedName =>
        hls ::= Highlight(n.region,Colors.name)
      case c:AnnotatedOpaque =>
        hls ::= Highlight(c.region,Colors.scomment)
      case c:AnnotatedCommentToken =>
        hls ::= Highlight(c.region,Colors.comment)
      case t:AnnotatedTermToken =>
        hls ::= Highlight(t.region,Colors.terminit)
      case md:ModuleDelimiter =>
        hls ::= Highlight(md.region,Colors.md)
      case md:DeclarationDelimiter =>
        hls ::= Highlight(md.region,Colors.dd)
      case md:ObjectDelimiter =>
        hls ::= Highlight(md.region,Colors.od)
      case p:AnnotatedPath =>
        hls ::= Highlight(p.region,Colors.termchecked)
      case e:ErrorText =>
        val diag = new Diagnostic(toRange(e.region),"Err0r",DiagnosticSeverity.Error,e.text)
        val params = new PublishDiagnosticsParams(uri,List(diag).asJava)
        client.publishDiagnostics(params)
        hls ::= Highlight(e.region,Colors.termerrored)
      case _ =>
    }
    _highlights = semanticHighlight(hls.reverse)
  }

  def highlight = {computeHighlight; synchronized{_highlights}}

  private var _highlights : List[Int] = Nil

  def init(s:String) = {
    _doctext = s
    computeHighlight
  }

  def toRange(sr:SourceRegion) = {
    val start = toLC(sr.start.offset)
    val end = toLC(sr.end.offset)
    val st = new org.eclipse.lsp4j.Position(start._1,start._2)
    val en = new org.eclipse.lsp4j.Position(end._1,end._2)
    new org.eclipse.lsp4j.Range(st,en)
  }

  def doctext = synchronized { _doctext }

  def toLC(offset:Int) = {
    val before = doctext.take(offset)
    var lines : List[String] = Nil
    var curr = ""
    before.foreach { case '\n' =>
      lines ::= curr
        curr = ""
    case c => curr = curr + c
    }
    lines = (curr :: lines).reverse
    (lines.length-1,lines.last.length)
  }

  def toOff(line:Int,col:Int) = {
    var l = 0
    var offset = 0
    val doc = doctext
    while (l < line && doc.isDefinedAt(offset)) {
      if (doc(offset) == '\n') {
        l += 1
      }
      offset += 1
    }
    offset + col
  }

  case class Highlight(line: Int,char:Int,length:Int,cls:Int) {
    def <=(o : Highlight) = {
      if (line < o.line) true else if (line == o.line) {
        char <= o.char
      } else false
    }
  }

  object Highlight {
    def apply(sr:SourceRegion,cls:Int):Highlight = {
      val (line,char) = toLC(sr.start.offset) // if (sr.start.line>=0 && sr.start.column>=0) (sr.start.line,sr.start.column) else toLC(sr.start.offset)
      Highlight(line,char,sr.end.offset-sr.start.offset,cls)
    }
    def apply(offset:Int,length:Int,cls:Int):Highlight = {
      val (line,char) = toLC(offset)
      Highlight(line,char,length,cls)
    }
  }

  def semanticHighlight(ls : List[Highlight]):List[Int] = {
    val tdi = new VersionedTextDocumentIdentifier(uri,version)
    semanticHighlight(tdi,ls)
  }

  def semanticHighlight(doc:VersionedTextDocumentIdentifier, highs : List[Highlight]): List[Int] = {
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
      case Highlight(line,char,length,cls) =>
        val nchar = if (line == lastline) (char - lastchar) else char
        val r = List(line-lastline,nchar,length,cls,0)
        lastline = line
        lastchar = char
        r
    }
    //val highlights = result.reverse.map(ls => new SemanticHighlightingInformation(ls.head.line,SemanticHighlightingTokens.encode(ls.map(_.token).asJava)))
    // val highlights = new SemanticHighlightingInformation(line,SemanticHighlightingTokens.encode(ls.asJava))
    //val params = new SemanticHighlightingParams()
    //params.setTextDocument(doc)
    //params.setLines(highlights.asJava)
    //client.semanticHighlighting(params)
  }

  lazy val nsMap = controller.getNamespaceMap

  lazy val ojp = {
    val np = new NotationBasedParser
    controller.extman.addExtension(np)
    np
  }
  lazy val parser = {
    val kp = new KeywordBasedParser(ojp)
    controller.extman.addExtension(kp)
    kp
  }

  lazy val errorCont = new ErrorHandler {
    private var errors : List[api.Error] = Nil
    override protected def addError(e: api.Error): Unit = {
      errors ::= e
    }
    def resetErrs = {
      errors = Nil
      this
    }
    def getErrors = errors
  }

  lazy implicit val spc = new StructureParserContinuations(errorCont) {
    /** to be called after parsing an element (but before parsing its body if any) */
    override def onElement(se: StructuralElement) {}
    /** to be called after parsing the body of a [[ContainerElement]], e.g., documents and declared modules */
    override def onElementEnd(se: ContainerElement[_]) {}
  }

  private var annotated : AnnotatedText = null

  private def parse: Unit = {
    errorCont.resetErrs
    val ps = ParsingStream.fromString(doctext,DPath(URI(file.toJava.toURI)),"mmt",Some(nsMap))
    val d = try { parser.apply(ps) } catch {
      case t:Throwable =>
        t.printStackTrace()
        ???
    }
    d match {
      case d: Document =>
        synchronized{ annotated = AnnotatedText.fromDocument(d,doctext,errorCont.getErrors)(controller) }
      case _ =>
        ???
    }
  }
}