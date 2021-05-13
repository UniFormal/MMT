package info.kwarc.mmt.lsp.mmt

import info.kwarc.mmt.api
import info.kwarc.mmt.api.{ContainerElement, DPath, ErrorHandler, StructuralElement}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.{AnnotatedCommentToken, AnnotatedKeyword, AnnotatedName, AnnotatedOpaque, AnnotatedPath, AnnotatedTermToken, AnnotatedText, DeclarationDelimiter, ErrorText, KeywordBasedParser, ModuleDelimiter, NotationBasedParser, ObjectDelimiter, ParsingStream, SourceRegion, StructureParserContinuations}
import info.kwarc.mmt.api.utils.{File, URI}
import info.kwarc.mmt.lsp.{LSPClient, LSPDocument}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, MessageParams, MessageType, PublishDiagnosticsParams, VersionedTextDocumentIdentifier}

class MMTFile(uri : String,client:MMTClient,server:MMTLSPServer) extends LSPDocument(uri, client, server) {
  val controller = server.controller
  private lazy val file = File(uri.drop(7))

  private var _highlights : List[Int] = Nil
  def highlight = {synchronized{_highlights}}

  //override val timercount : Int = 5

  /*
  override protected def onUpdate(deltas : List[Delta]) = computeHighlight

  def computeHighlight : Unit = {
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
    client.logMessage(new MessageParams(MessageType.Info,"Done"))
    client.refreshSemanticTokens()
  }

   */

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