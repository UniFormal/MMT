package info.kwarc.mmt.lsp.mmt

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.MMTSystem
import info.kwarc.mmt.lsp.{LSP, LSPClient, LSPServer, LSPWebsocket, LocalStyle, RunStyle, TextDocumentServer, WithAutocomplete}
import org.eclipse.lsp4j.{CompletionItem, CompletionList, CompletionOptions, CompletionParams, InitializeParams, InitializeResult, TextDocumentSyncKind}

import scala.jdk.CollectionConverters._

trait MMTClient extends LSPClient
class MMTLSPWebsocket extends LSPWebsocket(classOf[MMTClient],classOf[MMTLSPServer])
class MMTLSP(port : Int = 5007, webport : Int = 5008) extends LSP(classOf[MMTClient],classOf[MMTLSPServer],classOf[MMTLSPWebsocket])("mmt",port,webport) {
  override def newServer(style: RunStyle): MMTLSPServer = new MMTLSPServer(style)
}

class MMTLSPServer(style : RunStyle) extends LSPServer(classOf[MMTClient]) with TextDocumentServer[MMTClient,MMTFile] with WithAutocomplete[MMTClient] {

  override def controller: Controller = super.controller

  override def newDocument(uri: String): MMTFile = new MMTFile(uri,client,this)

  override def initialize(params: InitializeParams, result: InitializeResult): Unit = {
    super.initialize(params,result)

    //val sh = new SemanticTokensWithRegistrationOptions(new SemanticTokensLegend(Colors.scopes,Nil.asJava))
    //sh.setScopes(Colors.scopes)
    //sh.setFull(true)
    //result.getCapabilities.setSemanticTokensProvider(sh)
  }

  override val triggerChar: Char = 'j'

  val completionls = {
    val pairstrings = (MMTSystem.getResourceAsString("unicode/unicode-latex-map") + "\n" +
      MMTSystem.getResourceAsString("unicode/unicode-ascii-map")).split("\n")
    val pairs: List[(String, String)] = pairstrings.collect { case s if s.nonEmpty && !s.trim.startsWith("//") =>
      val ps = s.splitAt(s.lastIndexOf('|'))
      (ps._1.trim,ps._2.trim.drop(1))
    }.toList
    pairs.map {
      case (a,b) =>
        Completion(a + " | " + b,filterText = a,insertText = b)
    }
  }

  override def completion(doc : String, line : Int, char : Int): List[Completion] = completionls

  override def shutdown: Any = style match {
    case LocalStyle => scala.sys.exit()
    case _ =>
  }

  override def connect: Unit = logClient("Connected to MMT!")
}

object Colors {
  val keyword = 0
  val comment = 1
  val scomment = 2
  val name = 3
  val md = 4
  val dd = 5
  val od = 6
  val notation = 7
  val uri = 8
  val term = 9
  val feature = 10
  val terminit = 11
  val termchecked = 12
  val termerrored = 13
  val termvariable = 14
  val termconstantlocal = 15
  val termconstantincluded = 16
  val termconstantmeta = 17

  val scopesO = List(
    "mmt.keyword","mmt.comment","mmt.documentation","mmt.name",
    "mmt.md","mmt.dd","mmt.od"
    ,"mmt.notation","mmt.uri",
    "mm.term","mmt.feature",
    "mmt.term.init","mmt.term.checked","mmt.term.errored",
    "mmt.term.variable","mmt.term.constant.local","mmt.term.constant.included","mmt.term.constant.meta")
  val scopes = scopesO.asJava//scopesO.map(_.split('.').toList.asJava).asJava
}