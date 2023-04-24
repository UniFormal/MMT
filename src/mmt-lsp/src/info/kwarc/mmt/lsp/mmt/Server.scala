package info.kwarc.mmt.lsp.mmt

import info.kwarc.mmt.api
import info.kwarc.mmt.api.{DPath, ErrorHandler, utils}
import info.kwarc.mmt.api.archives.{BuildManager, TrivialBuildManager}
import info.kwarc.mmt.api.frontend.{Controller, Run}
import info.kwarc.mmt.api.parser.ParsingStream
import info.kwarc.mmt.api.utils.{File, MMTSystem, URI}
import info.kwarc.mmt.lsp.{LSP, LSPClient, LSPServer, LSPWebsocket, LocalStyle, RunStyle, TextDocumentServer, WithAnnotations, WithAutocomplete}
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.{CompletionItem, CompletionList, CompletionOptions, CompletionParams, InitializeParams, InitializeResult, SemanticTokensWithRegistrationOptions, TextDocumentSyncKind}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

trait MMTClient extends LSPClient
class MMTLSPWebsocket extends LSPWebsocket(classOf[MMTClient],classOf[MMTLSPServer])

class BuildMessage(val uri: String)

class MMTLSP(port : Int = 5007, webport : Int = 5008) extends LSP(classOf[MMTClient], classOf[MMTLSPServer], classOf[MMTLSPWebsocket])("mmt",port,webport) {
  override def newServer(style: RunStyle): MMTLSPServer = new MMTLSPServer(style)
}

object Socket {
  def main(args: Array[String]) : Unit = {
    val lsp = new MMTLSP
    val controller = Run.controller
    List("lsp", "lsp-mmt")
      .foreach(s => controller.handleLine(s"log+ $s"))
    controller.handleLine("log console")
    controller.handleLine("server on 8090")
    controller.extman.addExtension(lsp)
    controller.extman.get(classOf[BuildManager]).foreach(controller.extman.removeExtension)
    controller.extman.addExtension(new TrivialBuildManager)
    lsp.runSocketListener
  }
}

class MMTLSPServer(style : RunStyle) extends LSPServer(classOf[MMTClient])
  with TextDocumentServer[MMTClient,MMTFile]
  with WithAutocomplete[MMTClient]
  with WithAnnotations[MMTClient,MMTFile]
{

  override def controller: Controller = super.controller

  override val scopes: List[String] = Colors.scopes
  override val modifiers: List[String] = Colors.modifiers

  lazy val parser: IterativeParser = {
    val p = new IterativeParser()
    controller.extman.addExtension(p)
    p
  }

  override def newDocument(uri: String): MMTFile = new MMTFile(uri,client,this)

  override def initialize(params: InitializeParams, result: InitializeResult): Unit = {
    super.initialize(params,result)
    params.getWorkspaceFolders.asScala.map(_.getUri).foreach(uri => {
      val workspacePath = URI(uri.replace("c%3A", "C:")).path.mkString(java.io.File.separator)
      controller.addArchive(File(workspacePath))
    })
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

  override def connect: Unit = client.log(s"Connected to MMT v${MMTSystem.version}.")

  @JsonNotification("mmt/typecheck")
  def typecheck(msg: BuildMessage): Unit = {
    val doc: MMTFile = documents.find {
      case (uri, _) => File(msg.uri).toURI.toURL.sameFile(URI(uri).toJava.toURL)
    }.map(_._2).getOrElse {
      client.logError(s"Internal error File ${msg.uri} requested to build was not communicated to be" +
        s" open to LSP server. Open documents known to LSP server are: ${documents.keys.mkString(", ")}.")
      return
    }

    client.log(s"Typechecking `${doc.uri}`...")

    /*val ps = ParsingStream.fromString(doc.toString)
    // (text, DPath(URI(uri)), file.getExtension.getOrElse(""), Some(nsMap))
    val progress = new Progresser(file, (s1, s2) => note(s1, s2))
    ps.addListener(progress)
    clearFile(file.toString)*/
    doc.Annotations.clear
    doc.onUpdate(Nil)
    // doc.errorCont.getErrors


    client.log(s"Typechecking finished of `${doc.uri}`.")
  }
  @JsonNotification("mmt/build/mmt-omdoc")
  def buildMMTOmdoc(msg: BuildMessage): Unit = {
    val doc: MMTFile = documents.find {
      case (uri, _) => File(msg.uri).toURI.toURL.sameFile(URI(uri).toJava.toURL)
    }.map(_._2).getOrElse {
      client.logError(s"Internal error File ${msg.uri} requested to build was not communicated to be" +
        s" open to LSP server. Open documents known to LSP server are: ${documents.keys.mkString(", ")}.")
      return
    }

    client.log(s"Building `${doc.uri}` to mmt-omdoc...")
    val errors = mutable.ListBuffer[api.Error]()
    controller.build(File(msg.uri))(errors.append)
    client.resetErrors(doc.uri)
    client.documentErrors(doc, errors.toSeq : _*)
    client.log(s"Build finished of `${msg.uri}`!")
  }
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
  val termvariable = 11
  val termconstantlocal = 12
  val termconstantincluded = 13
  val termconstantmeta = 14
  val termoml = 15
  val termomlit = 16

  val mod_terminit = 0
  val mod_termchecked = 1
  val mod_termerrored = 2

  val scopes = List(
    "mmt.keyword","mmt.comment","mmt.documentation","mmt.name",
    "mmt.md","mmt.dd","mmt.od"
    ,"mmt.notation","mmt.uri",
    "mm.term","mmt.feature",
    "mmt-term-variable","mmt-term-constant-local","mmt-term-constant-included","mmt-term-constant-meta","mmt-term-oml","mmt-term-omlit")
  val modifiers = List(
    "mmt.term.init","mmt.term.checked","mmt.term.errored"
  )
}