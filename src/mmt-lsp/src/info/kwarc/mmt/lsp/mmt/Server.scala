package info.kwarc.mmt.lsp.mmt

import info.kwarc.mmt.api
import info.kwarc.mmt.api.{GlobalName, Path}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.ontology.Binary.toRelation
import info.kwarc.mmt.api.ontology.{DependsOn, IsConstant, Transitive}
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.utils.{File, FilePath, MMTSystem, URI}
import info.kwarc.mmt.lsp._
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

trait MMTClient extends LSPClient
class MMTLSPWebsocket extends LSPWebsocket(classOf[MMTClient],classOf[MMTLSPServer])

class BuildMessage(val uri: String)

class MMTLSP(port : Int = 5007, webport : Int = 5008) extends LSP(classOf[MMTClient], classOf[MMTLSPServer], classOf[MMTLSPWebsocket])("mmt",port,webport) {
  override def newServer(style: RunStyle): MMTLSPServer = new MMTLSPServer(style)
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
    super.initialize(params, result)

    val refOptions = new ReferenceOptions()
    refOptions.setWorkDoneProgress(true)
    result.getCapabilities.setReferencesProvider(refOptions)

    params.getWorkspaceFolders.asScala.map(_.getUri).foreach(uri => {
      val workspacePath = URI(uri.replace("c%3A", "C:")).path.mkString(java.io.File.separator)
      // todo: controller.addArchive(File(workspacePath)) no longer needed?

      client.log(s"Registering `$workspacePath` as mathpath archive.")
      controller.handleLine("mathpath archive " + workspacePath)
      controller.handleLine("lmh root " + workspacePath)
      client.log(s"Found archives: ${controller.backend.getArchives.map(_.id).mkString(", ")}.")
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

  override def references(params: ReferenceParams): List[Location] = {
    // load all relational data of archives
    controller.backend.getArchives.foreach(archive => {
      archive.allContent
      archive.readRelational(FilePath("/"), controller, "rel")
    })

    val doc = documents.getOrElse(params.getTextDocument.getUri, return Nil)
    val offset = doc._doctext.toOffset(params.getPosition.getLine, params.getPosition.getCharacter)

    val matchingDeclarations: List[GlobalName] = doc.Annotations.getAll.filter(a => {
      a.offset <= offset && offset <= a.end && a.symbolkind == SymbolKind.Constant
    }).collect {
      case a if a.value.isInstanceOf[Declaration] => a.value.asInstanceOf[Declaration]
    }.map(_.path)

    val dependencies: Set[Path] = matchingDeclarations.flatMap(path => {
      controller.depstore.querySet(path, Transitive(-DependsOn))
    }).toSet
    println(dependencies)

    Nil
  }

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
    client.resetErrors(doc.uri)
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
    client.documentErrors(doc,true, errors.toSeq : _*)
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