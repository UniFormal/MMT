package info.kwarc.mmt.lsp.mmt

import info.kwarc.mmt.api
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{Controller, ReportHandler}
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.ontology.DependsOn
import info.kwarc.mmt.api.ontology.RelationExp.AnyDep
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.{Constant, Declaration}
import info.kwarc.mmt.api.utils.{File, FilePath, MMTSystem, URI}
import info.kwarc.mmt.lsp._
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}

import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.jdk.CollectionConverters._

sealed class ShellLogMessage(
                            val ind: Integer,
                            val caller: String,
                            val group: String,
                            val msgParts: java.util.List[String]
                          )
trait MMTClient extends LSPClient {
  @JsonRequest def shellLog(msg: ShellLogMessage): CompletableFuture[Unit]
}

class MMTLSPWebsocket extends LSPWebsocket(classOf[MMTClient],classOf[MMTLSPServer])

sealed class BuildMessage(val uri: String)

sealed class HandleLineMessage(val line: String)

class MMTLSP(port : Int = 5007, webport : Int = 5008) extends LSP(classOf[MMTClient], classOf[MMTLSPServer], classOf[MMTLSPWebsocket])("mmt",port,webport) {
  override def newServer(style: RunStyle): MMTLSPServer = new MMTLSPServer(style)
}

class MMTLSPServer(style : RunStyle) extends LSPServer(classOf[MMTClient])
  with TextDocumentServer[MMTClient,MMTFile]
  with WithAutocomplete[MMTClient]
  with WithAnnotations[MMTClient,MMTFile] {

  override def controller: Controller = super.controller

  override val scopes: List[String] = Colors.scopes
  override val modifiers: List[String] = Colors.modifiers

  lazy val parser: IterativeParser = {
    val p = new IterativeParser()
    controller.extman.addExtension(p)
    p
  }

  override def newDocument(uri: String): MMTFile = new MMTFile(uri, client, this)

  override def initialize(params: InitializeParams, result: InitializeResult): Unit = {
    super.initialize(params, result)

    val refOptions = new ReferenceOptions()
    refOptions.setWorkDoneProgress(true)
    result.getCapabilities.setReferencesProvider(refOptions)

    controller.report.addHandler(new ReportHandler("vscode-mmt-extension") {
      override def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = {
        client.client.shellLog(new ShellLogMessage(ind, caller, group, msgParts.asJava))
      }
    })

    params.getWorkspaceFolders.asScala
      .flatMap(workspaceFolder => LSPServer.VSCodeToFile(workspaceFolder.getUri) match {
        case Some(f) => Some(f)
        case None =>
          client.log(s"Could not parse `${workspaceFolder.getUri} as a file path on your operating system.")
          None
      })
      .foreach(workspacePath => {
        client.log(s"Registering `$workspacePath` as mathpath archive.")
        controller.addArchive(workspacePath)
        // todo: We shouldn't set lmh root via CLI (e.g., because workspacePath may contain
        //       spaces. We should manually invoke the underlying action (as was done for
        //       adding the archives in the line above).
        controller.handleLine("lmh root " + workspacePath)

        if (controller.backend.getArchives.isEmpty) {
          client.logError(
            "No archives could be found. Likely no MMT content will work! " +
              "(1) Did you open VSCode in the root folder of your archives? " +
              "(2) Does the path reported above look correct? If nothing else helps, rename your folder hierarchy such that it doesn't contain any spaces or special characters."
          )
        } else {
          client.log(s"Found archives: ${controller.backend.getArchives.map(_.id).mkString(", ")}.")
        }
      })
  }

  override val triggerChar: Char = 'j'

  val completionls = {
    val pairstrings = (MMTSystem.getResourceAsString("unicode/unicode-latex-map") + "\n" +
      MMTSystem.getResourceAsString("unicode/unicode-ascii-map")).split("\n")
    val pairs: List[(String, String)] = pairstrings.collect { case s if s.nonEmpty && !s.trim.startsWith("//") =>
      val ps = s.splitAt(s.lastIndexOf('|'))
      (ps._1.trim, ps._2.trim.drop(1))
    }.toList
    pairs.map {
      case (a, b) =>
        Completion(a + " | " + b, filterText = a, insertText = b)
    }
  }

  override def completion(doc: String, line: Int, char: Int): List[Completion] = completionls

  // todo: atomic?
  private var hasLoadedRelational = false

  /**
    * Finds all usages ("references") of an MMT [[info.kwarc.mmt.api.StructuralElement StructuralElement]].
    *
    * @todo So far it only allows constants, e.g., not theories or views.
    */
  override def references(params: ReferenceParams): List[Location] = {
    // helper function wrapped in withProgress below
    def action(progress: (Double, String) => Unit): (List[Location], String) = {
      // Step 1: determine whose element we should seek references for
      //         (i.e., where has the user put their cursor in the active document?)
      val doc = documents.getOrElse(params.getTextDocument.getUri, {
        return (Nil, "Could not determine active document")
      })
      val offset = doc._doctext.toOffset(params.getPosition.getLine, params.getPosition.getCharacter)

      val path: ContentPath = {
        // first try to match constants
        val constant = doc.Annotations.getAll.collect {
          case a if a.value.isInstanceOf[Declaration] &&
            a.offset <= offset && offset <= a.end && a.symbolkind == SymbolKind.Constant
          => (a.length, a.value.asInstanceOf[Constant].path)
        }.sortBy(_._1).map(_._2).headOption

        constant match {
          case x@Some(_) => x
          case None =>
            doc.Annotations.getAll.collect {
              case a if a.value.isInstanceOf[Module] &&
                a.offset <= offset && offset <= a.end && a.symbolkind == SymbolKind.Module
              => (a.length, a.value.asInstanceOf[Module].path)
            }.sortBy(_._1).map(_._2).headOption
        }
      }.getOrElse {
        return (Nil, "Could not determine element for which to seek references")
      }

      // Step 2
      progress(0.0, "Initializing relational data of archives")
      if (!hasLoadedRelational) { // not yet loaded
        controller.backend.getArchives.foreach(_.readRelational(FilePath("/"), controller, "rel"))
        hasLoadedRelational = true
      }
      progress(50.0, "Loaded relational data of archives")

      // Step 3
      progress(60.0, "Querying relational data for references")
      val dependencies: Set[Path] = path match {
        case p: GlobalName =>
          // Note (1): We need to specialize to type and def component here because
          //           the relation DependsOn is component-component and module-module.
          // Note (2): As in the case below for MPaths, we could also use -AnyDep as the
          //           query here. Not sure if this is wanted.
          controller.depstore.querySet(p $ TypeComponent, -DependsOn) ++
            controller.depstore.querySet(p $ DefComponent, -DependsOn)
        case mp: MPath =>
          controller.depstore.querySet(mp, -AnyDep(controller.relman))
      }

      // Step 4
      progress(75.0, "Post-processing results")
      val references: List[Location] = dependencies.map {
        case cp: CPath => cp.parent
        case path => path
      }.map(dep => (dep, controller.getO(dep))).flatMap {
        case (_, Some(e)) => Some(e)
        case (dep, None) =>
          client.log(s"Reference `$dep` found in relational but not contents data.")
          None
      }.map(e => (e, SourceRef.get(e))).flatMap {
        case (e, Some(src)) => Some((e, src))
        case (e, None) =>
          client.log(s"Reference `$e` found in relational and content data, but no source references available.")
          None
      }.map { case (e, src) => (e, src, resolveSourceFilepath(src)) }.flatMap {
        case (_, src, Some(srcPath)) =>
          val (start, end) = (src.region.start, src.region.end)
          Some(new Location(srcPath.toURI.getPath, new Range(
            new Position(start.line, start.column),
            new Position(end.line, end.column)
          )))

        case (e, _, None) =>
          client.log(s"Reference `$e` found in relational and content data, but source references failed to resolve to an existing archive on disk.")
          None
      }.toList // todo: impose some user-friendly order?

      (references, "Loaded all references")
    }

    withProgress(params, "Searching References", "Searching References")(action)
  }

  /**
    * Tries to resolve the physical file path corresponding to the given [[SourceRef source reference]].
    *
    * A [[SourceRef]] normally contains an MMT logical path, e.g. `http://docs.omdoc.org/urtheories/lf.mmt#966.46.3:998.46.35`.
    * Using [[controller]] and [[Controller.backend.resolveLogical()]] this is resolved to a physical file path
    * (e.g., `<some user-specific path>/urtheories/source/lf.mmt`) hopefully pointing to the file in which
    * the content element for which `src` is the source references was declared.
    */
  private[mmt] def resolveSourceFilepath(src: SourceRef): Option[File] = {
    // TODO: The consecutive tries of resolving below are awkward, but necessary due to an MMT peculiarity (bug?)
    //       Concretely, if the element pointed to by src was *built* to mmt-omdoc, src.container happens to be
    //       a logical path as expected. If it was merely typechecked in-memory (e.g., via our LSP functionality),
    //       then src.container happens to be a physical path.
    val origin = controller.backend.resolveLogical(src.container) match {
      case x@Some(_) => x
      case None => controller.backend.resolvePhysical(File(src.container.toURL.getFile))
    }

    origin.map {
      case (originArchive, originFileparts) =>
        originArchive.root / archives.source.toString / FilePath(originFileparts)
    }
  }

  override def shutdown: Any = style match {
    case LocalStyle => scala.sys.exit()
    case _ =>
  }

  override def connect: Unit = client.log(s"Connected to MMT v${MMTSystem.version}.")

  private def findDocument(uri: String): Option[MMTFile] = {
    val target = File(uri).toURI
    documents.find {
      case (uri2, _) => target.toURL.sameFile(URI(uri2).toJava.toURL)
    } match {
      case Some((_, doc)) => Some(doc)
      case None =>
        client.logError(s"Internal error: file `${uri}` requested to build was not communicated to be" +
          s" open to LSP server. Open documents known to LSP server are: ${documents.keys.mkString(", ")}.")
        None
    }
  }

  @JsonNotification("mmt/typecheck")
  def typecheck(msg: BuildMessage): Unit = {
    val doc: MMTFile = findDocument(msg.uri).getOrElse(return)

    val docShortname = URI(doc.uri).path.last
    withProgress(msg, s"Typechecking ${docShortname}")(progress => {
      client.log(s"Typechecking `${doc.uri}`...")

      client.resetErrors(doc.uri)
      doc.Annotations.clear
      doc.reparse(Some {
        case Parsed(e) =>
          progress(1.0, s"Parsed ${e.path.name}?${e.name}")
        case Checked(e) =>
          progress(1.0, s"Checked ${e.path.name}?${e.name}")
        case Elaborated(e) =>
          progress(1.0, s"Elaborated ${e.path.name}?${e.name}")
        case _ => /* ignore */
      })

      client.log(s"Typechecking finished of `${doc.uri}`.")
      ((), s"Typechecking of $docShortname finished")
    })
  }
  @JsonNotification("mmt/build/mmt-omdoc")
  def buildMMTOmdoc(msg: BuildMessage): Unit = {
    val doc: MMTFile = findDocument(msg.uri).getOrElse(return)

    val docShortname = URI(doc.uri).path.last
    withProgress(msg, s"Building ${docShortname}")(progress => {
      client.log(s"Building `${doc.uri}` to mmt-omdoc...")

      val errors = mutable.ListBuffer[api.Error]()
      controller.build(File(msg.uri))(errors.append)
      client.resetErrors(doc.uri)
      client.documentErrors(doc, true, errors.toSeq: _*)

      client.log(s"Build finished of `${doc.uri}`!")
      ((), s"Build of $docShortname finished")
    })
  }

  @JsonNotification("mmt/shell/handleline")
  def handleLine(msg: HandleLineMessage): Unit = {
    controller.handleLine(msg.line)
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