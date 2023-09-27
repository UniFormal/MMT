package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api
import info.kwarc.mmt.api.{DPath, DefComponent, ErrorHandler, Level, OpenCloseHandler, Path, SourceError}
import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.archives.{BuildAll, BuildChanged, source}
import info.kwarc.mmt.api.documents.{DRef, Document, MRef}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMA, OMFOREIGN, OMS}
import info.kwarc.mmt.api.ontology.ULO
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.time.Time
import info.kwarc.mmt.api.utils.{File, JSONArray, JSONObject, JSONString, URI}
import info.kwarc.mmt.lsp.{AnnotatedDocument, ClientWrapper, LSPDocument, LSPServer}
import info.kwarc.mmt.stex.Extensions.{SHTMLContentManagement, Symbols}
import info.kwarc.mmt.stex.parsing.stex.HasAnnotations
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLParser, SemanticState}
import info.kwarc.mmt.stex.{FullsTeX, RusTeX, STeXParseError, TeXError}
import info.kwarc.rustex.Params
import org.eclipse.lsp4j.{InlayHintKind, SymbolKind}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

case class STeXLSPErrorHandler(eh : api.Error => Unit, cont: (Double,String) => Unit) extends OpenCloseHandler {
  override protected def addError(e: api.Error): Unit = eh(e)
  override def open: Unit = {}
  override def close: Unit = {}
}

class sTeXDocument(uri : String,override val client:ClientWrapper[STeXClient],override val server:STeXLSPServer) extends LSPDocument(uri, client, server) with AnnotatedDocument[STeXClient,STeXLSPServer] {
  private val thisdoc = this
  def params(progress : String => Unit) = new Params {
    private var files: List[String] = Nil
    private def prefix(pre:Option[String],s:String) : String = pre match {
      case Some(pre) => s.split('\n').map(s => pre + ": " + s).mkString("\n")
      case None => s
    }
    override def log(s: String): Unit = {}// client.log(prefix(Some("rustex-log"),s))
    override def write_16(s: String): Unit = client.log(prefix(Some("rustex-16"),s))
    override def write_17(s: String): Unit = client.log(prefix(Some("rustex-17"),s))
    override def write_18(s: String): Unit = client.log(prefix(Some("rustex-18"),s))
    override def write_neg_1(s: String): Unit = {}//client.log(prefix(Some("rustex--1"),s))
    override def write_other(s: String): Unit = {}//client.log(prefix(Some("rustex-other"),s))
    override def message(s: String): Unit = client.log(prefix(Some("rustex-msg"),s))
    def file_open(s: String): Unit = {
      files ::= s.trim.drop(1)
      progress(s.trim)
      //client.log(prefix(Some("rustex-file-open"),s))
    }
    def file_close(): Unit = {
      //client.log(prefix(Some("rustex-file-close"),files.head))
      files = files.tail
      files.headOption.foreach(progress)
    }
    val eh = STeXLSPErrorHandler(e => client.documentErrors(thisdoc,false,e),(_,s) => progress(s))
    def error(msg : String, stacktrace : List[(String, String)],files:List[(String,Int,Int)]) : Unit = {
      val (off1,off2,p) = _doctext.fullLine(files.head._2-1)
      val reg = SourceRegion(SourcePosition(off1,files.head._2,0),SourcePosition(off2,files.head._2,p))
      eh(TeXError(uri,msg,stacktrace,reg))
    }
    eh.open
  }

  def parsingstate(eh: STeXLSPErrorHandler) = {
    //val path = archive.flatMap(a => relfile.map(_.toFilePath.foldLeft(a.narrationBase)((a,b) => a / b))).getOrElse(URI(uri))
    new SemanticState(server.stexserver,server.stexserver.importRules,eh,dpath,_ => ()) // TODO add subgraph
  }

  var html:Option[HTMLNode] = None

  def buildFull() = { synchronized {
    client.resetErrors(uri)
    server.withProgress(uri + "-fullstex","Building " + uri.split('/').last, "full") { update =>
      val target = server.controller.extman.getOrAddExtension(classOf[FullsTeX],"fullstex")
      (target,archive,relfile) match {
        case (Some(t),Some(a),Some(f)) =>
          client.log("Building [" + a.id + "] " + f)
          val eh = STeXLSPErrorHandler(e => client.documentErrors(thisdoc, false, e), update)
          //val eh = STeXLSPErrorHandler(e => client.documentErrors(this,false, e), update)
          try {
            eh.open
            t.build(a, BuildChanged(), f.toFilePath, Some(eh))
          } catch {
            case e: Throwable =>
              client.log(e.getMessage)
          } finally {
            eh.close
          }
        case _ =>
          client.log("Building None!")
      }
      quickparse
      ((),"Done")
    }
  }}

  def buildWithDeps: List[Document] = {
    def collectInputrefs(doc:Document): List[DPath] = {
      doc.getDeclarations.collect {
        case d:Document => collectInputrefs(d)
        case d : DRef if d.target.last.endsWith(".omdoc") => List(d.target)
      }.flatten
    }
    this.synchronized {
      buildFull()
      server.controller.getO(this.dpath) match {
        case Some(doc:Document) =>
          import info.kwarc.mmt.api.ontology.SPARQL._
          import info.kwarc.mmt.api.ontology.RDFImplicits._
          doc :: collectInputrefs(doc).flatMap {d =>
            server.controller.backend.resolveLogical(d.uri) match {
              case None => return Nil
              case Some((a,ls)) =>
                val f = "file://" + ls.foldLeft(a / source)((p,s) => p / s).setExtension("tex").toString
                val nd = server.documents.synchronized {
                  server.documents.getOrElseUpdate(f, server.newDocument(f))
                }
                nd.buildWithDeps
            }
          }
        case _ =>
          client.logError("Build failed")
          Nil
      }
    }
  }

  def exportProblems(target:File): Unit = {
    Future {buildWithDeps}.andThen {
      case scala.util.Success(docs) if docs.nonEmpty =>
        val title = SHTMLContentManagement.getTitle(docs.head).map {t =>
          server.stexserver.present(t.toString)(None).toString
        }.getOrElse("<span></span>")
        val problems = docs.flatMap {d =>
          d.getDeclarations.collectFirst{case m:MRef => m.target} match {
            case Some(lmp) =>
              server.controller.getO(lmp) match {
                case Some(m:Theory) =>
                  m.getDeclarations.collect {
                    case c:Constant if c.rl.contains("problem") =>
                      c.df match {
                        case Some(OMA(OMS(Symbols.meta_problem), OMFOREIGN(node) :: Nil)) =>
                          server.stexserver.present(node.toString)(None)
                        case _ =>
                          client.logError("problem " + c.path + " has no content")
                          return
                      }
                  }
                case _ =>
                  client.logError(s"language module $lmp not found")
                  return
              }
            case _ =>
              client.logError("No language module found in " + d.path)
              return
          }
        }
        File.write(target,JSONObject(("title",JSONString(title)), "problems" -> JSONArray(problems.map(s => JSONString(s.toString)):_*)).toString)
      case _ => client.logError("Build failed")
    }
  }

  def buildHTML(): Unit = Future { this.synchronized {
    client.resetErrors(uri)
    this.file match {
      case Some(f) =>
        server.safely {
          server.withProgress(uri, "Building " + uri.split('/').last, "Building html... (1/2)") { update =>
            val pars = params(s => update(0,s))
            val html = RusTeX.parseString(f,doctext ,pars,List("c_stex_module_"))
            update(0, "Parsing HTML... (2/2)")
            //this.synchronized {
              val state = parsingstate(pars.eh)
              val newhtml = HTMLParser(html)(state)
              pars.eh.close
              val relman = server.controller.relman
              relman.extract(state.doc)(server.controller.depstore += _)
              state.doc.getModulesResolved(server.controller.globalLookup) foreach { mod =>
                if (!mod.isGenerated) relman.extract(mod)(server.controller.depstore += _)
              }
              client.log("html parsed")
              this.html = Some(server.stexserver.present(newhtml.toString)(None).get("body")()().head)
              ((), "Done")
            //}
          }
          val msg = new HTMLUpdateMessage
          msg.html = (server.localServer / (":" + server.htmlserver.get.pathPrefix) / "fulldocument").toString + "?" + LSPServer.URItoVSCode(uri) // uri
          this.client.client.updateHTML(msg)
          quickparse
        }
      case _ =>
    }
  }}

  override val timercount: Int = 0
  override def onChange(annotations: List[(Delta, DocAnnotation)]): Unit = {
    Annotations.notifyOnChange()
  }
  def quickparse = try this.synchronized { //val (t,_) = Time.measure {
    server.parser.synchronized {
      Annotations.clear
      import info.kwarc.mmt.stex.parsing._
      val ret = server.parser(_doctext, file.getOrElse(File(uri)), archive)
      server.nermodel.foreach {model =>
        val tks = model.predict(ret)
        tks._1.foreach {res =>
          val subs = res.res.filter(_.score >= server.nermodel.ner_threshold)
          subs.foreach { sub =>
            val start = _doctext.toLC(sub.start)
            val end = _doctext.toLC(sub.end)
            client.documentErrors(this,true,SourceError(uri,SourceRef(URI(uri),
              SourceRegion(SourcePosition(sub.start,start._1,start._2),
                SourcePosition(sub.end,end._1,end._2)
              )),"Should possibly be a symbol reference: " + sub.str,level = Level.Info))
          }
        }
      }
      ret.foreach(_.iterate { elem =>
        elem match {
          case t: HasAnnotations =>
            t.doAnnotations(this)
          case _ =>
        }
        elem.errors.foreach { e =>
          val start = _doctext.toLC(elem.startoffset)
          val (eo,end) = elem match {
            case e : Environment => (e.header.last.endoffset,_doctext.toLC(e.header.last.endoffset))
            case _ => (elem.endoffset,_doctext.toLC(elem.endoffset))
          }
          client.documentErrors( this, true,SourceError(uri, SourceRef(URI(uri),
            SourceRegion(
              SourcePosition(elem.startoffset, start._1, start._2),
              SourcePosition(eo, end._1, end._2)
            )), e.shortMsg, if (e.extraMessage != "") List(e.extraMessage) else Nil, e.level)
          )
        }
      })
      super.onUpdate(Nil)
    }/* }
    println("Parsing took: " + t)
    print("")*/
  } catch {
    case e: Throwable =>
      e.printStackTrace()
      print("")
  }
  override def onUpdate(changes: List[Delta]): Unit = {
    client.resetErrors(uri)
    quickparse
  }

}
