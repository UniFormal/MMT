package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api
import info.kwarc.mmt.api.{DPath, ErrorHandler, Level, OpenCloseHandler, SourceError}
import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.archives.{BuildAll, BuildChanged}
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.{File, URI}
import info.kwarc.mmt.lsp.{AnnotatedDocument, ClientWrapper, LSPDocument}
import info.kwarc.mmt.stex.Extensions.DocumentExtension
import info.kwarc.mmt.stex.xhtml.HTMLParser.HTMLNode
import info.kwarc.mmt.stex.xhtml.{HTMLParser, SemanticState}
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

class sTeXDocument(uri : String,val client:ClientWrapper[STeXClient],val server:STeXLSPServer) extends LSPDocument(uri, client, server) with AnnotatedDocument[STeXClient,STeXLSPServer] {

  lazy val archive = file.flatMap(f => server.controller.backend.resolvePhysical(f).map(_._1))
  lazy val relfile = archive.map(a => (a / info.kwarc.mmt.api.archives.source).relativize(file.get))

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
    val eh = STeXLSPErrorHandler(e => client.documentErrors(server.controller,doctext,uri,e),(_,_) => {})
    def error(msg : String, stacktrace : List[(String, String)],files:List[(String,Int,Int)]) : Unit = {
      val (off1,off2,p) = LSPDocument.fullLine(files.head._2-1,doctext)
      val reg = SourceRegion(SourcePosition(off1,files.head._2,0),SourcePosition(off2,files.head._2,p))
      eh(TeXError(uri,msg,stacktrace,reg))
    }
    eh.open
  }




  def parsingstate(eh: STeXLSPErrorHandler) = {
    val extensions = server.stexserver.extensions
    val rules = extensions.flatMap(_.rules)
    new SemanticState(server.controller,rules,eh,DPath(URI(uri)))
  }

  var html:Option[HTMLNode] = None

  def buildFull() = Future {
    client.resetErrors(uri)
    server.withProgress(uri + "-fullstex","Building " + uri.split('/').last, "full") { update =>
      val target = server.controller.extman.getOrAddExtension(classOf[FullsTeX],"fullstex")
      (target,archive,relfile) match {
        case (Some(t),Some(a),Some(f)) =>
          client.log("Building [" + a.id + "] " + f)
          val eh = STeXLSPErrorHandler(e => client.documentErrors(server.controller,doctext, uri, e), update)
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
      ((),"Done")
    }
  }

  def buildHTML(): Unit = {
    client.resetErrors(uri)
    this.file match {
      case Some(f) =>
        Future { server.safely {
          server.withProgress(uri, "Building " + uri.split('/').last, "Building html... (1/2)") { update =>
            val pars = params(update(0,_))
            val html = RusTeX.parse(f, pars,List("c_stex_module_"))
            update(0, "Parsing HTML... (2/2)")
            this.synchronized {
              val newhtml = HTMLParser(html)(parsingstate(pars.eh))
              pars.eh.close
              client.log("html parsed")
              //try {
                server.stexserver.doHeader(newhtml)

                val exts = server.stexserver.extensions
                val docrules = exts.collect {
                  case e: DocumentExtension =>
                    e.documentRules
                }.flatten

                def doE(e: HTMLNode): Unit = docrules.foreach(r => r.unapply(e))

                newhtml.iterate(doE)
                this.html = Some(newhtml)
                ((), "Done")
              /*} catch {
                case t: Throwable =>
                  t.printStackTrace()
                  client.log("Error: " + t.getMessage)
                  ((), "Failed")
              }*/
            }
          }
          val msg = new HTMLUpdateMessage
          msg.html = (server.localServer / (":" + server.lspdocumentserver.pathPrefix) / "fulldocument").toString + "?" + uri // uri
          this.client.client.updateHTML(msg)
        }}
      case _ =>
    }
  }

  override val timercount: Int = 0
  override def onChange(annotations: List[(Delta, Annotation)]): Unit = {}
  override def onUpdate(changes: List[Delta]): Unit = try this.synchronized { server.parser.synchronized {
    Annotations.clear
    client.resetErrors(uri)
    import info.kwarc.mmt.stex.parsing._
    val ret = server.parser(doctext,file.getOrElse(File(uri)),archive)
    ret.foreach(_.iterate{ elem =>
      elem.errors.foreach { e =>
        val start = LSPDocument.toLC(elem.startoffset,doctext)
        val end = LSPDocument.toLC(elem.endoffset,doctext)
        client.documentErrors(server.controller,doctext,uri,SourceError(uri,SourceRef(URI(uri),
          SourceRegion(
            SourcePosition(elem.startoffset,start._1,start._2),
            SourcePosition(elem.endoffset,end._1,end._2)
          )),e.shortMsg,if (e.extraMessage != "") List(e.extraMessage) else Nil,e.level)
        )
      }
      elem match {
        case t : HasAnnotations =>
          t.doAnnotations(this)
        case _ =>
      }
    })
    super.onUpdate(changes)
    if (timercount > 0) Annotations.notifyOnChange(client.client)
  }} catch {
    case e : Throwable =>
      e.printStackTrace()
      print("")
  }

}
