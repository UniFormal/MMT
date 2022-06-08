package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api
import info.kwarc.mmt.api.{DPath, ErrorHandler}
import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRegion}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lsp.{AnnotatedDocument, ClientWrapper, LSPDocument}
import info.kwarc.mmt.stex.Extensions.DocumentExtension
import info.kwarc.mmt.stex.xhtml.HTMLParser.HTMLNode
import info.kwarc.mmt.stex.xhtml.{HTMLParser, SemanticState}
import info.kwarc.mmt.stex.{RusTeX, TeXError}
import info.kwarc.rustex.Params

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

class sTeXDocument(uri : String,client:ClientWrapper[STeXClient],server:STeXLSPServer) extends LSPDocument(uri, client, server) with AnnotatedDocument[STeXClient,STeXLSPServer] {
  override def onChange(annotations: List[(Delta, Annotation)]): Unit = {}

  val params = new Params {
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
      files ::= s
      //client.log(prefix(Some("rustex-file-open"),s))
    }
    def file_close(): Unit = {
      //client.log(prefix(Some("rustex-file-close"),files.head))
      files = files.tail
    }
    def error(msg : String, stacktrace : List[(String, String)],files:List[(String,Int,Int)]) : Unit = {
      val (off1,off2,p) = LSPDocument.fullLine(files.head._2-1,doctext)
      val reg = SourceRegion(SourcePosition(off1,files.head._2,0),SourcePosition(off2,files.head._2,p))
      client.documentErrors(doctext,uri,TeXError(uri,msg,stacktrace,reg))
    }
  }

  lazy val errorCont = new ErrorHandler {
    override protected def addError(e: api.Error): Unit = client.documentErrors(doctext,uri,e)
  }

  def parsingstate = {
    val extensions = server.stexserver.extensions
    val rules = extensions.flatMap(_.rules)
    new SemanticState(server.controller,rules,errorCont,DPath(URI(uri)))
  }

  var html:Option[HTMLNode] = None

  def build(): Unit = {
    client.resetErrors(uri)
    this.file match {
      case Some(f) =>
        Future {
          server.withProgress(uri, "Building " + uri, "Building html... (1/2)") { update =>
            val html = RusTeX.parse(f, params)
            update(0, "Parsing HTML... (2/2)")
            this.synchronized {
              val newhtml = HTMLParser(html)(parsingstate)
              client.log("html parsed")
              try {
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
              } catch {
                case t: Throwable =>
                  t.printStackTrace()
                  client.log("Error: " + t.getMessage)
                  ((), "Failed")
              }
            }
          }
          val msg = new HTMLUpdateMessage
          try {
            client.log("baseURI: " + server.controller.server.get.baseURI)
          } catch {
            case t: Throwable =>
              client.log("Error: Server not running")
          }
          msg.html = (server.controller.server.get.baseURI / (":" + server.lspdocumentserver.pathPrefix) / "document").toString + "?" + uri // uri
          this.client.client.updateHTML(msg)
        }
      case _ =>
    }
  }

}
