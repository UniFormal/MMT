package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.lsp.{AnnotatedDocument, ClientWrapper, LSPDocument}
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
    override def log(s: String): Unit = client.log(prefix(Some("rustex-log"),s))
    override def write_16(s: String): Unit = client.log(prefix(Some("rustex-16"),s))
    override def write_17(s: String): Unit = client.log(prefix(Some("rustex-17"),s))
    override def write_18(s: String): Unit = client.log(prefix(Some("rustex-18"),s))
    override def write_neg_1(s: String): Unit = client.log(prefix(Some("rustex--1"),s))
    override def write_other(s: String): Unit = client.log(prefix(Some("rustex-other"),s))
    override def message(s: String): Unit = client.log(prefix(Some("rustex-msg"),s))
    def file_open(s: String): Unit = {
      files ::= s
      client.log(prefix(Some("rustex-file-open"),s))
    }
    def file_close(): Unit = {
      client.log(prefix(Some("rustex-file-close"),files.head))
      files = files.tail
    }
    def error(msg : String, stacktrace : List[(String, String)]) : Unit = {
      client.documentErrors(doctext,uri,TeXError(msg,stacktrace))
    }
  }

  server.mathhub_top match {
    case Some(f) => Future { RusTeX.initializeBridge(f / ".rustex") }
  }
  def build(): Unit = {
    this.file match {
      case Some(f) =>
        Future {
          val html = RusTeX.parse(f, params)
          val msg = new HTMLUpdateMessage
          msg.html = html
          this.client.client.updateHTML(msg)
        }
      case _ =>
    }
  }

}
