package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.lsp.{AnnotatedDocument, ClientWrapper, LSPDocument}
import info.kwarc.mmt.stex.RusTeX
import info.kwarc.rustex.Params

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

class sTeXDocument(uri : String,client:ClientWrapper[STeXClient],server:STeXLSPServer) extends LSPDocument(uri, client, server) with AnnotatedDocument[STeXClient,STeXLSPServer] {
  override def onChange(annotations: List[(Delta, Annotation)]): Unit = {}

  val params = new Params {
    def prefix(pre:Option[String],s:String) : String = pre match {
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
    override def file_clopen(s: String): Unit = client.log(prefix(Some("rustex"),s))
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
