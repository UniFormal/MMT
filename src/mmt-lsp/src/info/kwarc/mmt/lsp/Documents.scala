package info.kwarc.mmt.lsp
import scala.concurrent.Future

class LSPDocument[+A <: LSPClient,+B <: LSPServer[A]](val uri : String,client:A,server : B) {
  private var _doctext : String = ""
  def doctext = synchronized { _doctext }
  val timercount : Int = 0

  private val self = this

  private object Timer {
    private var timer = 0
    private var timerthread: Option[Future[Unit]] = None

    def reset = if (timercount == 0) updateNow else {
      synchronized {
        timer = 0
      }
      if (synchronized {
        timerthread.isEmpty
      }) {
        synchronized {
          timerthread = Some(Future {
            while (synchronized {
              timer < timercount && !busy
            }) {
              Thread.sleep(100)
              synchronized {
                timer += 1
              }
            }
            synchronized { timerthread = None }
            updateNow
          }(scala.concurrent.ExecutionContext.global))
        }
      }
    }
    var busy = false
  }

  private var _changes : List[(org.eclipse.lsp4j.Range,String)] = Nil

  def toOffset(line:Int,col:Int,text : String) = {
    var l = 0
    var offset = 0
    while (l < line && text.isDefinedAt(offset)) {
      if (text(offset) == '\n') {
        l += 1
      }
      offset += 1
    }
    offset + col
  }

  private def toLC(offset:Int,text : String) = {
    val before = text.take(offset)
    var lines : List[String] = Nil
    var curr = ""
    before.foreach { case '\n' =>
      lines ::= curr
      curr = ""
    case c => curr = curr + c
    }
    lines = (curr :: lines).reverse
    (lines.length-1,lines.last.length)
  }

  class Delta(_oldStart : Int, _oldEnd : Int, val oldText : String, _newStart : Int, _newEnd : Int, val newText : String) {

  }

  private def updateNow: Unit = {
    Timer.busy = true
    val deltas = synchronized {
      val olddoc = _doctext
      var _deltas : List[(Int,Int,Int,Int,String,String)] = Nil
      def merge(start : Int, end: Int, news : String) = {
        var oldstart = start
        var oldend = end
        val prev = _doctext take start
        val oldText = _doctext.slice(start,end)
        _doctext = prev + news + _doctext.drop(end)
        var add = true
        _deltas.map {
          case old@(os,oe,ns,ne,_,_) if ne < start =>
            val diff = (ne-ns) - (oe-os)
            oldstart = oldstart + diff
            oldend = oldend + diff
            old
          case (os,oe,ns,ne,ot,nt) if end < ns =>
            val diff = news.length - (end - start)
            (os,oe,ns + diff, ne + diff,ot,nt)
          case (os,oe,ns,ne,ot,nt) if ne == start =>
            add = false
            (os,oe+oldText.length,ns,ne+news.length,olddoc.slice(os,oe+oldText.length),_doctext.slice(ns,ne+news.length))
          case _ =>
            ???
        }
        if (add) _deltas ::= (oldstart,oldend,start,start+news.length,oldText,news)
      }
      val changes = {
        val ch = _changes
        _changes = Nil
        ch.reverse
      }
      changes.foreach { case (range,text) =>
        val cstart = toOffset(range.getStart.getLine, range.getStart.getCharacter,_doctext)
        val cend = toOffset(range.getEnd.getLine, range.getEnd.getCharacter,_doctext)
        merge(cstart,cend,text)
      }
      _deltas.reverse.map {
        case (os,oe,ns,ne,ot,nt) => new Delta(os,oe,ot,ns,ne,nt)
      }
    }

    onUpdate(deltas)
    // Timer.reset
    Timer.busy = false
  }

  def update(range: org.eclipse.lsp4j.Range ,text:String) = {
    synchronized { _changes ::= (range,text) }
    Timer.reset
  }

  protected def onUpdate(changes : List[Delta]) = {}

  def init(s:String) = {
    _doctext = s
    Timer.reset
  }















/*
  def toRange(sr:SourceRegion) = {
    val start = toLC(sr.start.offset)
    val end = toLC(sr.end.offset)
    val st = new org.eclipse.lsp4j.Position(start._1,start._2)
    val en = new org.eclipse.lsp4j.Position(end._1,end._2)
    new org.eclipse.lsp4j.Range(st,en)
  }




  case class Highlight(line: Int,char:Int,length:Int,cls:Int) {
    def <=(o : Highlight) = {
      if (line < o.line) true else if (line == o.line) {
        char <= o.char
      } else false
    }
  }

  object Highlight {
    def apply(sr:SourceRegion,cls:Int):Highlight = {
      val (line,char) = toLC(sr.start.offset) // if (sr.start.line>=0 && sr.start.column>=0) (sr.start.line,sr.start.column) else toLC(sr.start.offset)
      Highlight(line,char,sr.end.offset-sr.start.offset,cls)
    }
    def apply(offset:Int,length:Int,cls:Int):Highlight = {
      val (line,char) = toLC(offset)
      Highlight(line,char,length,cls)
    }
  }

  def semanticHighlight(ls : List[Highlight]):List[Int] = {
    val tdi = new VersionedTextDocumentIdentifier(uri,version)
    semanticHighlight(tdi,ls)
  }

  def semanticHighlight(doc:VersionedTextDocumentIdentifier, highs : List[Highlight]): List[Int] = {
    var lines = highs.sortBy(_.line)
    var result : List[List[Highlight]] = Nil
    while (lines.nonEmpty) {
      var index = lines.indexWhere(_.line != lines.head.line)
      if (index == -1) index = lines.length
      val split = lines.take(index)
      lines = lines.drop(index)
      result ::= split.sortBy(_.char)
    }
    var lastline = 0
    var lastchar = 0
    result.flatten.sortWith((p,q) => p<=q).flatMap {
      case Highlight(line,char,length,cls) =>
        val nchar = if (line == lastline) (char - lastchar) else char
        val r = List(line-lastline,nchar,length,cls,0)
        lastline = line
        lastchar = char
        r
    }
    //val highlights = result.reverse.map(ls => new SemanticHighlightingInformation(ls.head.line,SemanticHighlightingTokens.encode(ls.map(_.token).asJava)))
    // val highlights = new SemanticHighlightingInformation(line,SemanticHighlightingTokens.encode(ls.asJava))
    //val params = new SemanticHighlightingParams()
    //params.setTextDocument(doc)
    //params.setLines(highlights.asJava)
    //client.semanticHighlighting(params)
  }

 */


}