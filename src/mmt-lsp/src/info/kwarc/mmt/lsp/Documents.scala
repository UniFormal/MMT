package info.kwarc.mmt.lsp
import info.kwarc.mmt.api.utils.File
import org.eclipse.lsp4j.{InlayHintKind, SymbolKind}

import scala.concurrent.Future

object LSPDocument {
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

  def fullLine(line:Int,text : String) = {
    var l = 0
    var offset0 = 0
    while (l < line && text.isDefinedAt(offset0)) {
      if (text(offset0) == '\n') {
        l += 1
      }
      offset0 += 1
    }
    var offset1 = offset0
    var p0 = 0
    while (l == line && text.isDefinedAt(offset1)) {
      if (text(offset1) == '\n') {
        l += 1
      }
      offset1 += 1
      p0 += 1
    }
    (offset0,offset1,p0)
  }

  def toLC(offset:Int,text : String) = {
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

}

class LSPDocument[+A <: LSPClient,+B <: LSPServer[A]](val uri : String,client:ClientWrapper[A],server : B) {
  private var _doctext : String = ""
  def doctext =  _doctext
  val timercount : Int = 0
  lazy val file : Option[File] = {
    val f = File({
      val str = uri.drop(7) // stupid windows fix
      if (str.length > 2 && str(2) == ':') {
        str.take(2).toUpperCase + str.drop(2)
      } else str
    })
    if (f.exists()) Some(f) else None
  }

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

  case class Delta(oldStart : Int, oldEnd : Int, oldText : String, newText : String)

  protected def updateNow: Unit = {
    Timer.busy = true
    val deltas = synchronized {
      val changes = {
        val ch = _changes
        _changes = Nil
        ch.reverse
      }
      changes.map { case (range,text) =>
        val start = LSPDocument.toOffset(range.getStart.getLine, range.getStart.getCharacter,_doctext)
        val end = LSPDocument.toOffset(range.getEnd.getLine, range.getEnd.getCharacter,_doctext)
        val prev = _doctext take start
        val oldText = _doctext.slice(start,end)
        _doctext = prev + text + _doctext.drop(end)
        Delta(start,end,oldText,text)
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
    updateNow
  }
}

trait AnnotatedDocument[+A <: LSPClient,+B <: LSPServer[A]] extends LSPDocument[A,B] {

  class Annotation(__offset : Int, __length : Int, val value : Any,
                   val symbolkind : SymbolKind = null,
                   val symbolname : String = "",
                   val foldable : Boolean = false) {
    private var _offset: Int = __offset
    private var _length = __length
    private[AnnotatedDocument] var _semantic: Option[(Int, List[Int])] = None
    private[AnnotatedDocument] var _hover: Option[Unit => String] = None
    private[AnnotatedDocument] var _parent: Option[Annotation] = None
    private[AnnotatedDocument] var _children: List[Annotation] = Nil
    def children = _children


    def remove = {
      _parent.foreach(p => p._children = p._children.filterNot(_ == this))
      _parent = None
    }
    private[AnnotatedDocument] var _inlays: List[(String,String,Option[InlayHintKind],Int,Boolean,Boolean)] = Nil
    def addInlay(label:String,tooltip:String="",kind:Option[InlayHintKind] = None,positionOffset:Int = this.__offset + this._length,padleft : Boolean=false,padright:Boolean=false) = {
      _inlays ::= (label,tooltip,kind,positionOffset,padleft,padright)
    }
    def getInlays = _inlays

    private[AnnotatedDocument] var _implementation: Option[(String,Int,Int)] = None
    def setImplementation(file:String,start:Int,end:Int) = _implementation = Some((file,start,end))
    def getImplementation = _implementation

    private[AnnotatedDocument] var _declaration: Option[(String,Int,Int)] = None
    def setDeclaration(file:String,start:Int,end:Int) = _declaration = Some((file,start,end))
    def getDeclaration = _declaration

    private[AnnotatedDocument] var _definitions: List[(String,Int,Int)] = Nil
    def addDefinition(file:String,start:Int,end:Int) = _definitions ::= (file,start,end)
    def getDefinitions = _definitions

    private[AnnotatedDocument] var _codelenses: List[(String,String,List[AnyRef],Int,Int)] = Nil
    def addCodeLens(title:String,commandname:String,arguments:List[AnyRef]=Nil,start:Int,end:Int) = _codelenses ::= (title,commandname,arguments,start,end)
    def getCodeLenses = _codelenses


    private[AnnotatedDocument] def advance(i: Int) = _offset += i
    private[AnnotatedDocument] def stretch(i: Int) = _length += i
    def linechar = (LSPDocument.toLC(_offset, doctext), LSPDocument.toLC(_offset + length, doctext))
    def setSemanticHighlightingClass(cls: Int, modifiers: List[Int] = Nil) = _semantic = Some((cls, modifiers))
    def setHover(f : => String) = _hover = Some(_ => f)

    def offset = _offset
    def length = _length
    def end = _offset + _length

    private def getRanges : List[(Int,Int)] = {
      var rgs = List((_offset, end))
      _children.foreach { ch =>
        rgs = rgs.flatMap {
          case (a, b) if a <= ch.offset && ch.end <= b =>
            (if (a == ch.offset) Nil else List((a, ch.offset))) :::
              (if (b == ch.end) Nil else List((ch.end, b)))
          case p => List(p)
        }
      }
      rgs
    }

    private[lsp] def getHovers: List[(Int,Int,Unit => String)] = if (_hover.isEmpty) Nil else {
      getRanges.map { case (a,b) =>
        val f = _hover.get
        (a,b,f)
      }
    }

    private[lsp] def getHighlights: List[(Int,Int,Int,Int,List[Int])] = if (_semantic.isEmpty) Nil else {
      getRanges.map { case (a, b) =>
        val (line, char) = LSPDocument.toLC(a, doctext)
        val (scope, modifiers) = _semantic.get
        (line, char, b - a, scope, modifiers)
      }
    }
  }

  def onChange(annotations : List[(Delta,Annotation)]) : Unit

  override protected def onUpdate(changes: List[Delta]): Unit = {
    super.onUpdate(changes)
    synchronized{ Annotations.update(changes) }
  }

  object Annotations {
    def clear = _annotations = Nil
    def notifyOnChange(client:LSPClient) = {
      client.refreshSemanticTokens()
      client.refreshCodeLenses()
      client.refreshInlineValues()
      client.refreshInlayHints()
    }
    private var _annotations : List[Annotation] = Nil
    def getAll = _annotations
    def add(value : Any, offset : Int, length: Int,symbolkind : SymbolKind = null, symbolname : String = "", foldable : Boolean = false) : Annotation = {
      val a = new Annotation(offset,length,value,symbolkind,symbolname,foldable)
      add(a)
      a
    }
    def update(deltas : List[Delta]) = {
      deltas.foreach{d => _annotations.foreach {a =>
        if (d.oldEnd <= a.offset) {
          a.advance(d.newText.length - d.oldText.length)
        } else if (d.oldStart <= a.end) {
          a.stretch(d.newText.length - d.oldText.length)
        }
      }}
      val changed = deltas.flatMap(d => _annotations.find(a =>
        a.offset <= d.oldStart && a.end >= d.oldEnd
      ).map((d,_))).distinct
      onChange(changed)
    }
    def add(annotation : Annotation) : Unit = {
      _annotations.filter(a =>
        a.offset <= annotation.offset && a.end >= annotation.end
      ).sortBy(_.length).headOption match {
        case Some(p) if p.offset == annotation.offset && p.end == annotation.end =>
          annotation._parent = p._parent
          annotation._children = p._children
          return ()
        case Some(p) =>
          p._children ::= annotation
          annotation._parent = Some(p)
        case _ =>
      }
      _annotations.filter{ a =>
        annotation.offset <= a.offset && a.end >= annotation.end &&
          !a._children.contains(annotation)
      }.foreach {a =>
        a.remove
        a._parent = Some(annotation)
        annotation._children ::= a
      }
      _annotations ::= annotation
      _annotations = _annotations.sortWith((a,b) => a.offset < b.offset || (a.offset == b.offset && a.length > b.length))
    }
  }
}