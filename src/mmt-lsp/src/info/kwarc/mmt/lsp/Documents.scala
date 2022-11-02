package info.kwarc.mmt.lsp
import info.kwarc.mmt.api.utils.{File, URI}
import org.eclipse.lsp4j.{InlayHintKind, SymbolKind}

import scala.collection.mutable
import scala.concurrent.Future

trait SyncedDocUnparsed {
  def offset : Int
  def LineChar : (Int,Int)
  def trim: Unit
  def empty : Boolean
  def first : Char
  def drop(i : Int) : Unit
  def drop(s : String): Unit
  def startsWith(s : String) : Boolean
  def next(): Char
  def takeWhile(f : Char => Boolean): String
  def takeWhileSafe(f: Char => Boolean): String = takeWhile(f)
  def getnext(i : Int):String
}

class SyncedDocument {
  private class LocalSyncedDocUnparsed extends SyncedDocUnparsed {
    var _offset = 0
    var _line = 0
    var currline = lines.headOption
    var _char = 0
    def offset = _offset
    override def LineChar: (Int, Int) = (_line+1,_char+1)

    override def empty: Boolean = currline.isEmpty

    override def first: Char = currline match {
      case Some(line) if _char < line.line.length => line.line(_char)
      case Some(line) if _char == line.line.length => '\n'
    }
    def startsWith(s : String): Boolean = {
      if (s.isEmpty) return true
      var nchar = _char
      var nline = _line
      var str = s
      var ln = if (lines.isDefinedAt(_line)) lines(_line) else return false
      while (str.nonEmpty) {
        if (ln.line.isDefinedAt(nchar)) {
          if (ln.line(nchar) == str.head) {
            nchar += 1
            str = str.tail
          } else return false
        } else if (ln.line.length == nchar) {
          if (str.head == '\n') {
            str = str.tail
            if (str.isEmpty) return true
            nline += 1
            nchar = 0
            if (lines.isDefinedAt(nline)) {
              ln = lines(nline)
            } else return false
          } else return false
        }
      }
      true
    }
    def trim = while (!empty && first.isWhitespace) advance // TODO
    def drop(i : Int) = (1 to i).foreach(_ => advance)

    def takeWhile(f: Char => Boolean): String = {
      val sb = new mutable.StringBuilder()
      while (!empty && f(first)) {
        sb.addOne(next())
      }
      sb.toString()
    }

    def getnext(i: Int): String = {
      var nchar = _char
      var nline = _line
      var j = i
      val sb = new mutable.StringBuilder
      var ln = if (lines.isDefinedAt(_line)) lines(_line) else return ""
      while (j > 0) {
        if (ln.line.isDefinedAt(nchar)) {
          sb.addOne(ln.line(nchar))
          nchar += 1
          j -= 1
        } else if (ln.line.length == nchar) {
          sb.addOne('\n')
          nchar = 0
          nline += 1
          j -= 1
          if (lines.isDefinedAt(nline)) {
            ln = lines(nline)
          } else return sb.toString()
        }
      }
      sb.toString()
    }

    def drop(s:String) = {
      if (!startsWith(s)) {
        print("")
      }
      drop(s.length)
    }
    def advance = currline match {
      case Some(line) =>
      if (_char < line.line.length) {
        _char += 1
        _offset += 1
      } else {
        _line += 1
        _offset = line.off
        currline = if (lines.isDefinedAt(_line)) Some(lines(_line)) else None
        _char = 0
      }
    }
    def next() = {
      val n = first
      advance
      n
    }

  }
  def unparsed : SyncedDocUnparsed = new LocalSyncedDocUnparsed
  private class Line(var off : Int,var line : String,var end:String) {
    def full = line + end

    override def toString: String = off.toString + ": " + line + end
  }
  //                      endoffset, line, lineending
  private var lines : List[Line] = Nil
  def getText = lines.map(_.line).mkString("\n")

  private def doLines(s : String, add : Line => Unit): Unit ={
    var curro = 0
    var currline = new mutable.StringBuilder()
    while (s.isDefinedAt(curro)) {
      val next = s(curro)
      if (next == '\r' && s.isDefinedAt(curro + 1) && s(curro + 1) == '\n') {
        curro += 2
        add(new Line(curro, currline.toString(), "\r\n"))
        currline = new mutable.StringBuilder()
      } else if (next == '\r' || next == '\n') {
        curro += 1
        add(new Line(curro, currline.toString(), next.toString))
        currline = new mutable.StringBuilder()
      } else {
        currline.addOne(next)
        curro += 1
      }
    }
    add(new Line(curro, currline.toString(), ""))
  }
  def set(s : String) = this.synchronized {
    doLines(s,lines ::= _)
    lines = lines.reverse
  }
  def toOffset(line:Int,col:Int) : Int = this.synchronized{
    if (lines.isDefinedAt(line)) {
      val ln = lines(line)
      val full = ln.full
      if (full.isDefinedAt(col)) {
        ln.off - full.drop(col).length
      } else ln.off
    } else {
      lines.lastOption.map(_.off).getOrElse(0)
    }
  }

  def toLC(offset:Int) : (Int,Int) = this.synchronized {
    var line = 0
    var lastoff = 0
    while (lines.isDefinedAt(line) && lines(line).off < offset) {{
      lastoff = lines(line).off
      line += 1
    }}
    if (lines.isDefinedAt(line)) {
      val ln = lines(line)
      if (ln.off == offset) return (line+1,0)
      val full = ln.full
      val idx = offset - lastoff
      (line,if (full.isDefinedAt(idx)) idx else full.length)
    } else (lines.length-1,lines.lastOption.map(_.line.length-1).getOrElse(0))
  }

  def change(startl:Int,startc:Int,endl:Int,endc:Int, newtext:String) = this.synchronized {
    val (slineidx,sline) = if (lines.isDefinedAt(startl)) (startl,lines(startl))
      else lines.lastOption.map((lines.length-1,_)).getOrElse{
      val nl = new Line(0,"","")
      lines = lines ::: List(nl)
      (0,nl)
    }
    val (elineidx,eline) = if (lines.isDefinedAt(endl)) (endl, lines(endl))
    else lines.lastOption.map((lines.length - 1, _)).getOrElse {
      val nl = new Line(0, "", "")
      lines = lines ::: List(nl)
      (0, nl)
    }
    val prev = sline.line.take(startc)
    val suff = eline.full.drop(endc)
    var newlines : List[Line] = Nil //lines.take(slineidx)
    val s = prev + newtext + suff
    doLines(s,newlines ::= _)
    if (newlines.headOption.exists(_.line.isEmpty)) {newlines = newlines.tail}
    val addoffsets = if (lines.isDefinedAt(slineidx-1)) (lines(slineidx-1).off) else 0
    newlines.foreach(_.off += addoffsets)
    val nlines = lines.take(slineidx) ::: newlines.reverse
    val elines = lines.drop(elineidx+1)
    elines.foreach(_.off += (nlines.last.off - eline.off))
    lines = nlines ::: elines
  }

  def fullLine(line:Int) = this.synchronized {
    val start = if (lines.isDefinedAt(line-2)) lines(line-2).off else 0
    val (end,p0) = if (lines.isDefinedAt(line-1)) {
      val ln = lines(line-1)
      (ln.off - ln.end.length,ln.line.length)
    } else (0,0)
    (start,end,p0)
  }

}

class LSPDocument[+A <: LSPClient,+B <: LSPServer[A]](val uri : String,protected val client:ClientWrapper[A],protected val server : B) {
  val _doctext = new SyncedDocument
  def doctext =  _doctext.getText
  val timercount : Int = 0
  lazy val file : Option[File] = {
    val f = File(URI(uri).pathAsString)
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

  case class Delta(oldStart : Int, oldEnd : Int, newText : String)

  protected def updateNow: Unit = {
    Timer.busy = true
    val deltas = synchronized {
      val changes = {
        val ch = _changes
        _changes = Nil
        ch.reverse
      }
      changes.map { case (range,text) =>

        val start = _doctext.toOffset(range.getStart.getLine, range.getStart.getCharacter)
        val end = _doctext.toOffset(range.getEnd.getLine, range.getEnd.getCharacter)
        _doctext.change(range.getStart.getLine,range.getStart.getCharacter,range.getEnd.getLine, range.getEnd.getCharacter,text)
        Delta(start,end,text)
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
    _doctext.set(s)
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


    private[AnnotatedDocument] var _codeActions: List[CodeActionTrait] = Nil
    def getCodeActions = _codeActions
    def addCodeActionEdit(title:String,kind:String,edits:List[(String,List[(Int,Int,Int,Int,String)])]) =
      _codeActions ::= CodeActionEdit(title,kind,edits)

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
    def linechar = (_doctext.toLC(_offset), _doctext.toLC(_offset + length))
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
        val (line, char) = _doctext.toLC(a)
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
    def notifyOnChange() = {//[A <: LSPClient](client:ClientWrapper[A]) = {
      client.client.refreshSemanticTokens()
      client.client.refreshCodeLenses()
      client.client.refreshInlineValues()
      client.client.refreshInlayHints()
      client.republishErrors(uri)
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
          a.advance(d.newText.length - (d.oldEnd - d.oldStart))
        } else if (d.oldStart <= a.end) {
          a.stretch(d.newText.length - (d.oldEnd - d.oldStart))
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