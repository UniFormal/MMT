package info.kwarc.mmt.api.parser



/*

import info.kwarc.mmt.api.{ComponentContainer, DPath, DeclarationComponent, Path, SourceError, StructuralElement}
import info.kwarc.mmt.api.documents.{Document, MRef, Namespace, NamespaceImport, SRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.Generated
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.notations.{NotationContainer, TextNotation}
import info.kwarc.mmt.api.objects.{Obj, Term}
import info.kwarc.mmt.api.opaque.{ObjFragment, OpaqueElement, OpaqueText, ScopeFragment, StringFragment, TextFragment}
import info.kwarc.mmt.api.parser.Reader.{GS, RS, US}
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration, Include, NestedModule, ObjContainer, Structure, TermContainer}

abstract class AnnotatedText {
  private var _children :List[Child] = Nil
  def region : SourceRegion
  def start = region.start
  def end = region.end
  def text : String
  def children : List[Child] = _children.reverse
  def parent : Option[AnnotatedText]
  val keys:List[String] = Nil

  def asSequence:List[Child] = if (_children.isEmpty) this match {
    case c : Child => List(c)
    case _ => Nil
  } else children.flatMap(_.asSequence)

  def addChild(at : Child) : Unit = {
    if (!this.region.contains(at.region)) return ()
    val nStart = if (_children.nonEmpty) {
      if(_children.head.end <= at.start)
        _children.head.end
      else return ()
    } else this.start
    if (nStart.offset != at.start.offset) {
      val ws = text.substring(nStart.offset-this.start.offset,at.start.offset-this.start.offset)
      doOthers(ws,nStart)
    }
    _children ::= at
  }

  def doEnd: Unit = {
    val nStart = if (_children.nonEmpty) _children.head.end else this.start
    if (nStart.offset != end.offset) {
      val ws = text.substring(nStart.offset-this.start.offset,end.offset-this.start.offset)
      doOthers(ws,nStart)
    }
  }

  protected def doOthers(ninput : String,at:SourcePosition):Unit = {
    var input = ninput
    if (input == "") return
    var off = input.indexWhere(!_.isWhitespace)
    if (off == -1) off = input.length
    if (off!=0) _children ::= new Whitespace(at,input.take(off),this)
    input = input.drop(off)
    if (input == "") return
    input.head match {
      case c if c == GS.chars.head =>
        _children ::= new ModuleDelimiter(at+off,this)
        doOthers(input.drop(1),at + off+1)
      case c if c == RS.chars.head =>
        _children ::= new DeclarationDelimiter(at+off,this)
        doOthers(input.drop(1),at + off+1)
      case c if c == US.chars.head =>
        _children ::= new ObjectDelimiter(at+off,this)
        doOthers(input.drop(1),at + off+1)
      case '/' =>
        var index = input.lastIndexWhere(List(GS.chars.head,RS.chars.head,US.chars.head).contains)
        if (index == -1) index = input.length
        _children ::= new AnnotatedComment(at+off,input.take(index),this)
        doOthers(input.drop(index),at + (off + index))
      case _ if keys.exists(input.startsWith) =>
        val k = keys.find(input.startsWith).get
        _children ::= new AnnotatedKeyword(k,at+off,this)
        doOthers(input.drop(k.length),at + off + k.length)
      case _ if input.startsWith("role") =>
        _children ::= new AnnotatedKeyword("role",at+off,this)
        off += "role".length
        input = input.drop("role".length)
        var i = input.indexWhere(!_.isWhitespace)
        if (i>0) {
          _children ::= new Whitespace(at+off,input.take(i),this)
          input = input.drop(i)
          off += i
        }

        i = input.indexWhere(_.isWhitespace)
        if (i== -1) i = input.length
        val name = input.take(i)
        input = input.drop(i)
        _children ::= new AnnotatedName(name,at+off,this)
        off += i
        doOthers(input,at+off)
      case _ =>
        // _children ::= new ErrorText(SourceRegion(at+off,at+off+input.length),input,this)
    }
  }
}

trait Child extends AnnotatedText {
  val getParent : AnnotatedText
  override def parent: Option[AnnotatedText] = Some(getParent)
}

class Whitespace(at:SourcePosition,val text:String,val getParent : AnnotatedText) extends Child {
  def region = SourceRegion(at,at+text.length)
}
class AnnotatedCommentToken(at:SourcePosition, val text:String, val getParent : AnnotatedText) extends Child {
  def region = SourceRegion(at,at+text.length)
}

class ErrorText(val region: SourceRegion, val text:String, val getParent : AnnotatedText) extends Child {
  //def region = SourceRegion(at,at+text.length)
}

class AnnotatedComment(at:SourcePosition, val text:String, val getParent : AnnotatedText) extends Child {
  def region = SourceRegion(at,at+text.length)
  def init: Unit = {
    var int = text
    var off = 0
    while (int.nonEmpty) {
      val i = int.indexWhere(!_.isWhitespace)
      if (i == -1) int = ""
      else {
        int = int.drop(i)
        off += i
        var j = int.indexWhere(_.isWhitespace)
        if (j == -1) j = int.length
        addChild(new AnnotatedCommentToken(at+off,int.take(j),this))
        int = int.drop(j)
        off += j
      }
    }
    doEnd
  }
  init
}

abstract class AnnotatedDelimiter(at:SourcePosition,char:Int) extends Child {
  def region = SourceRegion(at,at+1)
  def text = char.toChar.toString
}

class ModuleDelimiter(at:SourcePosition, val getParent:AnnotatedText) extends AnnotatedDelimiter(at,GS.chars.head)
class DeclarationDelimiter(at:SourcePosition, val getParent:AnnotatedText) extends AnnotatedDelimiter(at,RS.chars.head)
class ObjectDelimiter(at:SourcePosition, val getParent:AnnotatedText) extends AnnotatedDelimiter(at,US.chars.head)

class AnnotatedKeyword(val key:String,at:SourcePosition,val getParent:AnnotatedText) extends Child {
  def region = SourceRegion(at,at+key.length)
  def text = key
}

class AnnotatedName(val name:String,at:SourcePosition,val getParent : AnnotatedText) extends Child {
  def region = SourceRegion(at,at+name.length)
  def text = name
}

abstract class SemanticText extends AnnotatedText {
  def semantic : StructuralElement
}

class AnnotatedDocument(val semantic:Document,val region : SourceRegion, val text : String, val parent:Option[AnnotatedText] = None) extends SemanticText

class NamespaceDeclaration(val semantic:Namespace,val region: SourceRegion, val text : String, val getParent:AnnotatedText) extends SemanticText with Child {

  private def init {
    assert(text.startsWith("namespace"))
    var it = text.drop("namespace".length)
    addChild(new AnnotatedKeyword("namespace", region.start, this))
    val si = it.indexWhere(!_.isWhitespace)
    doOthers(it.take(si), region.start + "namespace".length)
    it = it.drop(si)
    val nit = it.indexWhere(_.isWhitespace)
    val rest = if (it.isDefinedAt(nit)) it.drop(nit) else ""
    addChild(new AnnotatedDPath(semantic.namespace.doc,SourceRegion(region.start+"namespace".length+si,region.end-rest.length),it.dropRight(rest.length),this))
    doOthers(rest,region.end-rest.length)
  }
  init
}

class NSImportDeclaration(val semantic:NamespaceImport,val region: SourceRegion, val text : String, val getParent:AnnotatedText) extends SemanticText with Child {

  private def init {
    var off = 0
    assert(text.startsWith("import"))
    var it = text.drop("import".length)
    off += "import".length
    addChild(new AnnotatedKeyword("import", region.start, this))
    val si1 = it.indexWhere(!_.isWhitespace)
    doOthers(it.take(si1), region.start + off)
    it = it.drop(si1)
    off += si1

    assert(it.startsWith(semantic.prefix))
    it = it.drop(semantic.prefix.length)
    addChild(new AnnotatedName(semantic.prefix,region.start+off,this))
    off += semantic.prefix.length

    val si2 = it.indexWhere(!_.isWhitespace)
    doOthers(it.take(si2),region.start+off)
    it = it.drop(si2)
    off += si2

    val nit = it.indexWhere(_.isWhitespace)
    val rest = if (it.isDefinedAt(nit)) it.drop(nit) else ""
    addChild(new AnnotatedDPath(semantic.namespace.doc,SourceRegion(region.start+off,region.end-rest.length),it.dropRight(rest.length),this))
    doOthers(rest,region.end-rest.length)
  }
  init
}

class AnnotatedOpaque(val semantic:OpaqueElement,val region: SourceRegion, val text : String, val getParent:AnnotatedText) extends SemanticText with Child

class AnnotatedOpaqueText(val semantic:OpaqueText, val region: SourceRegion, val text:String, val getParent:AnnotatedText) extends SemanticText with Child

class AnnotatedConstant(val semantic:Constant, val region: SourceRegion, val text:String, val getParent:AnnotatedText) extends SemanticText with Child {
  override val keys: List[String] = List(":","=","#","##","@")

  private def init {
    var off = 0
    var it = text
    if (text.startsWith("constant")) {
      it = text.drop("constant".length)
      off += "constant".length
      addChild(new AnnotatedKeyword("constant", region.start, this))
      val si1 = it.indexWhere(!_.isWhitespace)
      doOthers(it.take(si1), region.start + off)
      it = it.drop(si1)
      off += si1
    }

    val name = semantic.name.last.toString
    assert(it.startsWith(name))
    it = it.drop(name.length)
    addChild(new AnnotatedName(name,region.start+off,this))
    off += name.length

    val si1 = it.indexWhere(!_.isWhitespace)
    doOthers(it.take(si1),region.start+off)
  }
  init
}

abstract class AnnotatedPath extends AnnotatedText with Child {
  val path:Path
}
class AnnotatedDPath(val path:DPath, val region:SourceRegion, val text:String, val getParent:AnnotatedText) extends AnnotatedPath

class AnnotatedTheory(val semantic:Theory, val region: SourceRegion, val text : String, val getParent:AnnotatedText) extends SemanticText with Child {
  private def init {
    assert(text.startsWith("theory"))
    var off = "theory".length
    var it = text.drop(off)
    addChild(new AnnotatedKeyword("theory", region.start, this))
    var si1 = it.indexWhere(!_.isWhitespace)
    doOthers(it.take(si1), region.start + off)
    it = it.drop(si1)
    off += si1

    val name = semantic.name.last.toString
    assert(it.startsWith(name))
    it = it.drop(name.length)
    addChild(new AnnotatedName(name,region.start+off,this))
    off += name.length
    si1 = it.indexWhere(!_.isWhitespace)
    doOthers(it.take(si1), region.start + off)
    it = it.drop(si1)
    off += si1

    semantic.metaC.get match {
      case Some(tm) =>
        assert(it.startsWith(":"))
        it = it.drop(1)
        addChild(new AnnotatedKeyword(":",region.start+off,this))
        off += 1
        si1 = it.indexWhere(!_.isWhitespace)
        doOthers(it.take(si1), region.start + off)
        it = it.drop(si1)
        off += si1

        val nref = SourceRef.get(tm) match {
          case Some(r) => r.region
          case _ => // need to guess meta theory here
            SourceRegion(region.start + off,region.start + off + it.indexWhere(c => c.isWhitespace || c == '>' || c == '='|| c == US.chars.head))
        }
        val ntext = text.substring(nref.start.offset-region.start.offset,nref.end.offset-region.start.offset)
        addChild(new AnnotatedTerm(semantic.metaC,nref,ntext,this))
        off += ntext.length
        it = it.drop(ntext.length)
        si1 = it.indexWhere(!_.isWhitespace)
        doOthers(it.take(si1), region.start + off)
        it = it.drop(si1)
        off += si1
      case None =>
    }
    if(it.startsWith("=") || it.startsWith(">")) { // TODO or abbrev?
      it.head match {
        case '=' =>
          addChild(new AnnotatedKeyword("=", region.start + off, this))
        case '>' =>
          addChild(new AnnotatedKeyword(">", region.start + off, this))
          val params = semantic.paramC
          it = it.tail
          off += 1
          si1 = it.indexWhere(!_.isWhitespace)
          doOthers(it.take(si1), region.start + off)
          it = it.drop(si1)
          off += si1

          si1 = it.indexOf(US.chars.head)
          val partx = it.take(si1).trim
          addChild(new AnnotatedTerm(params, SourceRegion(region.start + off, region.start + off + partx.length), partx, this))
          it = it.drop(partx.length)
          off += partx.length

          si1 = it.indexOf('=')
          doOthers(it.take(si1), region.start + off)
          off += si1
          addChild(new AnnotatedKeyword("=", region.start + off, this))
        case _ =>
          ???
      }
    }
  }
  init
}

class AnnotatedTerm[A<:Obj](val semantic:ObjContainer[A], val region: SourceRegion, val text : String, val getParent:AnnotatedText) extends Child {
  def init(txt : String,at:SourcePosition) {
    var it = txt
    val off = txt.indexWhere(!_.isWhitespace)
    if (off == -1) doEnd else {
      it = it.drop(off)
      var i = it.indexWhere(_.isWhitespace)
      if (i == -1) i = it.length
      addChild(new AnnotatedTermToken(at+off,it.take(i),this))
      init(it.drop(i),at+off+i)
    }
  }
  init(text,region.start)
}
class AnnotatedTermToken(val at: SourcePosition, val text : String, val getParent:AnnotatedText) extends Child {
  def region = SourceRegion(at,at+text.length)
}

class AnnotatedNotation(val semantic:TextNotation, val region: SourceRegion, val text : String, val getParent:AnnotatedText) extends Child

class AnnotatedInclude(val semantic:Structure, val region: SourceRegion, val text : String, val getParent:AnnotatedText) extends Child {

  private def init {
    var off = 0
    var it = text

    assert(it.startsWith("include"))
    it = it.drop("include".length)
    addChild(new AnnotatedKeyword("include",region.start+off,this))
    off += "include".length

    var si1 = it.indexWhere(!_.isWhitespace)
    doOthers(it.take(si1),region.start+off)

    val nref = SourceRef.get(semantic.from) match {
      case Some(r) => r.region
      case _ =>
        ???
    }
    val ntext = text.substring(nref.start.offset-this.start.offset,nref.end.offset-this.start.offset)
    addChild(new AnnotatedTerm(semantic.tpC,nref,ntext,this))
    it = it.drop(ntext.length)
    off += ntext.length

    si1 = it.indexWhere(!_.isWhitespace)
    doOthers(it.take(si1),region.start+off)
  }
  init
}

class AnnotatedDerived(val semantic:DerivedDeclaration, val region: SourceRegion, val text : String, val getParent:AnnotatedText) extends SemanticText with Child {
  override val keys: List[String] = List("=")
  private def init {
    var off = 0
    var it = text

    assert(it.startsWith(semantic.feature))
    it = it.drop(semantic.feature.length)
    addChild(new AnnotatedKeyword(semantic.feature,region.start+off,this))
    off += semantic.feature.length

    if (semantic.tp.isDefined) {
      val isr = SourceRef.get(semantic.tp.get) match {
        case Some(ref) => ref.region
        case _ =>
          ???
      }
      addChild(new AnnotatedTerm(semantic.tpC,isr,text.substring(isr.start.offset-start.offset,isr.end.offset-start.offset),this))
    }
  }
  init
}

object AnnotatedText {
  def fromDocument(doc:Document,input:String, errs : List[info.kwarc.mmt.api.Error])(implicit controller:Controller):AnnotatedText = {
    SourceRef.get(doc) match {
      case Some(sr) =>
        val start = sr.region.start
        assert(start.column == 0 && start.line == 0 && start.offset == 0)
        val end = sr.region.end // end.offset == input.length-2 for some reason!
        val ret = new AnnotatedDocument(doc,sr.region,input.substring(start.offset,end.offset))
        doIn(ret,input,doc.getDeclarations)
        errs.foreach(doError(ret,input,_))
        ret.doEnd
        ret
      case None =>
        ???
    }
  }

  private def doError(parent : AnnotatedText,input:String,err : info.kwarc.mmt.api.Error) = err match {
    case SourceError(_, ref, mainMessage, _, _) =>
      parent.addChild(new ErrorText(ref.region,mainMessage,parent))
    case _ =>
      ???
    //parent.addChild(new ErrorText(at+off,input,this))
  }

  private def doIn(parent : AnnotatedText,input : String,children : List[StructuralElement])(implicit controller:Controller): Unit = {
    if (children.isEmpty) return
    val head = children.head
    val ref = SourceRef.get(head) match {
      case Some(sr) => sr.region
      case None =>
        ???
    }
    val text = input.substring(ref.start.offset,ref.end.offset)
    head match {
      case n@Namespace(_,_) =>
        parent.addChild(new NamespaceDeclaration(n,ref,text,parent))
        doIn(parent,input,children.tail)
      case n@NamespaceImport(_,_,_) =>
        parent.addChild(new NSImportDeclaration(n,ref,text,parent))
        doIn(parent,input,children.tail)
      case th:Theory =>
        val td = new AnnotatedTheory(th,ref,text,parent)
        parent.addChild(td)
        val d = th.asDocument
        doIn(td,input,d.getPrimitiveDeclarations flatMap {
          case r:SRef =>
            if (!th.get(r.target.name).isGenerated) List(th.get(r.target.name)) else Nil
          case d => List(d)
        })
        td.doEnd
        doIn(parent,input,children.tail)
      case m:MRef if m.isGenerated =>
        controller.getAs(classOf[Module],m.target) match {
          case th:Theory if !th.isGenerated =>
            doIn(parent,input,th::children.tail)
          case _ =>
            ???
        }

      case t:OpaqueText =>
        val op = new AnnotatedOpaque(t,ref,text,parent)
        parent.addChild(op)
        var init = text
        var off = 0
        def flatten(tf : TextFragment) : List[Child] = tf match {
          case ScopeFragment(ls) => ls.flatMap(flatten)
          case StringFragment(str) =>
            val i = init.indexOf(str)
            off += i
            init = init.drop(i + str.length)
            val ret = List(new AnnotatedComment(ref.start+off,str,op))
            off += str.length
            ret
          case o: ObjFragment if o.tc.isInstanceOf[TermContainer] =>
            val tc = o.tc.asInstanceOf[TermContainer]
            val nref = SourceRef.get(tc.get.get) match {
              case Some(r) => r.region
              case _ =>
                ???
            }
            val tx = o.tc.read.get
            val i = init.indexOf(tx)
            val i1 = init.take(i).lastIndexOf('$')
            val i2 = i + tx.length + init.drop(i+tx.length).indexOf('$')+1
            val ret = List(new AnnotatedTerm(tc,SourceRegion(ref.start+off+i1,ref.start+off+i2),init.substring(i1,i2),op))
            off += i2
            init = init.drop(i2)
            ret
        }
        flatten(t.text).foreach(op.addChild)
        op.doEnd
        doIn(parent,input,children.tail)

      case c:Constant =>
        val nc = new AnnotatedConstant(c,ref,text,parent)
        parent.addChild(nc)
        val components = c.getComponents.map(_.value).filter(_.isDefined)
        val sorted = components.sortBy {
          case tc:TermContainer if tc.get.isDefined =>
            val r = SourceRef.get({
              if (tc.parsed.isDefined) tc.parsed.get else tc.get.get
            })
            r.map(_.region.start.offset).getOrElse(input.length)
          case nc:NotationContainer if nc.getAllNotations.nonEmpty =>
            val rs = nc.getAllNotations.map(SourceRef.get(_).map(_.region.start.offset).getOrElse(input.length))
            rs.min
          case _ =>
            ???
        }
        doTerm(nc,input,sorted)
        nc.doEnd
        doIn(parent,input,children.tail)
      case i@Include(_) =>
        val inc = new AnnotatedInclude(i.asInstanceOf[Structure],ref,text,parent)
        parent.addChild(inc)
        doIn(parent,input,children.tail)
      case n: NestedModule =>
        doIn(parent,input,n.module::children.tail)
      case dd: DerivedDeclaration =>
        val nd = new AnnotatedDerived(dd,ref,text,parent)
        doIn(nd,input,dd.getPrimitiveDeclarations)
        nd.doEnd
        parent.addChild(nd)
        doIn(parent,input,children.tail)
      case _ =>
        ???
    }
  }

  private def doTerm(parent : AnnotatedText,input : String,children : List[ComponentContainer])(implicit controller:Controller): Unit = {
    if (children.isEmpty) return
    val head = children.head
    head match {
      case tc:TermContainer if tc.read.isDefined =>
        SourceRef.get({
          if (tc.parsed.isDefined) tc.parsed.get else tc.get.get
        }) match {
          case Some(ref) =>
            parent.addChild(new AnnotatedTerm(tc,ref.region,input.substring(ref.region.start.offset,ref.region.end.offset),parent))
            doTerm(parent,input,children.tail)
          case _ =>
            doTerm(parent,input,children.tail)
        }
      case _:TermContainer =>
        // TODO Inferred, probably
        doTerm(parent,input,children.tail)
      case nc : NotationContainer =>
        nc.getAllNotations.foreach { n =>
          SourceRef.get(n) match {
            case Some(ref) =>
              parent.addChild(new AnnotatedNotation(n,ref.region,input.substring(ref.region.start.offset,ref.region.end.offset),parent))
              doTerm(parent,input,children.tail)
            case _ =>
              ???
          }
        }
      case _ =>
        ???
    }
    /*
    val ref = SourceRef.get(head.value.) match {
      case Some(sr) => sr.region
      case None =>
        ???
    }
    val text = input.substring(ref.start.offset,ref.end.offset)
    head match {}
     */
  }
}

 */