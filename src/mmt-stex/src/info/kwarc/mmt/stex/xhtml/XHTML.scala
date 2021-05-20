package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.{File, MMTSystem, URI, Unparsed, XMLEscaping}
import info.kwarc.mmt.api.{DPath, Error, ErrorHandler, ErrorThrower, LocalName, NamespaceMap, OpenCloseHandler, ParseError, Path}
import info.kwarc.mmt.stex.STeXError
import info.kwarc.mmt.stex.xhtml.HTMLParser.HTMLNode
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource

import java.io.StringReader
import scala.annotation.tailrec
import scala.collection.mutable
import scala.xml._
import scala.xml.parsing.NoBindingFactoryAdapter

abstract class HTMLRule {
  protected def resource(n : HTMLNode) = n.attributes.get((n.namespace,"resource"))
  protected def property(n : HTMLNode) = n.attributes.get((n.namespace,"property"))
  val priority : Int = 0
  def rule(s : HTMLParser.ParsingState) : PartialFunction[HTMLParser.HTMLNode,HTMLParser.HTMLNode]
}

class CustomHTMLNode(orig : HTMLParser.HTMLNode) extends HTMLParser.HTMLNode(orig.state,orig.namespace,orig.label) {
  replace(orig)
}

object HTMLParser {

  val ns_html = "http://www.w3.org/1999/xhtml"
  val ns_mml = "http://www.w3.org/1998/Math/MathML"
  val ns_stex = "http://kwarc.info/ns/sTeX"
  val empty = '\u200E'

  class ParsingState(val controller : Controller, rules : List[HTMLRule]) {
    val namespaces = mutable.Map.empty[String,String]

    private[HTMLParser] var _top : Option[HTMLNode] = None
    protected var _parent : Option[HTMLNode] = None
    private var _namespace : String = ""
    private val _rules : List[PartialFunction[HTMLNode,HTMLNode]] = rules.sortBy(-_.priority).map(_.rule(this))
    private var _id = 0
    def generateId = {
      _id += 1
      "stexelem" + (_id-1)
    }

    protected def onTop = {}

    def namespace = _namespace
    def top = _top match {
      case Some(e) => e
      case _ =>
        throwError("???")
    }

    private[HTMLParser] var header : String = ""
    def throwError(s : String) = throw new STeXError(s,None,None)
    def error(s : String) : Unit = {
      println(s)
      ???
    }

    object SourceReferences {
      private val files = mutable.Map.empty[File, String]

      private def toOffset(f: File, line: Int, col: Int): SourcePosition = {
        var (o, l, c) = (0, 1, 0)
        files.getOrElseUpdate(f, File.read(f)).foreach {
          case _ if l > line =>
            return SourcePosition(o, line, col)
          case _ if l == line && col == c =>
            return SourcePosition(o, line, col)
          case '\n' =>
            c = 0
            l += 1
            o += 1
          case _ =>
            o += 1
            c += 1
        }
        SourcePosition(-1, line, col)
      }

      private[ParsingState] def doSourceRef(str: String): SourceRef = {
        var file: String = ""
        var from: (Int, Int) = (0, 0)
        var to: (Int, Int) = (0, 0)
        str.split('#') match {
          case Array(f, r) =>
            file = f
            r.drop(1).dropRight(1).split(')') match {
              case Array(b, e) =>
                b.split(';') match {
                  case Array(l, c) =>
                    from = (l.toInt, c.toInt)
                  case _ =>
                    print("")
                    ???
                }
                e.drop(1).split(';') match {
                  case Array(l, c) =>
                    to = (l.toInt, c.toInt)
                  case _ =>
                    print("")
                    ???
                }
              case _ =>
                print("")
                ???
            }
          case _ =>
            print("")
            ???
        }
        val fileuri = controller.backend.resolvePhysical(File(file)).map { case (archive, path) =>
          path.foldLeft(archive.narrationBase)((u, s) => u / s)
        }.getOrElse {
          URI(File(file).toURI)
        }
        SourceRef(fileuri, SourceRegion(toOffset(File(file), from._1, from._2), toOffset(File(file), to._1, to._2)))
      }
    }

    private[HTMLParser] def withParent[A](n : HTMLNode)(f : => A) = {
      val oldparent = _parent
      val oldnamespace = _namespace
      try { f } finally {_parent = oldparent; _namespace = oldnamespace}
    }

    private def bookkeep(n : HTMLNode) = {
      if (_parent.isEmpty && _top.isEmpty) {
        _top = Some(n)
        onTop
        n.attributes.toList.filter(_._1._1 == "xmlns").foreach { t =>
          n.attributes.remove(t._1)
          namespaces(t._1._2) = t._2
        }
      } else{
        val p = _parent.getOrElse(_top.get)
        p._children ::= n
        n._parent = Some(p)
      }
      n.attributes.get((ns_stex,"sourceref")) match {
        case Some(s) if s.contains("#(") =>
          n.attributes.remove((ns_stex,"sourceref"))
          n._sourceref = Some(SourceReferences.doSourceRef(s))
        case None =>
        case Some(s) =>
          n._sourceref = Some(SourceRef.fromURI(URI(s)))
      }
      if (n._sourceref.isEmpty && _parent.exists(_._sourceref.isDefined)) n._sourceref = _parent.get._sourceref
      val newn = _rules.collectFirst{ case r if r.isDefinedAt(n) => r(n)}.getOrElse(n)
      newn
    }
    private[HTMLParser] def openclose(n : HTMLNode) = {
      val newn = bookkeep(n)
      newn.onAddI
    }
    private[HTMLParser] def open(n : HTMLNode) = {
      val newn = bookkeep(n)
      _parent = Some(newn)
      _namespace = newn.namespace
    }
    private[HTMLParser] def close(label : String) = {
      if (!_parent.exists(_.label == label)) {
        error("???")
      }
      val elem = _parent.get
      _parent = elem._parent
      _namespace = _parent.map(_.namespace).getOrElse("")
      elem.onAddI
      elem
    }

    private[HTMLParser] def present(n : HTMLNode,indent : Int = 0) : String = {
      {if (_top.contains(n)) header + "\n" else ""} +
      {
        if (n.startswithWS) "\n" + {if (indent>0) (0 until indent).map(_ => "  ").mkString else ""} else ""
      } + {n match {
        case t : HTMLText =>
          t.text + {if(t.endswithWS) "\n" else ""}
        case _ =>
          "<" + n.label + {
            if (!n._parent.exists(_.namespace == n.namespace)) " xmlns=\"" + n.namespace + "\"" else ""
          } + {
            if (_top.contains(n)) namespaces.toList.map {
              case (p,v) => " xmlns:" + p + "=\"" + v + "\""
            }.mkString else ""
          } + n.attributes.toList.reverse.map{
            case ((ns,key),value) =>
              {if (ns == n.namespace) " "
              else if (namespaces.values.toList.contains(ns))
                " " + namespaces.toList.collectFirst{case p if p._2 == ns => p._1}.get + ":"
              else " " + ns + ":"
              } + key + "=\"" + XMLEscaping(value) + "\""
          }.mkString + {
            if (n._sourceref.isDefined && !n._parent.exists(_._sourceref == n._sourceref)) " " + "stex:sourceref=\"" + n._sourceref.get.toString + "\"" else ""
          } + {
            if (n.classes.nonEmpty) " class=\"" + n.classes.mkString(" ") + "\"" else ""
          } + {
            if(n._children.isEmpty) "/>" else {
              ">" + n._children.reverse.map(present(_,indent+1)).mkString + {
                {if (n.endswithWS) "\n" + {if (indent>0) (0 until indent).map(_ => "  ").mkString else ""} else ""} +
                  "</" + n.label + ">"
              }
            }
          }
      }}
    }
  }

  class HTMLNode(var state : ParsingState, val namespace : String, val label : String) {
    override def toString: String = state.present(this)

    val attributes = mutable.Map.empty[(String, String), String]
    var classes : List[String] = Nil
    private[HTMLParser] var _parent: Option[HTMLNode] = None

    def parent = _parent

    private[HTMLParser] var _children: List[HTMLNode] = Nil

    def children = _children.reverse

    def isEmpty : Boolean = _children.forall(_.isEmpty)

    private[HTMLParser] def onAddI = onAdd

    private[HTMLParser] var startswithWS = false
    private[HTMLParser] var endswithWS = false
    private[HTMLParser] var _sourceref: Option[SourceRef] = None
    def sourceref = _sourceref

    def addAttribute(key: String, value: String) = key.split(':') match {
      case Array(a, b) =>
        val ns = state.namespaces.getOrElse(a, a)
        attributes((ns, b)) = value
      case Array(a) =>
        attributes((namespace, a)) = value
      case _ =>
        state.error("???")
    }

    protected def onAdd = {}

    protected def replace(n: HTMLNode) = {
      _parent = n._parent
      startswithWS = n.startswithWS
      _sourceref = n._sourceref
      classes = n.classes
      n.attributes.foreach{case ((a,b),c) => attributes((a,b)) = c}
      state = n.state
      if (state._top contains n) state._top = Some(this)
      _parent.foreach(_._children.splitAt(_parent.get._children.indexOf(n)) match {
        case (before, _ :: after) =>
          _parent.get._children = before ::: this :: after
        case _ =>
          state.error("???")
      })
    }

    def ancestors: List[HTMLNode] = _parent match {
      case Some(p) => p :: p.ancestors
      case _ => Nil
    }

    def collectAncestor[A](f: PartialFunction[HTMLNode, A]): Option[A] = _parent match {
      case Some(f(a)) => Some(a)
      case Some(e) => e.collectAncestor(f)
      case _ => None
    }

    @tailrec
    final def iterate(f: HTMLNode => Unit): Unit = {
      f(this)
      children match {
        case a :: _ => a.iterate(f)
        case _ => successor match {
          case Some(s) => s.iterate(f)
          case _ =>
        }
      }
    }

    @tailrec
    final protected def successor: Option[HTMLNode] = parent match {
      case Some(p) =>
        val ch = p.children
        ch.drop(ch.indexOf(this)).tail.headOption match {
          case Some(h) => Some(h)
          case None =>
            p.successor
        }
      case _ => None
    }

    def get(_label : String = "")(_attributes : (String,String,String)*)(cls : String = "") : List[HTMLNode] = {
      val matches = _label match {
        case "" =>
          _attributes match {
            case Nil =>
              cls match {
                case "" => (n : HTMLNode) => true
                case c => (n : HTMLNode) => n.classes.contains(c)
              }
            case _ =>
              (n : HTMLNode) => _attributes.exists(t => n.attributes.get(t._1,t._2).contains(t._3)) && (cls match {
                case "" => true
                case c => n.classes.contains(c)
              })
          }
        case _ =>
          _attributes match {
            case Nil =>
              cls match {
                case "" => (n : HTMLNode) => n.label == _label
                case c => (n : HTMLNode) => n.label == _label && n.classes.contains(c)
              }
            case _ =>
              (n : HTMLNode) => n.label == _label && _attributes.exists(t => n.attributes.get(t._1,t._2).contains(t._3)) && (cls match {
                case "" => true
                case c => n.classes.contains(c)
              })
          }
      }
      get(matches)
    }
    protected def get(matches : HTMLNode => Boolean) : List[HTMLNode] = _children.filter(matches) ::: _children.flatMap(_.get(matches))

    def delete = _parent.foreach{p =>
      p._children = p._children.filterNot(_ == this)
    }
    def add(n : Node): HTMLNode = add(n.toString())
    def add(s : String): HTMLNode = state.withParent(this){
      apply(s)(state)
      _children.head
    }
    def add(n : HTMLNode): Unit = {
      n._parent.foreach(p => p._children = p._children.filterNot(_ == n))
      n._parent = Some(this)
      n.state = this.state
      _children ::= n
    }

    def node = XML.loadString(this.toString)

  }

  class HTMLText(state : ParsingState, val text : String) extends HTMLNode(state,"","") {
    override def toString() = XMLEscaping(text)
    override def isEmpty = toString() == "" || toString() == "&200e"
  }

  object HTMLNode {
    def apply(state : ParsingState, label : String, xmlns : String = "") = label.split(':') match {
      case Array(l) if xmlns.nonEmpty =>
        new HTMLNode(state,xmlns,l)
      case Array(nsa,l) if state.namespaces.contains(nsa) =>
        new HTMLNode(state,state.namespaces(nsa),l)
      case Array(l) =>
        new HTMLNode(state,state.namespace,l)
      case _ =>
        ???
    }
  }

  def apply(file : File)(implicit state : ParsingState) = {
    implicit val in = new Unparsed(File.read(file),s => throw new STeXError(s,None,None))
    in.trim
    doHeader
    doNext
    state.top
  }

  def apply(s : String)(implicit state : ParsingState) = {
    implicit val in = new Unparsed(s,s => throw new STeXError(s,None,None))
    doNext
    state.top
  }

  private def doHeader(implicit state : ParsingState,in : Unparsed): Unit = {
    in.takeUntilString("<!DOCTYPE",Nil)
    state.header = "<!DOCTYPE" + in.takeUntilChar('>','\\')._1 + ">"
    in.trim
  }

  @tailrec
  private def doNext(implicit state : ParsingState,in : Unparsed): Unit = if (!in.empty) {
    val startWS = in.head.isWhitespace
    in.trim
    if (!in.empty) {
      in.next() match {
        case '<' if in.getnext(3).toString == "!--" =>
          in.drop("!--")
          in.takeUntilString("-->",Nil)
        case '<' if in.head == '/' =>
          in.next()
          val label = in.takeWhile(_ != '>').trim
          in.next()
          val n = state.close(label)
          n.endswithWS = startWS
        case '<' =>
          var label = in.takeWhile(c => !c.isWhitespace && c != '>')
          if (in.head == '>') {
            in.next()
            if (label.endsWith("/")) {
              label = label.init
              val n = HTMLNode(state,label)
              n.startswithWS = startWS
              state.openclose(n)
            } else {
              val n = HTMLNode(state,label)
              n.startswithWS = startWS
              state.open(n)
            }
          } else {
            var xmlns = ""
            var classes : List[String] = Nil
            var attributes : List[(String,String)] = Nil
            var close = false
            var done = false
            while (!done) in.head match {
              case '/' if in.getnext(2).toString == "/>" =>
                in.drop("/>")
                close = true
                done = true
              case '>' =>
                done = true
                in.next()
              case c if c.isWhitespace =>
                in.trim
              case _ =>
                val attr = in.takeWhile(_ != '=').trim
                in.next()
                if(in.head != '\"') {
                  state.error("???")
                }
                in.next()
                val value = in.takeWhile(_ != '\"').trim
                in.next()
                if (attr == "xmlns") xmlns = value
                else if (attr == "class") classes = value.split(' ').map(_.trim).toList
                else attributes ::= (attr,value)
            }
            val n = HTMLNode(state,label,xmlns)
            n.startswithWS = startWS
            attributes.foreach(p => n.addAttribute(p._1,p._2))
            n.classes = classes
            if (close) state.openclose(n) else state.open(n)
          }
        case c =>
          var txt = c + in.takeWhileSafe(_ != '<')
          val endWS = txt.lastOption.exists(_.isWhitespace)
          txt = XMLEscaping.unapply(txt.trim)
          if (txt.nonEmpty) {
            val n = new HTMLText(state, txt)
            n.startswithWS = startWS
            n.endswithWS = endWS
            state.openclose(n)
          }
        case _ =>
          state.error("???")
      }
      doNext
    }
  }
}


// ---------------------------------------------------------------------------------------------------------------------

/*

class XHTMLAnnotation(val node : XHTMLNode) {
  val priority = 0
  def init = node.addAnnotation(this)
  def afterPopulating(s : XHTMLParsingState) : Unit = {}
  val isEmpty = false
  init
}

class XHTMLParsingState(rules : List[PartialFunction[(XHTMLNode,XHTMLParsingState), Unit]] = Nil, error : ErrorHandler = ErrorThrower) {
  def withErrors[A](f : => A) = try {Some(f)} catch {
    case e : Error =>
      error(e)
      None
  }
  def open(node : XHTMLNode) = {
    rules.foreach(r => withErrors{r.apply(node,this)})
    node match {
      case _ if List("math","mrow","mi","mo","msub","msup","mtext","mfrac","mspace","munderover","merror","mtext","omnum","munder","mstyle").contains(node.label) =>
          new MathMLAnnotation(node)
      case _ if node.label == "mn" =>
        MathMLLiteral(node)
      case _ =>
    }
  }
  def close(node : XHTMLNode) = {
    node.getAnnotations.foreach(r => withErrors(r.afterPopulating(this)))
  }
}

object XHTML {
  def text(s : String) = new XHTMLText(s)

  def unescape(s : String) : String = {
    val s1 = XMLEscaping.unapply(s)
    if (s1 == s) s else unescape(s1)
  }

  def makeAttributes(ls : ((String,String),String)*) = ls.foldLeft(scala.xml.Null : MetaData){
    case (e,(("",k),v)) =>
      new UnprefixedAttribute(k,v,e)
    case (e,((p,k),v)) =>
      new PrefixedAttribute(p,k,v,e)
  }
/*
  object Rules {
    implicit val defaultrules : List[PartialFunction[(XHTMLNode,Unit]] = List(
      {case e : Elem if e.label == "html" => new XHTMLDocument},
      {case e : Elem if e.label == "math" => new XHTMLMath(Some(e))},
      {case e : Elem if e.label == "svg" =>
        val ne = new XHTMLNode(Some(e)) {}
        ne.attributes(("","xmlns")) = "http://www.w3.org/2000/svg"
        ne
      },
      {case e : Elem if List("mrow","mi","mo","msub","msup","mtext","mfrac","mspace","munderover","merror").contains(e.label) => new XMathML(Some(e))}
    )
  }
 */
  private lazy val parserFactory = {
    val ret = new SAXFactoryImpl
    ret.setNamespaceAware(true)
    ret
  }
  private lazy val adapter = new NoBindingFactoryAdapter
  private lazy val parser = parserFactory.newSAXParser()

  def parse(f : File, state : Option[XHTMLParsingState] = None) = {
    val ret = adapter.loadXML(new InputSource(File.Reader(f)),parser)
    apply(ret,state)
  }

  def applyString(s : String, state : Option[XHTMLParsingState] = None) = {
    val ret = adapter.loadXML(new InputSource(new StringReader(s)),parser)
    apply(ret,state)
  }

  def apply(node : Node, state : Option[XHTMLParsingState] = None) : XHTMLNode = {
    val nstate = state.getOrElse(new XHTMLParsingState(Nil))
    applyI(node)(nstate).head
  }

  private def applyI(node : Node)(implicit state : XHTMLParsingState) : List[XHTMLNode] = state.withErrors{
    val top = node match {
      case e : Elem if e.label == "script" && e.attribute("type").map(_.toString()).contains("application/xml")
        && e.child.length == 1 && e.child.head.isInstanceOf[scala.xml.Text] && e.toString().contains("&lt;") => // Parser doesn't handle scripts; considers them opaque strings
        val start = node.toString().takeWhile(_ != '>') + ">"
        val ret = scala.xml.XML.loadString(start + node.child.map(c => unescape(c.toString())).mkString("") + "</script>")
        return applyI(ret)
      case e : Elem if e.label == "html" =>
        val ret = new XHTMLDocument
        state.open(ret)
        node.child.foreach{ c =>
          applyI(c).foreach{ ci =>
            ret.add(ci)
            state.close(ci)
          }
        }
        ret
      case t : scala.xml.Text =>
        var init = t.toString().takeWhile(_.isWhitespace)
        var end = t.toString().reverse.takeWhile(_.isWhitespace).reverse
        val content = t.toString().drop(init.length).dropRight(end.length)
        if (init.contains('\n')) init = "\n"
        if (end.contains('\n')) end = "\n"
        //if (content.isEmpty) return Nil
        val ret = List(init,content,end).map(s => new XHTMLText(s))
        ret.foreach{ c =>
          state.open(c)
        }
        return ret
      case a : Atom[String] =>
        val ret = new XHTMLText(a.data)
        state.open(ret)
        ret
      case e : Elem =>
        val ret = new XHTMLNode(Some(node))
        state.open(ret)
        node.child.foreach{ c =>
          applyI(c).foreach{ ci =>
            ret.add(ci)
            state.close(ci)
          }
        }
        ret
      case null =>
        return Nil
    }
    List(top)
  }.getOrElse(Nil)
  val empty = scala.xml.Text("\u200E")
}

class XHTMLNode(initial_node : Option[Node] = None) {
  var prefix = initial_node.map(_.prefix).getOrElse("")
  var label = initial_node.map(_.label).getOrElse("")
  val attributes = mutable.Map.empty[(String,String),String]
  var scope = initial_node.map(_.scope).orNull
  protected var _children : List[XHTMLNode] = Nil
  var parent : Option[XHTMLNode] = None

  private var annotations : List[XHTMLAnnotation] = Nil
  def addAnnotation(a : XHTMLAnnotation) = annotations ::= a
  def getAnnotations = annotations.sortBy(-_.priority)
  def deleteAnnotation(a : XHTMLAnnotation) = annotations = annotations.filterNot(_ == a)

  private def fill(a : MetaData) : Unit = a match {
    case UnprefixedAttribute(str, value, data) =>
      attributes(("",str)) = value.toString()
      fill(data)
    case PrefixedAttribute(str, str1, value, data) =>
      attributes((str,str1)) = value.toString()
      fill(data)
    case scala.xml.Null =>
    case _ =>
      ???
  }

  initial_node.foreach(n => fill(n.attributes))
  private var _classes = attributes.get(("","class")).map(_.split(' ').toList).getOrElse(Nil).filterNot(_ == "")
  attributes.remove(("","class"))
  def addClass(cls : String) = if (!_classes.contains(cls)) _classes ::= cls
  def removeClass(cls : String) = _classes = _classes.filterNot(_ == cls)
  def classes = _classes

  def strip : Unit = {
    attributes.remove(("","property"))
    attributes.remove(("","resource"))
    attributes.remove(("stex","arg"))
    _classes = _classes.filterNot(_.startsWith("stex:"))
    children.foreach(_.strip)
  }

  def ancestors : List[XHTMLNode] = if (parent.isDefined) parent.get :: parent.get.ancestors else Nil
  def collectFirstAncestor[A](f : PartialFunction[XHTMLNode,A]) : Option[A] = this match {
    case f(a) => Some(a)
    case _ => parent.flatMap(_.collectFirstAncestor(f))
  }

  def isMathML = collectFirstAncestor {
      case a if a.label == "math" => true
      case a if a.label == "mtext" && a != this => false
    }.getOrElse(false)

  def top : XHTMLNode = parent match {
    case None => this
    case Some(p) => p.top
  }

  def getHead = top.get("head")()().head

  def children = _children


  def node : Node = Elem(prefix,label,XHTML.makeAttributes({
    if (_classes.isEmpty)
      attributes.toSeq.reverse
    else ((("","class"),_classes.mkString(" ")) :: attributes.toSeq.reverse.toList)
  }:_*),scope,true,children.map(_.node) :_*)

  def add(s : String) : Unit = add(XHTML.text(s))
  def add(n : Node) : Unit = add(XHTML(n))
  def add(e : XHTMLNode) : Unit = {
    e.parent = Some(this)
    _children = _children ::: List(e)
  }

  def addAfter(e : XHTMLNode,after : XHTMLNode) : Unit = _children.indexOf(after) match {
    case -1 => _children = _children ::: List(e)
      e.parent = Some(this)
    case i => _children = _children.take(i+1) ::: e :: _children.drop(i+1)
      e.parent = Some(this)
  }

  protected def delete(e : XHTMLNode) : Unit = _children = _children.filterNot(_ == e)
  def delete : Unit = parent.foreach(_.delete(this))

  override def toString: String = node.toString()

  @tailrec
  final def iterate(f : XHTMLNode => Unit) : Unit = {
    f(this)
    children match {
      case a :: _ => a.iterate(f)
      case _ => successor match {
        case Some(s) => s.iterate(f)
        case _ =>
      }
    }
  }

  @tailrec
  final def successor : Option[XHTMLNode] = parent match {
      case Some(p) =>
        val ch = p.children
        ch.drop(ch.indexOf(this)).tail.headOption match {
          case Some(h) => Some(h)
          case None =>
            p.successor
        }
      case _ => None
  }

  def get(_label : String = "")(_attributes : (String,String,String)*)(cls : String = "") : List[XHTMLNode] = {
    val matches = _label match {
      case "" =>
        _attributes match {
          case Nil =>
            cls match {
              case "" => (n : XHTMLNode) => true
              case c => (n : XHTMLNode) => n._classes.contains(c)
            }
          case _ =>
            (n : XHTMLNode) => _attributes.exists(t => n.attributes.get(t._1,t._2).contains(t._3)) && (cls match {
              case "" => true
              case c => n._classes.contains(c)
            })
        }
      case _ =>
        _attributes match {
          case Nil =>
            cls match {
              case "" => (n : XHTMLNode) => n.label == _label
              case c => (n : XHTMLNode) => n.label == _label && n._classes.contains(c)
            }
          case _ =>
            (n : XHTMLNode) => n.label == _label && _attributes.exists(t => n.attributes.get(t._1,t._2).contains(t._3)) && (cls match {
              case "" => true
              case c => n._classes.contains(c)
            })
        }
    }
    get(matches)
  }
  protected def get(matches : XHTMLNode => Boolean) : List[XHTMLNode] = _children.filter(matches) ::: _children.flatMap(_.get(matches))

  private var _id = 0
  def generateId = {
    _id += 1
    "stexelem" + (_id-1)
  }

  def isEmpty : Boolean = annotations.forall(_.isEmpty) && {_children.isEmpty || _children.forall(_.isEmpty)}

  def getTopMath : XHTMLNode = if (!parent.get.isMathML) this else parent.get.getTopMath
}

class XHTMLText(init : String) extends XHTMLNode() {
  private var _text = init
  def text = _text

  override def node: scala.xml.Text = scala.xml.Text(text)
  override def strip = {}

  override def isEmpty: Boolean = {
    val trimmed = text.replace('\u2061',' ').trim
    trimmed.isEmpty
  }
}

class XHTMLDocument extends XHTMLNode(None) {
  private val doc_prefix = "<!DOCTYPE html>"

  override def node: Node =
    <html xmlns="http://www.w3.org/1999/xhtml"
          xmlns:om="http://www.openmath.org/OpenMath"
          xmlns:stex="http://kwarc.info/ns/sTeX"
          xmlns:ml="http://www.w3.org/1998/Math/MathML"
    >{children.map(_.node)}</html>

  override def toString: String = doc_prefix + super.toString
}

 */