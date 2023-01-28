package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.{File, URI, Unparsed, XMLEscaping}
import info.kwarc.mmt.stex.Extensions.LateBinding
import info.kwarc.mmt.stex.STeXError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try
import scala.xml._

abstract class HTMLRule {
  val priority : Int = 0
  def apply(s : HTMLParser.ParsingState,n:HTMLNode) : Option[HTMLNodeWrapper]
}

object HTMLParser {

  val ns_html = "http://www.w3.org/1999/xhtml"
  val ns_mml = "http://www.w3.org/1998/Math/MathML"
  val ns_stex = "http://kwarc.info/ns/sTeX"
  val ns_shtml = "http://kwarc.info/ns/SHTML"
  val ns_mmt = "http://uniformal.github.io/MMT"
  val ns_rustex = "http://kwarc.info/ns/RusTeX"
  val ns_svg = "http://www.w3.org/2000/svg"
  val empty = '\u200E'

  class ParsingState(val controller : Controller, rules : List[HTMLRule]) {
    val namespaces = mutable.Map.empty[String,String]
    namespaces("xhtml") = ns_html
    namespaces("mml") = ns_mml
    namespaces("stex") = ns_stex
    namespaces("shtml") = ns_shtml
    namespaces("mmt") = ns_mmt
    namespaces("rustex") = ns_rustex
    namespaces("svg") = ns_svg

    var _top : Option[HTMLNode] = None
    protected var _parent : Option[HTMLNode] = None
    private var _namespace : String = ""
    private val _rules : List[HTMLRule] = rules.sortBy(-_.priority)
    private def applyRules(nn : HTMLNode) : HTMLNode = {
      var node = nn
      _rules.foreach{r =>
        r(this,node) match {
          case Some(n) =>
            n.replace(node)
            node = n
            node.onOpen
          case _ =>
        }
      }
      node
    }

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
        if (File(file).exists()) {
          SourceRef(fileuri, SourceRegion(toOffset(File(file), from._1, from._2), toOffset(File(file), to._1, to._2)))
        } else SourceRef.anonymous(file)
      }
    }

    private[xhtml] def withParent[A](n : HTMLNode)(f : => A) = {
      val oldparent = _parent
      val oldnamespace = _namespace
      _parent = Some(n)
      _namespace = n.namespace
      try { f } finally {_parent = oldparent; _namespace = oldnamespace}
    }

    protected def onTop(n : HTMLNode): Option[HTMLNode] = None

    private def bookkeep(n : HTMLPlainNode) = {
      val nn = if (_parent.isEmpty && _top.isEmpty) {
        val in = onTop(n).getOrElse(n)
        _top = Some(in)
        in.plain.attributes.toList.filter(_._1._1 == "xmlns").foreach { t =>
          in.plain.attributes.remove(t._1)
          namespaces(t._1._2) = t._2
        }
        in
      } else {
        val p = _parent.getOrElse(_top.get)
        p.plain._children ::= n
        n._parent = Some(p)
        n
      }
      nn.plain.attributes.get((ns_shtml,"sourceref")) match {
        case Some(s) if s.contains("#(") =>
          nn.plain.attributes.remove((ns_shtml,"sourceref"))
          nn.plain._sourceref = Some(SourceReferences.doSourceRef(s))
        case None =>
        case Some(s) =>
          nn.plain.attributes.remove((ns_shtml,"sourceref"))
          nn.plain._sourceref = Some(SourceRef.fromURI(URI(s)))
      }
      nn.plain.attributes.get((ns_rustex,"sourceref")) match {
        case Some(s) if s.contains("#(") =>
          nn.plain.attributes.remove((ns_rustex,"sourceref"))
          nn.plain._sourceref = Some(SourceReferences.doSourceRef(s))
        case None =>
        case Some(s) =>
          nn.plain.attributes.remove((ns_rustex,"sourceref"))
          nn.plain._sourceref = Some(SourceRef.fromURI(URI(s)))
      }
      if (nn.plain._sourceref.isEmpty && _parent.exists(_.sourceref.isDefined)) nn.plain._sourceref = _parent.get.plain._sourceref
      val newn = applyRules(nn)
      newn
    }
    private[xhtml] def openclose(n : HTMLPlainNode) = {
      val newn = bookkeep(n)
      newn.onAdd
    }
    private[xhtml] def open(n : HTMLPlainNode) = {
      val newn = bookkeep(n)
      _parent = Some(newn)
      _namespace = newn.namespace
    }
    private[HTMLParser] def close(label : String) = {
      if (!_parent.exists(_.label == label)) {
        error("Malformed HTML: </" + label + "> does not close any currently open node")
      }
      val elem = _parent.get
      elem.onAdd
      _parent = elem.plain._parent
      _namespace = _parent.map(_.namespace).getOrElse("")
      elem
    }

    private val void_elements = List(
      "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr"
    )

    private[xhtml] def present(n : HTMLPlainNode,indent : Int = 0,forcenamespace : Boolean = false,dotop:Boolean=true) : String = {
      {if (_top.contains(n)) header else ""} +
      {
        if (n.startswithWS) "\n" + {if (indent>0) (0 until indent).map(_ => "  ").mkString else ""} else ""
      } + {n match {
        case t : HTMLText =>
          t.toString() + {if(t.endswithWS) "\n" else ""}
        case _ =>
          "<" + n.label + {
            if (!n._parent.exists(_.namespace == n.namespace) || forcenamespace) " xmlns=\"" + n.namespace + "\"" else ""
          } + {
            if (_top.contains(n) && dotop) namespaces.toList.map {
              case (p,v) => " xmlns:" + p + "=\"" + v + "\""
            }.mkString else ""
          } + n.attributes.toList.reverse.map{
            case ((ns,key),value) =>
              {if (ns == n.namespace) " "
              else if (namespaces.values.toList.contains(ns))
                " " + namespaces.toList.collectFirst{case p if p._2 == ns => p._1}.get + ":"
              else " " + ns + ":"
              } + key + "=\"" + value.replace("\"","\'").replace("&","%26") + "\""
          }.mkString + {
            if (n._sourceref.isDefined && !n._parent.exists(_.sourceref == n._sourceref)) " " + "shtml:sourceref=\"" + n._sourceref.get.toString + "\"" else ""
          } + {
            if (n.classes.nonEmpty) " class=\"" + n.classes.distinct.mkString(" ") + "\"" else ""
          } + {
            if(n._children.isEmpty && n.namespace == ns_html && void_elements.contains(n.label)) "/>" else {
              ">" + n._children.reverse.map(i => present(i.plain,indent+1)).mkString + {
                {if (n.endswithWS) "\n" + {if (indent>0) (0 until indent).map(_ => "  ").mkString else ""} else ""} +
                  "</" + n.label + ">"
              }
            }
          }
      }}
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
    implicit val in = new Unparsed(s.replace("&amp;nbsp;","&nbsp;"),s => throw new STeXError(s,None,None))
    in.trim
    doHeader
    doNext
    state.top
  }

  private def doHeader(implicit state : ParsingState,in : Unparsed): Unit = {
    if (in.remainder.toString.length > 2 && (in.getnext(2).startsWith("<!") || in.getnext(2).startsWith("<?"))) {
      val s = in.takeUntilChar('>', '\\')._1 + ">\n"
      if (s.startsWith("<!")) state.header += s
      in.trim
      doHeader
    }
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
          n.plain.endswithWS = startWS
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
                val bgchar = if (in.head == '\"') '\"' else if (in.head == '\'') '\'' else
                  state.error("???")
                in.next()
                val value = in.takeWhile(_ != bgchar).trim
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
          var txt = s"${c}${in.takeWhileSafe(_ != '<')}"
          val endWS = txt.lastOption.exists(_.isWhitespace)
          txt = if (txt.trim == empty.toString) empty.toString else txt.trim/*Try(XMLEscaping.unapply(txt.trim)).toOption.getOrElse({
            print("")
            txt.trim
          })*/
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