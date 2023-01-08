package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.{CPath, ContentPath, DefComponent, GlobalName, MPath, NamespaceMap, Path, StructuralElement, TypeComponent}
import info.kwarc.mmt.api.objects.{OMID, OMS, Obj, Term}
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration}
import info.kwarc.mmt.api.utils.{File, MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.OMDocHTML
import info.kwarc.mmt.stex.vollki.FullsTeXGraph
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLParser}

import scala.xml.Elem
//import info.kwarc.mmt.stex.xhtml.{HTMLConstant, HTMLParser, HTMLRule, HTMLTheory, HasLanguage, OMDocHTML}
//import info.kwarc.mmt.stex.{STeX, translations}

import scala.xml.Node

trait FragmentExtension extends STeXExtension {
  def doConstant(c : Constant, preview : Boolean) : Option[HTMLNode] = None
  def doDerivedDecl(node : HTMLNode, c : DerivedDeclaration, preview : Boolean) : Unit = {}
  def doExpression(node : HTMLNode, tm : Term, component : Option[CPath], preview : Boolean) : Unit = {}
}

object FragmentExtension extends STeXExtension {
/*
  override def serverReturn(request: ServerRequest): Option[ServerResponse] = request.path.lastOption match {
    case Some("fragment") =>
      request.query match {
        case "" =>
          Some(ServerResponse("Empty fragment","txt"))
        case ps =>
          val (comp,lang) = ps.split('&') match {
            case Array(a) => (a,None)
            case Array(a,l) if l.startsWith("language=") => (a,if (l.drop(9).isEmpty) None else Some(l.drop(9)))
            case _ => (ps,None)
          }
          val path = Path.parse(comp)
          doFragment(path,lang)
      }
    case Some("symbol") =>
      request.query match {
        case "" =>
          Some(ServerResponse("Empty Document path", "txt"))
        case s =>
          var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
          html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + server.pathPrefix + "/declaration?" + s)
          html = html.replace("BASE_URL_PLACEHOLDER", "")
          Some(ServerResponse(html, "text/html"))
      }
    case Some("declaration") =>
      request.query match {
        case "" =>
          Some(ServerResponse("Empty fragment","txt"))
        case ps =>
          val (comp,lang) = ps.split('&') match {
            case Array(a) => (a,None)
            case Array(a,l) if l.startsWith("language=") => (a,if (l.drop(9).isEmpty) None else Some(l.drop(9)))
            case _ => (ps,None)
          }
          val path = Path.parse(comp)
          Some(doDeclaration(path,lang))
      }
    case Some("declheader") =>
      request.query match {
        case "" =>
          Some(ServerResponse("Empty fragment","txt"))
        case ps =>
          val path = Path.parse(ps)
          Some(controller.getO(path) match {
            case Some(c: Constant) =>
              val (doc, body) = server.emptydoc
              body.add(doDeclHeader(c))
              ServerResponse("<body>" + body.toString + "</body>", "text/html")
            case Some(d) =>
              ServerResponse("Not yet implemented: " + d.getClass.toString, "txt")
            case _ =>
              ServerResponse("Declaration not found", "txt")
          })
      }
    case _ => None
  }






  def getFragmentDefault(ce : StructuralElement,language:Option[String]) : String = {
    def text(s : String) = scala.xml.Text(s)
    var ret : List[Path] = Nil
    controller.depstore.query(ce.path,-SymdocRelational.documents)(s => ret ::= s)
    ret.flatMap(controller.getO).headOption match {
      case Some(c : Constant) => c.df match {
        case Some(STeX.symboldoc(_,lang,node)) if language.contains(lang) || lang.isEmpty || (language.isEmpty && lang.contains("en")) => // TODO language
          return node.map(_.toString()).mkString//XMLEscaping.unapply(str)
        case _ =>
      }
      case _ =>
    }
    ce match {
      case c : Constant =>
        val res = "Symbol <b>" + c.name + "</b> in module " + (SourceRef.get(c) match {
          case Some(sr) =>
            controller.backend.resolveLogical(sr.container) match {
              case Some((a,f)) =>
                val url = "/:sTeX/browser/fulldocument" +
                  "?archive=" + a.id + "&filepath=" + (f.init ::: f.last.replace(".tex",".xhtml") :: Nil).mkString("/")
                s"<a href='${url}'>${c.parent.name.toString}</a>"
              case _ => c.parent.name.toString
            }
          case _ => c.parent.name.toString
        })
        "<div>"+res+"</div>"
        /*
        val macroname = OMDocHTML.getMacroName(c)
        (<table>
          <tr><td><b>Type</b></td><td>{c.tp.map(server.xhtmlPresenter.asXML(_,Some(c.path $ TypeComponent))).getOrElse(text("None"))}</td></tr>
          {macroname.foreach{name => OMDocHTML.getNotations(c,controller).map{
            case ("",_,_,n,_) =>
              <tr><td>{"\\"+name}</td><td>{n}</td></tr>
            case (f,_,_,n,_) =>
              <tr><td>{"\\"+name+"[" + f + "]"}</td><td>{n}</td></tr>
          }}}
        </table>).toString()

         */
      case _ =>
        ???
    }
  }


  def termLink(o : Obj, comp : Option[CPath]) = DocumentExtension.makePostButton(
    server.xhtmlPresenter.asXML(o, comp),
    "/:" + server.pathPrefix + "/expression",
    ("openmath",o.toNode.toString().replace("\n","").replace("\n","")),
    ("component",comp.map(_.toString).getOrElse("None"))
  )

 */

}