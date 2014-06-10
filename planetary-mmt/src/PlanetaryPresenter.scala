package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import modules._
import symbols._
import documents._
import presentation._
import frontend._
import objects._
import utils._
import flexiformal._
import archives._
import notations._


class NotationPresenter(controller : Controller, var notations : List[(GlobalName,TextNotation)] = Nil) 
  extends presentation.MathMLPresenter {
  report = controller.report
  override def getNotation(path: GlobalName): Option[TextNotation] = {
    notations.find(p => p._1 == path) match {
      case None => super.getNotation(path)
      case Some(p) => Some(p._2)
    }
  }
  //this overrides variable presentation to placeholders for notation rendering (currently changing color to red)
  override def doVariable(n: LocalName)(implicit pc : PresentationContext) {
      val vdAtt = pc.context.find(_.decl.name == n) match {
         case None => Nil
         case Some(vd) => List("jobad:varref" -> vd.declpos.toString)
      }
      val mi = xml.element("mi", ("style" -> "color:red;") :: vdAtt ::: jobadattribs, n.toString)
      pc.out(mi)
   }
}


abstract class PlanetaryAbstractPresenter(name : String) extends Presenter(new presentation.MathMLPresenter) {
  override val outExt = "html"
  val key = name
  def isApplicable(format : String) = format == name
  protected lazy val notPres = new NotationPresenter(controller)
  
  // easy-to-use HTML markup
  protected val htmlRh = utils.HTML(s => rh(s))
   
  import htmlRh._
  def wrapScope(standalone : Boolean, uri : Path)(content : => Unit) {
    if (standalone) {
      rh("<!DOCTYPE html>")
      html{
        head{
          rh(<meta name="mmturi" content={uri.toPath}></meta>)
        }
        body{
          div(attributes=List("xmlns" -> utils.xml.namespace("html"),
                              "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
            content
          }
        }
      }
    } else {
      div(attributes=List("xmlns" -> utils.xml.namespace("html"),
                          "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
        content
      }
    }
  }
}

class PlanetaryHeadersPresenter extends PlanetaryAbstractPresenter("planetary-headers") {
  def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
     this._rh = rh
     s match { 
       case doc : Document => 
         wrapScope(standalone, doc.path)(doDocument(doc))
       case thy : DeclaredTheory => 
         wrapScope(standalone, thy.path)(doTheory(thy))
       case view : DeclaredView =>
         wrapScope(standalone, view.path)(doView(view))
       case d : Declaration => rh("Headers presenter not applicable to declarations " + s.getClass().toString())
       case _ => rh("TODO: Not implemented yet, presentation function for " + s.getClass().toString())
     }
     //TODO? reset this._rh 
   }
  
  import htmlRh._
  def doDocument(doc : Document) = {
    div("document") {
      ul("doc-body") {
        doc.getItems foreach {
          case d: DRef => li("dref") {
            rh(<span jobad:href={d.target.toPath} data-relative="true"> {d.target.last} </span>)
          }
          case m: MRef => li("mref") {a(m.target.last) {text{m.target.last}}}
          case s: SRef => li("sref") {a(s.target.last) {text{s.target.last}}}
        }
      }
    }
  }
  
  def doTheory(t : DeclaredTheory) = {
    //TODO
  } 
  
  def doView(t : DeclaredView) = { 
    //TODO
  }
  
}


class PlanetaryPresenter extends PlanetaryAbstractPresenter("planetary") {
   
   def setRh(rh : RenderingHandler) = this._rh = rh 
  
   def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
     this._rh = rh
     s match { 
       case doc : Document => 
         wrapScope(standalone, doc.path)(doDocument(doc))
       case thy : DeclaredTheory => 
         wrapScope(standalone, thy.path)(doTheory(thy))
       case view : DeclaredView =>
         wrapScope(standalone, view.path)(doView(view))
       case fd : FlexiformalDeclaration => 
         wrapScope(standalone, fd.path)(doFlexiformalDeclaration(fd))
       case _ => rh("TODO: Not implemented yet, presentation function for " + s.getClass().toString())
     }
     //TODO? reset this._rh 
   }   
   
   import htmlRh._
   
   private def doName(p: Path) {
      val s = p.last
      var attrs = p match {
        case g : GlobalName => List("id" -> (g.module.toMPath.name.toPath + "?" + g.name.toPath))
        case m : MPath => List("id" -> m.name.toPath)
        case _ => Nil
      }
      attrs ::= ("jobad:href" -> p.toPath)
      span(cls="name", attributes=attrs) {text(s)}
   }
   
    def doName(path : Path, loadable : Boolean) : Unit = loadable match {
     case true =>
       span(cls = "name loadable", attributes = List("jobad:load" -> path.toPath)) {
         text(path.last)
       }
     case false => doName(path)
   }
   
   private def doMath(t: Obj) {
        apply(t, None)(rh)
   }
   private def doComponent(comp: DeclarationComponent, t: Obj) {
      td {span {text(comp.toString)}}
      td {doMath(t)}
   }
   private def doNotComponent(comp: NotationComponent, tn: TextNotation) {
      td {span {text(comp.toString)}}
      td {span {text(tn.toText)}}
   }
   private val scriptbase = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/script/"
   private val cssbase    = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/css/"
   
   def doTheory(t: DeclaredTheory) {
     div("theory") {
       div("theory-header") {doName(t.path)}
       t.getPrimitiveDeclarations.foreach {
         case PlainInclude(from,to) => 
           span("include") {
             table {
               tr {
                 td { span("keyword") {text("includes")} }
                 td { rh(<span jobad:href={from.toPath}> {from.name.toPath} </span>) }
               }
             }
           } 
         case c : Constant =>
           doConstant(c)
         case f : FlexiformalDeclaration =>  
           doFlexiformalDeclaration(f)
       }
     }
   }
   
   def doConstant(c : Constant) {
     div(cls = "constant", attributes = List("jobad:presents" -> c.path.toPath)) {
       span("keyword"){text("constant")}
       doName(c.path)
       doNotations(c.notC.getAllNotations.map(c.path -> _), c.path)
     }
   }
   
   def encPath(path : GlobalName) : String = {
     path.module.toMPath.name.toString + "_" + path.name.toString
   }
   
   def doNotations(notations : List[(GlobalName, TextNotation)], path : GlobalName) {
     if (!notations.isEmpty) {
       val onclick = "onclick=\"if ($(this).html() == \'Show Notations\') {$(this).html(\'Hide Notations\')} else {$(this).html(\'Show Notations\')};" +
                     "$(document.getElementById(\'not_" + encPath(path) + "\')).toggle( \'fold\' );\""
       rh(" <small><a style=\"cursor:pointer;\" " + onclick + ">" + "Show Notations" + "</a></small>")
       table(cls = "table table-striped table-condensed", attributes = ("id" -> ("not_" + encPath(path))) :: ("style" -> "display:none;") :: Nil) {
         thead {
           tr {
             th { text{"Languages"} } 
             th { text{"Arguments"} }
             th { text{"Rendering"} }
           }
         }
         tbody {
           notations.foreach(p => doNotation(p._1, p._2))
         }
       }
     }
   }
   
   def doNotation(spath : GlobalName, not : TextNotation)  = {
     def getVarName(i : Int) : String = {
       val name = ((i % 26) + 'a'.toInt).toChar
       val indices =  (i / 26) match {
         case 0 => ""
         case n => n.toString
       }
       name.toString + indices
     }

     val arity = not.arity.length
     val tm = OMA(OMID(spath), (0 until arity).map(n => OMV(getVarName(n))).toList)
     tr {
       td { text{not.scope.languages.mkString("/")} }
       td { text{not.arity.length.toString} }
       td {
         notPres.notations = List(spath -> not)
         notPres(tm, None)(rh)
         notPres.notations = Nil
       }
     }
   }
   
   def doFlexiformalDeclaration(fd : FlexiformalDeclaration) : Unit = fd match {
     case n : PlainNarration => 
       div("flexiformal plain") {
         doNarrativeObject(fd.content)
       }
     case d : Definition => 
       div(cls = "flexiformal definition", 
           attributes = List(("jobad:defines" -> d.targets.head.toPath))) {
         //span("keyword"){text("definition" )}
         doNarrativeObject(fd.content)
       }
     case x => 
       throw ImplementationError("Presentation for " + x.getClass() + " not implemented yet")
   }
   
   def doNarrativeObject(no : NarrativeObject) : Unit = no match {
     case n : NarrativeXML => rh(n.node)
     case r : NarrativeRef => r.self match {
      case false => 
        rh("<span jobad:href=\"" + r.target.toPath + "\">")
        r.objects.foreach(doNarrativeObject)
        rh("</span>")
      case true => 
        rh("<span class=\"definiendum\" jobad:href=\"" + r.target.toPath + "\">")
        r.objects.foreach(doNarrativeObject)
        rh("</span>")
     }
     case tm : NarrativeTerm => apply(tm.term, None)(rh)
     case n : NarrativeNode => 
       rh.writeStartTag(n.node.prefix, n.node.label, n.node.attributes, n.node.scope)
       n.child.map(doNarrativeObject)
       rh.writeEndTag(n.node.prefix, n.node.label)
   }
   
   def doView(v: DeclaredView) {}
   override def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
     div("namespace") {
        namespaces.foreach {bt =>
            div("subnamespace") {
               val name = bt.dirName + "/" + bt.outFile.segments.last
               a(name) {
                  text(bt.contentDPath.toPath)
               }
            }
         }
         modules.foreach {bt =>
            div("submodule") {
               a(bt.outFile.segments.last) {
                  text(bt.contentMPath.toPath)
               }
            }
         }
      }
   }
   
   def doDocument(doc: Document) {
     div("document") {
       ul("doc-body") { doc.getItems foreach {
         case d: DRef => 
           //val doc = controller.getDocument(d.path)
           li("dref") {
             doName(d.target, true) 
           }
         case m : MRef =>
           controller.get(m.target) match {
             case t : DeclaredTheory => 
               if (t.name != OMV.anonymous) {
                 li("mref") {
                   doTheory(t)
                 }
               }
             case v : DeclaredView => 
               li("mref") {
                 doView(v)
               }
             case s => throw ImplementationError("Presenting for " + s.getClass() + " not implemented yet ")
           }
         case s => throw ImplementationError("Presenting for " + s.getClass() + " not implemented yet ")
       }}
     }
   }
}

