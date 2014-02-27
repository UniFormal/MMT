package info.kwarc.mmt.api.flexiformal

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

class FlexiformalPresenter extends Presenter {
   override val outExt = "html"
   def key = "ihtml"
   val outDim = Dim("export", "ihtml")
   private lazy val mmlPres = new presentation.MathMLPresenter(controller) // must be lazy because controller is provided in init only
     
   def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
     this._rh = rh
     s match { 
       case doc : Document => 
         wrapScope(doDocument(doc))
       case thy : DeclaredTheory => 
         wrapScope(doTheory(thy))
       case view : DeclaredView =>
         wrapScope(doView(view))
       case fd : FlexiformalDeclaration => 
         wrapScope(doFlexiformalDeclaration(fd))
       case _ => rh("TODO: Not implemented yet, presentation function for " + s.getClass().toString())
     }
     //TODO? reset this._rh 
   }
   
   
   
   def apply(o : Obj)(implicit rh : RenderingHandler) = mmlPres(o)(rh)
   
   def isApplicable(format : String) = format == "ihtml"
   
   // easy-to-use HTML markup
   protected val htmlRh = new utils.HTML(s => rh(s))
   import htmlRh._
   
   def wrapScope(body : => Unit) {
     div(attributes=List("xmlns" -> utils.xml.namespace("html"),
                         "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
       body
     }
   }
   
   private def doName(s: String) {
      span("name") {text(s)}
   }
   
   private def doName(path : Path, loadable : Boolean) : Unit = loadable match {
     case true =>
       span(cls = "name loadable", attributes = List("jobad:load" -> path.toPath)) {
         text(path.last)
       }
     case false => doName(path.last)
   }
   
   private def doMath(t: Obj) {
        rh(mmlPres.asString(t))
   }
   private def doComponent(comp: DeclarationComponent, t: Obj) {
      td {span {text(comp.toString)}}
      td {doMath(t)}
   }
   private def doNotComponent(comp: NotationComponent, tn: parser.TextNotation) {
      td {span {text(comp.toString)}}
      td {span {text(tn.toText)}}
   }
   private val scriptbase = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/script/"
   private val cssbase    = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/css/"
   
   def doTheory(t: DeclaredTheory) {
     println(t.toNode)
     div("theory") {
       div("theory-header") {doName(t.name.toString)}
       t.getPrimitiveDeclarations.foreach {
         case PlainInclude(from,to) => 
           span("include") {
             table {
               tr {
                 td { span("keyword") {text("include")} }
                 td { span("jobad:mmturi") {text(from.toPath)} }
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
       doName(c.name.toPath)
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
         doNarrativeObject(fd.content)
       }
     case n : NotationDefinition => 
       div(cls = "flexiformal notation",
       attributes = List(("jobad:denotes" -> n.target.toPath))) {
         doNarrativeObject(fd.content)
       }
     case x => 
       throw ImplementationError("Presentation for " + x.getClass() + " not implemented yet")
   }
   
   def doNarrativeObject(no : NarrativeObject) : Unit = no match {
     case t : NarrativeText =>
       rh(<span class="narrative-text"> {t.text} </span>)
     case r : NarrativeRef => r.self match {
     	case false => rh(<span jobad:href={r.target.toPath}> {r.text} </span>)
     	case true => rh(<span class="definiendum" jobad:href={r.target.toPath}> {r.text} </span>)
     }
     case tm : NarrativeTerm => mmlPres(tm.term)(rh)
     case n : NarrativeNode => 
       rh.writeStartTag(n.node.prefix, n.node.label, n.node.attributes, n.node.scope)
       n.child.map(doNarrativeObject)
       rh.writeEndTag(n.node.prefix, n.node.label)
   }
   
   def doView(v: DeclaredView) {}
   override def exportNamespace(dpath: DPath, bd: BuildDir, namespaces: List[(BuildDir,DPath)], modules: List[(BuildFile,MPath)]) {
     div("namespace") {
        namespaces.foreach {case (bd, dp) =>
            div("subnamespace") {
               val name = bd.dirName + "/" + bd.outFile.segments.last
               a(name) {
                  text(dp.toPath)
               }
            }
         }
         modules.foreach {case (bf, mp) =>
            div("submodule") {
               a(bf.outFile.segments.last) {
                  text(mp.toPath)
               }
            }
         }
      }
   }
   
   def doDocument(doc: Document) {
     println(doc.toNode)
     div("document") {
       span("name") {
         text(doc.path.last)
       }
       ul("doc-body") { doc.getItems foreach {
         case d: DRef => 
           //val doc = controller.getDocument(d.path)
           li("dref") {
             doName(d.target, true) 
           }
         case m : MRef =>
           controller.get(m.target) match {
             case t : DeclaredTheory => 
               li("mref") {
                 doTheory(t)
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

