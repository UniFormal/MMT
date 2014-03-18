package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._
import documents._
import presentation._
import frontend._
import objects._
import utils._
import flexiformal._

trait HTMLPresenter extends Presenter {
   override val outExt = "html"
   private lazy val mmlPres = new presentation.MathMLPresenter(controller) // must be lazy because controller is provided in init only
     
   def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
     this._rh = rh
     s match { 
       case doc : Document => 
         doHTMLOrNot(doc.path, standalone) {doDocument(doc)}
       case thy : DeclaredTheory => 
         doHTMLOrNot(thy.path.doc, standalone) {doTheory(thy)}
       case view : DeclaredView =>
         doHTMLOrNot(view.path.doc, standalone) {doView(view)}
       case _ => rh("TODO: Not implemented yet, presentation function for " + s.getClass().toString())
     }
     //TODO? reset this._rh 
   }
   
   def apply(o : Obj)(implicit rh : RenderingHandler) = mmlPres(o)(rh)
   
   def isApplicable(format : String) = format == "html"
   
   // easy-to-use HTML markup
   protected val htmlRh = new utils.HTML(s => rh(s))
   import htmlRh._
   
   private def doName(s: String) {
      span("name") {text(s)}
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
   /*
    * @param dpath identifies the directory (needed for relative paths)
    */
   private def doHTMLOrNot(dpath: DPath, doit: Boolean)(b: => Unit) {
      if (! doit) {
        return div(attributes=List("xmlns" -> utils.xml.namespace("html"), "xmlns:jobad" -> utils.xml.namespace("jobad"))) {b}
      }
      val pref = Range(0,dpath.uri.path.length+2).map(_ => "../").mkString("")
      html(attributes=List("xmlns" -> utils.xml.namespace("html"))) {
        head {
          css(cssbase+"mmt.css")
          css(cssbase+"JOBAD.css")
          css(cssbase+"jquery/jquery-ui.css")
          css(pref + "html.css")
          javascript(scriptbase + "jquery/jquery.js")
          javascript(scriptbase + "jquery/jquery-ui.js")
          javascript(scriptbase + "mmt/mmt-html.js")
          javascript(scriptbase + "mmt/mmt-js-api.js")
          javascript(scriptbase + "jobad/deps/underscore-min.js")
          javascript(scriptbase + "jobad/JOBAD.js")
          javascript(scriptbase + "jobad/modules/navigation.js")
          javascript(scriptbase + "jobad/modules/hovering.js")
          javascript(scriptbase + "jobad/modules/interactive-viewing.js")
          javascript(pref + "html.js")
        }
        body {
           b
        }
      }
   }
   def doTheory(t: DeclaredTheory) {
      div("theory") {
         div("theory-header") {doName(t.name.toString)}
         t.getPrimitiveDeclarations.foreach {
            d => div("constant") {table("constant") {
               tr("constant-header") {
                    td {doName(d.name.toString)}
                    td {
                       def toggle(label: String) {
                          span("compToggle", onclick = s"toggle(this,'$label')") {text(label)}
                       }
                       d.getComponents.foreach {case (comp, tc) => if (tc.isDefined) 
                          toggle(comp.toString)
                       }
                       if (! d.metadata.getTags.isEmpty)
                          toggle("tags")
                       if (! d.metadata.getAll.isEmpty)
                          toggle("metadata")
                    }
               }
               d.getComponents.foreach {
                  case (comp, tc: AbstractTermContainer) =>
                     tr(comp.toString) {
                        tc.get.foreach {t =>
                            doComponent(comp, t)
                        }
                     }
                  case (comp: NotationComponent, nc: NotationContainer) =>
                     tr(comp.toString) {
                        nc(comp).foreach {n =>
                           doNotComponent(comp, n)
                         }
                     }
                  case (comp, no : flexiformal.NarrativeObject) =>
                    tr(comp.toString) {
                      td { doNarrativeObject(no) }
                    }
               }
               if (! d.metadata.getTags.isEmpty) tr("tags") {
                  td {text("tags")}
                  td {d.metadata.getTags.foreach {
                     k => div("tag") {text(k.toPath)}
                  }}
               }
               def doKey(k: GlobalName) {
                  td{span("key", title=k.toPath) {text(k.toString)}}
               }
               d.metadata.getAll.foreach {
                  case metadata.Link(k,u) => tr("link metadata") {
                     doKey(k)
                     td {a(u.toString) {text(u.toString)}}
                  }
                  case md: metadata.MetaDatum => tr("metadatum metadata") {
                     doKey(md.key)
                     td {doMath(md.value)}
                  }
               }
            }
         }}
      }
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
      doHTMLOrNot(dpath, true) {div("namespace") {
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
      }}
   }
   
   def doDocument(doc: Document) {
     div("document") {
       span("name") {
         text(doc.path.last)
       }
       ul { doc.getItems foreach {
         case d: DRef => 
           li("dref") {
             controller.get(d.target) match {
               case doc : Document => doDocument(doc)
               case _ => //nothing to do
             } 
           }
         case m : MRef =>
           li("mref") {
             controller.get(m.target) match {
               case thy : DeclaredTheory => doTheory(thy)
               case _ => //nothing to do
             }
           }
       }}
     }
   }
}

class HTMLExporter extends HTMLPresenter{
  val key = "html"
  val outDim = Dim("export", "html")
}


class MMTDocExporter extends HTMLPresenter {
  val key = "mmtdoc"
  val outDim = Dim("export", "mmtdoc")
  import htmlRh._

  override def doDocument(doc: Document) {
      html {
         body {
            ul {doc.getItems foreach {
               case d: DRef =>
                  li("dref") {
                     a(d.target.toPath) {
                        text(d.target.last)
                     }
                  }
               case m: MRef =>
                  li("mref") {
                     a(m.target.toPath) {
                        text(m.target.last)
                     }
                  }
            }}
         }
      }
   }
}
 

