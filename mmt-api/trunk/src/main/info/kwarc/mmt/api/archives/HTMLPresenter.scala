package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._

abstract class HTMLPresenter(objectPresenter: ObjectPresenter) extends Presenter(objectPresenter) {
   override val outExt = "html"

   def apply(s : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) = {
     this._rh = rh
     s match {
       case doc : Document =>
         doHTMLOrNot(doc.path, standalone) {doDocument(doc)}
       case thy : DeclaredTheory =>
         doHTMLOrNot(thy.path.doc, standalone) {doTheory(thy)}
       case view : DeclaredView =>
         doHTMLOrNot(view.path.doc, standalone) {doView(view)}
       case d: Declaration => doHTMLOrNot(d.path.doc, standalone) {doDeclaration(d)}
     }
     this._rh = null
   }

   // easy-to-use HTML markup
   protected val htmlRh = utils.HTML(s => rh(s))
   import htmlRh._

   private def doName(p: ContentPath) {
      val (name, path) = p.name match {
         case LocalName(ComplexStep(t)::Nil) => (t.name.toString, t) // hardcoding the import case of includes
         case n => (n.toString, p)
      }
      span("name", attributes=List(("jobad:href",path.toPath))) {text(name)}
   }
   /** renders a MMT URI outside a math object */
   private def doPath(p: Path) {
      span("mmturi", attributes=List("jobad:href" -> p.toPath)) {
         val pS = p match {
            case d: DPath => d.last
            case m: MPath => m.name.toString
            case g: GlobalName => g.name.toString
            case c: CPath => c.parent.name.toString + "?" + c.component.toString
         }
         text {pS}
      }
   }
   private def doMath(t: Obj, owner: Option[CPath]) {
        objectLevel(t, owner)(rh)
   }
   private def doComponent(cpath: CPath, t: Obj) {
      td {span("compLabel") {text(cpath.component.toString)}}
      td {doMath(t, Some(cpath))}
   }
   private def doNotComponent(cpath: CPath, tn: TextNotation) {
      td {span("compLabel") {text(cpath.component.toString)}}
      td {span {
         val firstVar = tn.arity.firstVarNumberIfAny
         val firstArg = tn.arity.firstArgNumberIfAny
         text {tn.markers.map {
            case Arg(n,_) =>
               val argNum = n-firstArg
               if (argNum < 5)
                  List("a", "b", "c", "d", "e")(argNum)
               else
                  "a" + argNum.toString
            case ImplicitArg(n,_) =>
               val argNum = n-firstArg
               if (argNum < 3)
                  List("I", "J", "K")(argNum)
               else
                  "I" + argNum.toString
            case SeqArg(n, sep,_) => n.toString + sep.text + "..." + sep.text + n.toString
            case Var(n, typed, sepOpt,_) =>
               val varNum = n-firstVar
               val varname = if (varNum < 3)
                  List("x", "y", "z")(varNum)
               else
                  "x" + varNum.toString
               val typedString = if (typed) ":_" else ""
               sepOpt match {
                  case None => varname + typedString
                  case Some(sep) => varname + typedString + sep.text + "..." + sep.text + varname + typedString
               }
            case Delim(s) => s
            case SymbolName() => cpath.parent.name.toPath
            case m => m.toString
         }.mkString(" ")}
         text {" (precedence " + tn.precedence.toString + ")"}
      }}
   }
   private val scriptbase = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/script/"
   private val cssbase    = "https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/css/"
   /**
    * @param dpath identifies the directory (needed for relative paths)
    * @param doit if true, wrap HTML header etc. around argument, otherwise, return arguments as a div
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

   def doDeclaration(d: Declaration) {
            val usedby = controller.depstore.querySet(d.path, -ontology.RefersTo).toList.sortBy(_.toPath)
            div("containerconstant toggleTarget inlineBoxSibling  panelHorizontalScroll ") {
               div(" constant-header") {
                 span {doName(d.path)}

                 def toggle(label: String) {
                    button("compToggle  btn btn-sm btn-default pull-right", onclick = s"interaction.toggleClick(this.parentNode,'$label')") {text(label)}
                 }
                 d.getComponents.foreach {case (comp, tc) => if (tc.isDefined)
                    toggle(comp.toString)
                 }
                 //if (! usedby.isEmpty)
                    toggle("used-by")
                 //if (! d.metadata.getTags.isEmpty)
                    toggle("tags")
                 //if (! d.metadata.getAll.isEmpty)
                    toggle("metadata")
               }
               table("constant-components ") {
                  d.getComponents.foreach {
                     case (comp, tc: AbstractTermContainer) =>
                        tr(comp.toString) {
                           tc.get.foreach {t =>
                               doComponent(d.path $ comp, t)
                           }
                        }
                     case (comp: NotationComponent, nc: NotationContainer) =>
                        tr(comp.toString) {
                           nc(comp).foreach {n =>
                              doNotComponent(d.path $ comp, n)
                            }
                        }
                  }
                  if (! usedby.isEmpty) {
                     tr("used-by") {
                        td {span("compLabel") {text{"used by"}}}
                        td {usedby foreach doPath}
                     }
                  }
                  if (! d.metadata.getTags.isEmpty)
                     tr("tags") {
                     td {span("compLabel"){text{" ---tags"}}}
                     td {d.metadata.getTags.foreach {
                        k => div("tag") {text(k.toPath)}
                     }}
                  }
                  def doKey(k: GlobalName) {
                     td{span("key compLabel", title=k.toPath) {text(k.toString)}}
                  }
                  d.metadata.getAll.foreach {
                     case metadata.Link(k,u) => tr("link metadata") {
                        doKey(k)
                        td {a(u.toString) {text(u.toString)}}
                     }
                     case md: metadata.MetaDatum => tr("metadatum metadata") {
                        doKey(md.key)
                        td {doMath(md.value, None)}
                     }
                  }
               }
            }
   }

   def doTheory(t: DeclaredTheory) {
      div("theory container-fluid") {
         div("theory-header", onclick="toggleClick(this)") {doName(t.path)}
         t.getPrimitiveDeclarations foreach doDeclaration
      }
   }
   def doView(v: DeclaredView) {}
   override def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {
      doHTMLOrNot(dpath, true) {div("namespace") {
         namespaces.foreach {case bd =>
            div("subnamespace") {
               val name = bd.dirName + "/" + bd.outFile.segments.last
               a(name) {
                  text(bd.contentDPath.toPath)
               }
            }
         }
         modules.foreach {case bf =>
            div("submodule") {
               a(bf.outFile.segments.last) {
                  text(bf.contentMPath.toPath)
               }
            }
         }
      }}
   }

   def doDocument(doc: Document) {
     val locOpt = controller.backend.resolveLogical(doc.path.uri)
     val svgOpt = locOpt flatMap {
       case (arch, path) =>
         val fpath = Archive.narrationSegmentsAsFile(FilePath(path), "omdoc")
         val f = (arch.root / "export" / "svg" / "narration" / fpath).setExtension("svg")
         if (f.toJava.exists())
            Some("/:svg?"+doc.path.uri.toString)
         else
           None
     }
     div("document") {
       span("name") {
         text(doc.path.last)
       }
       ul("ref") { doc.getItems foreach {
         case d: DRef =>
           li("dref") {
             span(cls = "name loadable", attributes=List("jobad:load" -> d.target.toPath)) {
               text(d.target.last)
             }
           }
         case m : MRef =>
           li("mref") {
             span(cls = "name loadable", attributes=List("jobad:load" -> m.target.toPath)) {
               text(m.target.last)
             }
           }
       }}
       svgOpt foreach {src =>
          div("graph") {
            htmlobject(src, "image/svg+xml")
          }
       }
     }
   }
}

class HTMLExporter extends HTMLPresenter(new MathMLPresenter) {
  val key = "html"
}


class MMTDocExporter extends HTMLPresenter(new MathMLPresenter) {
  val key = "mmtdoc"
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


