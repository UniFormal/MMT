package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._
import documents._
import presentation._
import frontend._
import objects._
import utils._

class HTMLContentExporter extends ContentExporter {
   val key = "content-html"
   val outDim = Dim("export", "html", "content")
   override val outExt = "html"
   private lazy val mmlPres = new presentation.MathMLPresenter(controller) // must be lazy because controller is provided in init only

   // provides easy-to-use HTML markup inside Scala code
   private val htmlRh = new utils.HTML(s => rh(s))
   import htmlRh._
   
   private def doName(s: String) {
      span("name") {rh(s)}
   }
   private def doMath(t: Obj) {
      math {
        rh(mmlPres.asString(t))
      }
   }
   private def doComponent(comp: DeclarationComponent, t: Obj) {
      td {span {rh(comp.toString)}}
      td {doMath(t)}
   }
   private def doHTML(b: => Unit) {
      html {
        head {
         css("file:///c:/other/oaff/test/html.css")
         javascript("file:/c:/other/oaff/test/html.js")
         javascript("http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js")
        }
        body {
           b
        }
      }
   }
   def doTheory(t: DeclaredTheory, bf: BuildFile) {
      doHTML {div("theory") {
         div("theory-header") {doName(t.name.toPath)}
         t.getPrimitiveDeclarations.foreach {
            d => table("constant") {
               tr("constant-header") {
                    td {doName(d.name.toPath)}
                    td {
                       def toggle(label: String) {
                          span("compToggle", onclick = s"toggle(this,'$label')") {rh(label)}
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
                  case (comp, nc: NotationContainer) =>
                     //TODO render notations
               }
               if (! d.metadata.getTags.isEmpty) tr("tags") {
                  td {rh("tags")}
                  td {d.metadata.getTags.foreach {
                     k => div("tag") {rh(k.toPath)}
                  }}
               }
               def doKey(k: GlobalName) {
                  td{span("key", title=k.toPath) {rh(k.toString)}}
               }
               d.metadata.getAll.foreach {
                  case metadata.Link(k,u) => tr("link metadata") {
                     doKey(k)
                     td {a(u.toString) {rh(u.toString)}}
                  }
                  case md: metadata.MetaDatum => tr("metadatum metadata") {
                     doKey(md.key)
                     td {doMath(md.value)}
                  }
               }
            }
         }
      }}
   }
   def doView(v: DeclaredView, bf: BuildFile) {}
   def doNamespace(dpath: DPath, namespaces: List[(BuildDir,DPath)], modules: List[(BuildFile,MPath)]) {
      doHTML {div("namespace") {
         namespaces.foreach {case (bd, dp) =>
            div("subnamespace") {
               val name = bd.dirName + "/" + bd.outFile.segments.last
               a(name) {
                  rh(dp.toPath)
               }
            }
         }
         modules.foreach {case (bf, mp) =>
            div("submodule") {
               a(bf.outFile.segments.last) {
                  rh(mp.toPath)
               }
            }
         }
      }}
   }
}

class HTMLNarrationExporter extends NarrationExporter {
   val key = "narration-html"
   val outDim = Dim("export", "html", "narration")
   override val outExt = "html"
   private lazy val mmlPres = new presentation.MathMLPresenter(controller) // must be lazy because controller is provided in init only

   // provides easy-to-use HTML markup inside Scala code
   private val htmlRh = new utils.HTML(s => rh(s))
   import htmlRh._
   
   def doDocument(doc: Document, bt: BuildTask) {
      html {
         body {
            ul {doc.getItems foreach {
               case d: DRef =>
                  li("dref") {
                     a(d.target.toPath) {
                        rh(d.target.last)
                     }
                  }
               case m: MRef =>
                  li("mref") {
                     a(m.target.toPath) {
                        rh(m.target.last)
                     }
                  }
            }}
         }
      }
   }
}