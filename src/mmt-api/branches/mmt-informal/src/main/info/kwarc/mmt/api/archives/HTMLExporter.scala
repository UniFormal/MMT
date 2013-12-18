package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import symbols._
import presentation._
import frontend._
import objects._
import utils._

class HTMLExporter extends ContentExporter {
   val outDim = "html"
   val key = "html"
   private lazy val mmlPres = new presentation.MathMLPresenter(controller) // must be lazy because controller is provided in init only
   private def optAttr(key: String, value: String) = if (value == "") "" else s""" $key="$value""""
   private def html(body: => Unit) {
      rh("<html>")
      rh("<head>")
      rh("""<link rel="stylesheet" type="text/css" href="file:///c:/other/oaff/test/html.css"></link>""")
      rh("""<script type="text/javascript" src="file:/c:/other/oaff/test/html.js"></script>""")
      rh("""<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"></script>""")
      rh("</style>")
      rh("</head>")
      rh("<body>")
      body
      rh("</body></html>")
   }
   private def div(cls: String)(body: => Unit) {
      rh(s"""<div class="$cls">""")
      body
      rh("</div>")
   }
   private def span(cls: String, onclick : String = "", title: String = "")(body: => Unit) {
      rh(s"""<span${optAttr("class", cls)}${optAttr("onclick", onclick)}${optAttr("title", title)}>""")
      body
      rh("</span>")
   }
   private def a(ref: String)(body: => Unit) {
      rh(s"""<a href="$ref">""")
      body
      rh("</a>")
   }
   private def table(cls: String)(body: => Unit) {
      rh(s"""<table${optAttr("class", cls)}>""")
      body
      rh("</table>")
   }
   private def tr(cls: String)(body: => Unit) {
      rh(s"""<tr${optAttr("class", cls)}>""")
      body
      rh("</tr>")
   }
   private def td(body: => Unit) {
      rh("<td>")
      body
      rh("</td>")
   }
   private def doName(s: String) {
      rh(s"""<span class="name">$s</span>""")
   }
   private def doMath(t: Obj) {
      rh("<math>")
      rh(mmlPres.asString(t))
      rh("</math>")
   }
   private def doComponent(comp: DeclarationComponent, t: Obj) {
      td {rh(s"<span>${comp.toString}</span>")}
      td {doMath(t)}
   }
   def doTheory(t: DeclaredTheory, bf: BuildFile) {
      html {div("theory") {
         div("theory-header") {doName(t.name.toPath)}
         t.getPrimitiveDeclarations.foreach {
            d => table("constant") {
               tr("constant-header") {
                    td {doName(d.name.toPath)}
                    td {
                       def toggle(label: String) {
                          span("compToggle", s"toggle(this,'$label')") {rh(label)}
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
                  case (comp, tc: TermContainer) =>
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
      html {div("namespace") {
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