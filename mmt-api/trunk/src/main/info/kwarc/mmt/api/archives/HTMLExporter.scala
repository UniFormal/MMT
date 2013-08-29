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
   private lazy val mmlPres = new MathMLPresenter(controller) // must be lazy because controller is provided in init only
   private def html(body: => Unit) {
      rh("<html><body>")
      body
      rh("</body></html>")
   }
   private def div(cls: String)(body: => Unit) {
      rh(s"""<div class="$cls">""")
      body
      rh("</div>")
   }
   private def span(cls: String)(body: => Unit) {
      rh(s"""<span class="$cls">""")
      body
      rh("</span>")
   }
   private def a(ref: String)(body: => Unit) {
      rh(s"""<a href="$ref">""")
      body
      rh("</a>")
   }
   
   private def doName(s: String) {
      rh(s"""<span class="name">$s</span>""")
   }
   private def doComponent(comp: DeclarationComponent, t: Term) {
      div(comp.toString) {
         rh(s"<span>${comp.toString}</span><math>")
         rh(mmlPres.asString(t))
         rh("</math>")
      }
   }
   def doTheory(t: DeclaredTheory) {
      html {div("theory") {
         doName(t.name.toPath)
         t.getPrimitiveDeclarations.foreach {d =>
            doName(d.name.toPath)
            d.getComponents.foreach {case (comp, tc) =>
               tc.get.foreach {t =>
                   doComponent(comp, t)
               }
            }
         }
      }}
   }
   def doView(v: DeclaredView) {}
   def doNamespace(dpath: DPath, namespaces: List[(BuiltDir,DPath)], modules: List[(BuiltFile,MPath)]) {
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

class MathMLPresenter(val controller: Controller) extends presentation.NotationBasedPresenter {
   def getNotation(term: Term) = Presenter.getNotation(controller, term)
   override def doIdentifier(p: ContentPath)(implicit rh : RenderingHandler) {
      val s = p match {
         case OMMOD(m) % name => name.toPath  //not parsable if there are name clashes 
         case _ => p.toPath
      }
      val n = <mo jobad:xref={p.toPath}>{s}</mo>
      rh(n)
   }
   override def doVariable(n: LocalName)(implicit rh : RenderingHandler) {
      val node = <mi>{n.toPath}</mi>
      rh(node)
   }
   override def doOperator(s: String)(implicit rh : RenderingHandler) {
      val n = <mo>{s}</mo>
      rh(n)
   }
   override def doDelimiter(p: GlobalName, d: parser.Delimiter)(implicit rh : RenderingHandler) {
      val n = <mo jobad:xref={p.toPath}>{d.text}</mo>
      rh(n)
   }
   override def doSpace(level: Int)(implicit rh : RenderingHandler) {
      val n = <mspace/>
      rh(n)
   }
}