package info.kwarc.mmt.python

import info.kwarc.mmt.api._
import documents._
import modules._
import symbols._
import objects._
import presentation._

class InNotebookHTMLPresenter(oP: ObjectPresenter) extends Presenter(oP) {
  val key = "notebook-presenter"
  def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler): Unit = {
     val htmlRh = utils.HTML(s => rh(s))
     val ps = new PresentationScope(htmlRh)
     ps(e)
  }

  def exceptionAsHTML(e : Exception): String = {
    val html = new utils.HTMLBuilder
    import html._
    text("Error:\n")
    pre { code { text(Error(e).toStringLong) } }
    html.result
  }
  
  /** local class so that we can import htmlRh and build HTML programmatically */
  private class PresentationScope(htmlRh: utils.HTML)(implicit rh : RenderingHandler) {
     import htmlRh._
     def apply(e: StructuralElement): Unit = {
        e match {
          case mr: MRef =>
            apply(controller.get(mr.target))
          case thy: Theory =>
            doKeyword("theory")
            doName(thy.name)
            thy.meta foreach {m =>
              doOperator(":")
              doPath(m)
            }
            doDefComponent(thy)
          case v: View =>
            doKeyword("view")
            doName(v.name)
            doOperator(":")
            doTerm(v.from)
            doOperator("-->")
            doTerm(v.to)
            doDefComponent(v)
          case nm: NestedModule =>
            apply(nm.module)
            // always empty in a notebook
          case c: Constant =>
            doName(c.name)
            c.tp foreach {t =>
              doOperator(":")
              doTerm(t)
            }
            c.df foreach {t =>
              doOperator("=")
              doTerm(t)
            }
          case Include(id) =>
            doKeyword("include")
            doPath(id.from)
            val last = id.args.length
            if (last != 0) {
              doOperator("(")
              id.args.zipWithIndex.foreach {case (a,i) =>
                doTerm(a)
                if (i != last) doOperator(",")
              }
              doOperator(")")
            }
          case s: Structure =>
            doKeyword("structure")
            doName(s.name)
            doOperator(":")
            doTerm(s.from)
            doDefComponent(s)
          case se => text("object " + se.path)
        }
     }
     /* definiens */
     def doDefComponent(m: ModuleOrLink): Unit = {
       m.dfC.get foreach {df =>
         doOperator("=")
         doTerm(df)
       }
     }
     /** names of new declarations */
     def doName(l: LocalName): Unit = {
        span("name") {
          text(l.toString)
        }
     }
     /** references to previous declarations */
     def doPath(p: Path): Unit = {
       span("uri", attributes = List(HTMLAttributes.href -> p.toPath)) {
         text(p.toString)
       }
     }
     /** terms */
     def doTerm(t: Term): Unit = {
       // handled via the provided object presenter
       oP(t, None)(rh)
     }
     /** concrete syntax: alphanumeric keywords */
     def doKeyword(k: String): Unit = {
       span("keyword") {
         text(k)
       }
     }
     /** concrete syntax: symbolic operators */
     def doOperator(s: String): Unit = {
        span("operator") {
          text(s)
        }
     }
  }
}
 