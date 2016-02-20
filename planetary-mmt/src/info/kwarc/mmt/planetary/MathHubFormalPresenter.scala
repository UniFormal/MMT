package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import modules._
import symbols._
import documents._
import presentation._
import frontend._
import objects._
import utils._
import informal._
import archives._
import opaque._
import notations._
import utils.xml._
import HTMLAttributes._


class MathHubFormalPresenter extends HTMLPresenter(new MathMLPresenter) {
   val key = "mh-html"
   import htmlRh._

   override def doDocument(doc: Document) {
     div {
        doNarrativeElementInDoc(doc)
     }
   }
   
   /** captures common parts of narrative and content element rendering */
   override def doNarrativeElement(ne: NarrativeElement, recurse: NarrativeElement => Unit) {ne match {
      case doc: Document =>
        div("document toggle-root inlineBoxSibling") {
          div("document-header", attributes=List(toggleTarget -> "document-body")) {
             val name = doc.path.last
             span("name") {
                text(name)
             }
             NarrativeMetadata.title.get(doc).foreach {t =>
                text(": ")
                text(t)
             } 
          }
          div("document-body") {
             doc.getDeclarations foreach recurse
          }
        }
      case oe: OpaqueElement =>
         val oi = controller.extman.get(classOf[OpaqueHTMLPresenter], oe.format)
                  .getOrElse(DefaultOpaqueElementInterpreter)
         div("opaque-"+oe.format + " inlineBoxSibling") {
            oi.toHTML(objectPresenter, oe)(rh)
         }
      case m: MRef if controller.get(m.target).isInstanceOf[DeclaredTheory] => 
        div {
          doTheory(controller.get(m.target).asInstanceOf[DeclaredTheory])
        }
      case m: MRef if controller.get(m.target).isInstanceOf[DeclaredView] =>   
        div{
          doView(controller.get(m.target).asInstanceOf[DeclaredView])
        }
      case s : SRef if controller.get(s.target).isInstanceOf[Declaration] => 
        div {
          doDeclaration(controller.get(s.target).asInstanceOf[Declaration])
        }
      case r: NRef => //default case
        val label = r match {
           case _:DRef => "dref"
           case _:MRef => "mref"
           case _:SRef => "sref"
        }
        div("document-"+label + " inlineBoxSibling") {
          span(cls = "name mmturi loadable", attributes=List(load -> r.target.toPath)) {
            val hideName = r.name.steps.forall(_==LNStep.empty) || (r match {
               case r:MRef =>
                  r.nameIsTrivial
               case _ => false
            })
            if (!hideName) {
               text(r.name.toString)
               literal(" &#8594; ")
            }
            text(r.target.toString)
          }
        }
   }}
}