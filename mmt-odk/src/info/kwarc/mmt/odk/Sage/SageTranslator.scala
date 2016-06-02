package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api.{LocalName, MPath}
import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}

import scala.collection.mutable

/**
  * Created by raupi on 02.06.16.
  */

case class Axiom(name : String)

class SageTranslator(controller: Controller, bt: BuildTask, index: Document => Unit) {
  var allaxioms : List[Axiom] = Nil

  val theories : mutable.HashMap[String,DeclaredTheory] = mutable.HashMap.empty
  val categories : mutable.HashMap[String,ParsedCategory] = mutable.HashMap.empty

  val axth = new DeclaredTheory(Sage.docpath,LocalName("Axioms"),Some(Sage.theory))
  controller add axth
  val doc = new Document(Sage.docpath)
  doc add MRef(Sage.docpath,axth.path)

  private def doCategory(cat : ParsedCategory): Unit =
  theories.getOrElse(cat.name, {
    val th = new DeclaredTheory(cat.path.parent, cat.path.name, Some(Sage.theory))
    controller add th
    controller add PlainInclude(axth.path, th.path)
    theories += ((cat.name, th))

    cat.implied foreach (s => doCategory(categories(s)))
    cat.implied foreach (s => controller add PlainInclude(theories(s).path,th.path))
    val importedaxioms = cat.implied.flatMap(s => categories(s).axioms).distinct

    val newaxioms = cat.axioms.filter(!importedaxioms.contains(_))
    newaxioms foreach (ax => {
      val c = Constant(th.toTerm, LocalName(ax), Nil, Some(Sage.ded(OMS(axth.path ? ax))), None, None)
      controller add c
    })
    // TODO more stuff
    doc add MRef(Sage.docpath, th.path)
  }
  )

  def apply(ls : List[SageObject]) = {
    // Do Axioms
    ls foreach {
      case pc @ ParsedCategory(name,_,axioms,_,_,_) =>
        allaxioms :::= axioms map Axiom
        categories += ((name,pc))
    }
    allaxioms = allaxioms.distinct

    allaxioms foreach (ax => {
      val c = Constant(axth.toTerm,LocalName(ax.name),Nil,Some(OMS(Sage.prop)),None,None)
      controller add c
    })

    ls foreach {
      case cat : ParsedCategory =>
        doCategory(cat)
    }

    index(doc)
  }
}
