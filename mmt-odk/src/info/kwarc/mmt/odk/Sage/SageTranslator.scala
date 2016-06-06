package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api.{DPath, LocalName, MPath}
import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.documents.{DRef, Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import info.kwarc.mmt.api.utils.JSONObject

import scala.collection.mutable

/**
  * Created by raupi on 02.06.16.
  */

case class Axiom(name : String)

class SageTranslator(controller: Controller, bt: BuildTask, index: Document => Unit) {
  var allaxioms : List[Axiom] = Nil

  val theories : mutable.HashMap[String,DeclaredTheory] = mutable.HashMap.empty
  val intcategories : mutable.HashMap[String,ParsedCategory] = mutable.HashMap.empty
  var missings : List[String] = Nil
  def categories(s : String) : ParsedCategory = intcategories(s)/* intcategories.getOrElse(s,{
    missings ::= s
    ParsedCategory(s,Nil,Nil,Nil,"INTERNAL",JSONObject(Nil))
  }) */

  val topdoc = new Document(DPath({Sage._base / "axioms"}.uri.setExtension("omdoc")),root = true)
  val axth = new DeclaredTheory(Sage._base,LocalName("Axioms"),Some(Sage.theory))
  controller add topdoc
  controller add axth
  controller add MRef(topdoc.path,axth.path)
  val docs : mutable.HashMap[DPath,Document] = mutable.HashMap((Sage.docpath,topdoc))

  private def doCategory(cat : ParsedCategory): Unit =
  theories.getOrElse(cat.name, {
    val th = new DeclaredTheory(cat.path.parent, cat.path.name, Some(Sage.theory))
    controller add th

    val doc = docs.getOrElse(th.parent,{
      val ndoc = new Document(DPath(th.parent.uri.setExtension("omdoc")),root = true)
      controller add ndoc
      //controller add new DRef(topdoc.path,)
      docs(th.parent) = ndoc
      ndoc
    })
    controller add MRef(doc.path,th.path)

    controller add PlainInclude(axth.path, th.path)
    theories(cat.name) = th

    cat.includes foreach (s => doCategory(categories(s)))
    cat.includes foreach (s => controller add PlainInclude(theories(s).path,th.path))
    val importedaxioms = cat.includes.flatMap(s => categories(s).axioms).distinct

    val newaxioms = cat.axioms.filter(!importedaxioms.contains(_))
    newaxioms foreach (ax => {
      val c = Constant(th.toTerm, LocalName(ax), Nil, Some(Sage.ded(OMS(axth.path ? ax))), None, None)
      controller add c
    })
    // TODO more stuff

    //controller add MRef(Sage.catdoc, th.path)
  }
  )

  def apply(ls : List[SageObject]) = {
    // Do Axioms
    ls foreach {
      case pc @ ParsedCategory(name,_,axioms,_,_,_) =>
        allaxioms :::= axioms map Axiom
        intcategories += ((name,pc))
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
    //index(doc)
    //index(catdoc)
    docs.values foreach index
    missings.distinct foreach println
  }
}
