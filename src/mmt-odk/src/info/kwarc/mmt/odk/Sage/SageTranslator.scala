package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api.{LocalName, _}
import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, RelationHandler}
import info.kwarc.mmt.api.documents.{DRef, Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.opaque.{OpaqueText, StringFragment}
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import info.kwarc.mmt.api.utils.JSONObject
import info.kwarc.mmt.lf.Arrow

import scala.collection.mutable

/**
  * Created by raupi on 02.06.16.
  */

case class Axiom(name : String)
case class Structure(name : String)

class SageTranslator(controller: Controller, bt: BuildTask, index: Document => Unit) {
  var allaxioms : List[Axiom] = Nil
  var allstructures : List[Structure] = Nil

  val theories : mutable.HashMap[String,DeclaredTheory] = mutable.HashMap.empty
  val intcategories : mutable.HashMap[String,ParsedCategory] = mutable.HashMap.empty
  var missings : List[String] = Nil
  def categories(s : String) : ParsedCategory = intcategories(s)/* intcategories.getOrElse(s,{
    missings ::= s
    ParsedCategory(s,Nil,Nil,Nil,"INTERNAL",JSONObject(Nil))
  }) */

  val topdoc = new Document(DPath({Sage._base / "inheritable"}.uri.setExtension("omdoc")),root = true)
  val axth = new DeclaredTheory(Sage._base,LocalName("Axioms"),Some(Sage.theory))
  val structh = new DeclaredTheory(Sage._base,LocalName("Structures"),Some(Sage.theory))
  controller add topdoc
  controller add axth
  controller add structh
  controller add MRef(topdoc.path,axth.path)
  controller add MRef(topdoc.path,structh.path)
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
    controller add PlainInclude(structh.path, th.path)
    theories(cat.name) = th

    cat.includes foreach (s => doCategory(categories(s)))
    cat.includes foreach (s => controller add PlainInclude(theories(s).path,th.path))
    val importedaxioms = cat.includes.flatMap(s => categories(s).axioms).distinct
    val importedstructs = (cat.includes ::: cat.structure.filterNot(_ == cat.name)).flatMap(s => categories(s).structure).distinct
    val newaxioms = cat.axioms.filter(!importedaxioms.contains(_))
    val newstructs = cat.structure.filter(s => !importedstructs.contains(s) && allstructures.contains(Structure(s)))

    newaxioms foreach (ax => {
      val c = Constant(th.toTerm, LocalName(ax), Nil, Some(Sage.ded(OMS(axth.path ? ax))), None, None)
      controller add c
    })
    newstructs foreach (s => controller add Constant(th.toTerm,LocalName(s),Nil,Some(Sage.structof(s)),None,None))
    //addOpaque("Element Methods:",th)
    var sec : Option[String] = None
    def doMethod(m : SageMethod) = {
      val c = Constant(th.toTerm, LocalName(m.tp + "." + m.name), Nil, Some(doArity(m.arity)), None, None)
      if (sec.isDefined) c.setDocumentHome(LocalName(sec.get))
      controller add c
      addOpaque(m.doc, th, sec)
      //if (m.optional) addOpaque("(Optional)", th, sec)
    }

    if (cat.elem_methods._2.nonEmpty) {
      sec = Some("Element Methods")
      controller add new Document(th.path.toDPath / sec.get,root = false)
      if (cat.elem_methods._1 != "") addOpaque(cat.elem_methods._1, th, sec)
      cat.elem_methods._2 foreach doMethod
    }

    if (cat.morph_methods._2.nonEmpty) {
      sec = Some("Morphism Methods")
      controller add new Document(th.path.toDPath / sec.get, root = false)
      if (cat.morph_methods._1 != "") addOpaque(cat.morph_methods._1, th, sec)
      cat.morph_methods._2 foreach doMethod
    }

    if (cat.parent_methods._2.nonEmpty) {
      sec = Some("Parent Methods")
      controller add new Document(th.path.toDPath / sec.get, root = false)
      if (cat.parent_methods._1 != "") addOpaque(cat.parent_methods._1, th, sec)
      cat.parent_methods._2 foreach doMethod
    }

    if (cat.subcategory_methods._2.nonEmpty) {
      sec = Some("Subcategory Methods")
      controller add new Document(th.path.toDPath / sec.get, root = false)
      if (cat.subcategory_methods._1 != "") addOpaque(cat.subcategory_methods._1, th, sec)
      cat.subcategory_methods._2 foreach doMethod
    }
    //controller add MRef(Sage.catdoc, th.path)
  }
  )

  def addOpaque(text : String,th : DeclaredTheory, sec : Option[String] = None) =
    controller add new OpaqueText(if (sec.isDefined) th.path.toDPath / sec.get else th.path.toDPath,List(StringFragment(text)))

  def doArity(ar : Int) : Term = if (ar == 0) OMS(Sage.obj) else Arrow(OMS(Sage.obj),doArity(ar-1))

  def apply(ls : List[SageObject]) = {
    // Do Axioms
    ls foreach {
      case pc @ ParsedCategory(name,_,axioms,_,_,_,_,_,_,_,_) =>
        allaxioms :::= (axioms map Axiom).reverse
        if (pc.isStructure) allstructures ::= Structure(pc.name)
        intcategories += ((name,pc))
    }
    allaxioms = allaxioms.distinct
    allstructures = allstructures.distinct
    // TODO weird hack around weird bug
    controller add Constant(axth.toTerm,LocalName("Dummy"),Nil,None,None,None)

    allaxioms foreach (ax => {
      val c = Constant(axth.toTerm,LocalName(ax.name),Nil,Some(OMS(Sage.prop)),None,None)
      controller add c
    })
    allstructures foreach (st => {
      val c = Constant(structh.toTerm,LocalName(st.name),Nil,Some(OMS(Sage.structure)),None,None)
      controller add c
    })

    ls foreach {
      case cat : ParsedCategory =>
        doCategory(cat)
    }
    val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
      throw GeneralError(s"no mmt checker found")
    }
    //println(axth)
    (axth :: structh :: theories.values.toList) foreach (th => checker(th)(new CheckingEnvironment(new ErrorLogger(controller.report), RelationHandler.ignore, MMTTask.generic)))
    //println(axth)
    //index(doc)
    //index(catdoc)
    docs.values foreach index
    missings.distinct foreach println
  }
}
