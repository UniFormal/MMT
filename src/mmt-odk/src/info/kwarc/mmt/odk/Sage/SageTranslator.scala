package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.opaque._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf.{Arrow, Typed}
import info.kwarc.mmt.odk.LFX.ModelsOf

import scala.collection.mutable
import scala.util.Try

/**
  * Created by raupi on 02.06.16.
  */

case class Axiom(name : String)
case class Structure(name : String)

class SageTranslator(controller: Controller, bt: BuildTask, index: Document => Unit, log : (=> String) => Unit) {
  var allaxioms : List[Axiom] = Nil
  var allstructures : List[Structure] = Nil

  val theories : mutable.HashMap[String,Theory] = mutable.HashMap.empty
  val intcategories : mutable.HashMap[String,ParsedCategory] = mutable.HashMap.empty
  val intclasses : mutable.HashMap[String,ParsedClass] = mutable.HashMap.empty
  var missings : List[String] = Nil
  def sobject(s : String) : SageObject = intcategories.getOrElse(s,intclasses(s))/* intcategories.getOrElse(s,{
    missings ::= s
    ParsedCategory(s,Nil,Nil,Nil,"INTERNAL",JSONObject(Nil))
  }) */

  val topdoc = new Document(DPath({Sage._base / bt.inFile.name}.uri.setExtension("")), FileLevel)
  controller.add(topdoc)

  val axth = controller.getO(Sage._base ? LocalName("Axioms")) match {
    case Some(dc : Theory) => dc
    case _ =>
      val ret = Theory.empty(Sage._base, LocalName("Axioms"), Some(Sage.theory))
      controller.add(ret)
      controller add MRef(topdoc.path,ret.path)
      ret
  } //
  val structh = Theory.empty(Sage._base,LocalName("Structures"), Some(Sage.theory))
  add(structh)
  add(MRef(topdoc.path,structh.path))
  val sagedoc = controller.getO(Sage.docpath / "sage").getOrElse({
    val nd = new Document(Sage.docpath / "sage", FileLevel)
    controller add nd
    nd
  }).asInstanceOf[Document]
  val docs : mutable.HashMap[String,Document] = mutable.HashMap(("",topdoc),("sage",sagedoc))

  def getDoc(d : List[String]) : Document = docs.getOrElseUpdate(d.mkString("."), {
      val dpath = d.foldLeft(Sage.docpath)((d,s) => d / s)
      val level = if (dpath.^ == dpath) FileLevel else SectionLevel
      val ndoc = new Document(dpath, level)
      if (ndoc.parentOpt.isDefined) getDoc(d.init)
      controller.add(ndoc)
      //controller add new DRef(topdoc.path,)
      ndoc
    })

  private def add(se : StructuralElement) : Unit = Try(controller.getO(se.path)) match {
    case scala.util.Success(Some(_)) =>
    case _ => controller add se
  }

  private def doMethod(m : SageMethod)(implicit th: Theory, sec : Option[String]) = {
    val c = Constant(th.toTerm, LocalName(m.tp + "." + m.name), Nil, Some(doArity(m.arity)), None, None)
    if (sec.isDefined) c.setDocumentHome(LocalName(sec.get))
    controller add c
    addOpaque(m.doc, th, sec)
    //if (m.optional) addOpaque("(Optional)", th, sec)
  }

  private def doFunction(f : SageFunction) = {
    val parent = theories.getOrElseUpdate(f.steps.init.mkString("."),{
      val pt = Theory.empty(f.theory.parent,f.theory.name,Some(Sage.theory))
      controller add pt
      addOpaque(f.doc, pt)
      val pdoc = getDoc(f.document)
      controller add MRef(pdoc.path,pt.path)
      pt
    })
    val c = Constant(parent.toTerm,f.name,Nil,Some(doArity(f.arity)),None,Some("Function"))
    add(c)
  }

  private def doSageObject(obj : SageObject) : Unit = obj match {
    case pc : ParsedClass => doClass(pc)
    case pc : ParsedCategory => doCategory(pc)
    case _ => throw ImplementationError("doSageObject() expected a ParsedClass or ParsedCategory")
  }

  private def doClass(clss : ParsedClass) =
    theories.getOrElse(clss.n, {
      implicit val th = Theory.empty(clss.theory.parent, clss.theory.name, Some(Sage.theory))
      controller add th
      addOpaque(clss.doc, th)

      val doc = getDoc(clss.document)
      controller.add(MRef(doc.path, th.path))

      controller.add(PlainInclude(axth.path, th.path))
      controller.add(PlainInclude(structh.path, th.path))
      theories(clss.n) = th

      clss.includes foreach (s => doSageObject(sobject(s)))
      clss.includes foreach (s => controller add PlainInclude(theories(s).path, th.path))

      implicit var sec: Option[String] = None

      if (clss.methods.nonEmpty) {
        sec = Some("Methods")
        controller add new Document(th.path.toDPath / sec.get, SectionLevel)
        clss.methods foreach doMethod
      }

      val parent = theories.getOrElseUpdate(clss.steps.init.mkString("."), {
        val pt = Theory.empty(clss.classes_theory.parent,clss.classes_theory.name,Some(Sage.theory))
        controller add pt
        val pdoc = getDoc(clss.classes_document)
        controller add MRef(pdoc.path,pt.path)
        pt
      })
      val c = Constant(parent.toTerm, clss.name,Nil,Some(OMS(Typed.ktype)),Some(ModelsOf(th.toTerm)),Some("Class"))
      controller add c
    }
    )

  private def doCategory(cat : ParsedCategory): Unit =
  theories.getOrElse(cat.n, {
    // val doc = getDoc(cat.path.parent)
    implicit val th = Theory.empty(cat.theory.parent, cat.theory.name, Some(Sage.theory))
    controller.add(th)
    addOpaque(cat.doc, th)

    val doc = getDoc(cat.document)
    controller.add(MRef(doc.path,th.path))

    if (cat.gap != "") println(th.path.toString + " ~> " + cat.gap)

    controller.add(PlainInclude(axth.path, th.path))
    controller.add(PlainInclude(structh.path, th.path))
    theories(cat.n) = th

    cat.includes foreach (s => doSageObject(sobject(s)))
    cat.includes foreach (s => controller add PlainInclude(theories(s).path,th.path))
    val importedaxioms = cat.includes.map(sobject).flatMap{
      case s : ParsedCategory => s.axioms
      case _ => throw ImplementationError("expected ParsedCategory")
    }.distinct
    val importedstructs = (cat.includes ::: cat.structure.filterNot(_ == cat.steps.last)).map(sobject).flatMap{
      case s :ParsedCategory => s.structure
      case _ => throw ImplementationError("expected ParsedCategory")
    }.distinct
    val newaxioms = cat.axioms.filter(!importedaxioms.contains(_))
    val newstructs = cat.structure.filter(s => !importedstructs.contains(s) && allstructures.contains(Structure(s)))

    newaxioms foreach (ax => {
      val c = Constant(th.toTerm, LocalName(ax), Nil, Some(Sage.ded(OMS(axth.path ? ax))), None, None)
      add(c)
    })
    newstructs foreach (s => controller add Constant(th.toTerm,LocalName(s),Nil,Some(Sage.structof(s)),None,None))
    //addOpaque("Element Methods:",th)
    implicit var sec : Option[String] = None


    if (cat.elem_methods._2.nonEmpty) {
      sec = Some("Element Methods")
      controller add new Document(th.path.toDPath / sec.get, SectionLevel)
      if (cat.elem_methods._1 != "") addOpaque(cat.elem_methods._1, th, sec)
      cat.elem_methods._2 foreach doMethod
    }

    if (cat.morph_methods._2.nonEmpty) {
      sec = Some("Morphism Methods")
      controller add new Document(th.path.toDPath / sec.get, SectionLevel)
      if (cat.morph_methods._1 != "") addOpaque(cat.morph_methods._1, th, sec)
      cat.morph_methods._2 foreach doMethod
    }

    if (cat.parent_methods._2.nonEmpty) {
      sec = Some("Parent Methods")
      controller add new Document(th.path.toDPath / sec.get, SectionLevel)
      if (cat.parent_methods._1 != "") addOpaque(cat.parent_methods._1, th, sec)
      cat.parent_methods._2 foreach doMethod
    }

    if (cat.subcategory_methods._2.nonEmpty) {
      sec = Some("Subcategory Methods")
      controller add new Document(th.path.toDPath / sec.get, SectionLevel)
      if (cat.subcategory_methods._1 != "") addOpaque(cat.subcategory_methods._1, th, sec)
      cat.subcategory_methods._2 foreach doMethod
    }
    //controller add MRef(Sage.catdoc, th.path)
  }
  )

  private def addOpaque(text : String,th : Theory, sec : Option[String] = None) =
    controller add new OpaqueText(if (sec.isDefined) th.path.toDPath / sec.get else th.path.toDPath, OpaqueText.defaultFormat, StringFragment(text))

  private def doArity(ar : Int) : Term = if (ar == 0) OMS(Sage.obj) else Arrow(OMS(Sage.obj),doArity(ar-1))

  def apply(ls : List[SageObject]) = {
    // Do Axioms
    ls foreach {
      case pc @ ParsedCategory(name,_,axioms,_,_,_,_,_,_,_,_) =>
        allaxioms :::= (axioms map Axiom).reverse
        if (pc.isStructure) allstructures ::= Structure(pc.steps.last)
        intcategories += ((name,pc))
      case pc @ ParsedClass(name,_,_,_) =>
        intclasses += ((name,pc))
      case _ =>
    }
    allaxioms = allaxioms.distinct
    allstructures = allstructures.distinct
    // TODO weird hack around weird bug
    // add(Constant(axth.toTerm,LocalName("Dummy"),Nil,None,None,None))

    allaxioms foreach (ax => {
      val c = Constant(axth.toTerm,LocalName(ax.name),Nil,Some(OMS(Sage.prop)),None,None)
      add(c)
    })
    allstructures foreach (st => {
      val c = Constant(structh.toTerm,LocalName(st.name),Nil,Some(OMS(Sage.structure)),None,None)
      add(c)
    })

    ls foreach {
      case cat : ParsedCategory =>
        doCategory(cat)
      case pc : ParsedClass =>
        doClass(pc)
      case fun : SageFunction =>
        doFunction(fun)
      case _ =>
    }
    val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
      throw GeneralError(s"no mmt checker found")
    }
    // TODO infinite loop, apparently
    // (axth :: structh :: theories.values.toList) foreach (th => checker(th)(new CheckingEnvironment(controller.simplifier, new ErrorLogger(controller.report), RelationHandler.ignore, MMTTask.generic)))
    //println(axth)
    //index(doc)
    //index(catdoc)
    theories.values.toList.sortBy(_.path.toString) foreach (t => controller add MRef(topdoc.path,t.path))
    docs.values foreach index
    missings.distinct.foreach(m => println("Missing: " + m))
  }
}
