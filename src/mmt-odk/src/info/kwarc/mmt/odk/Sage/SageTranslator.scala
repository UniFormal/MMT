package info.kwarc.mmt.odk.Sage

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.opaque._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.lf.Arrow

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

  val theories : mutable.HashMap[String,DeclaredTheory] = mutable.HashMap.empty
  val intcategories : mutable.HashMap[String,ParsedCategory] = mutable.HashMap.empty
  val intclasses : mutable.HashMap[String,ParsedClass] = mutable.HashMap.empty
  var missings : List[String] = Nil
  def sobject(s : String) : SageObject = intcategories.getOrElse(s,intclasses(s))/* intcategories.getOrElse(s,{
    missings ::= s
    ParsedCategory(s,Nil,Nil,Nil,"INTERNAL",JSONObject(Nil))
  }) */

  val topdoc = new Document(DPath({Sage._base / bt.inFile.name}.uri.setExtension("omdoc")),root = true)
  controller.add(topdoc)

  val axth = controller.getO(Sage._base ? LocalName("Axioms")) match {
    case Some(dc : DeclaredTheory) => dc
    case _ =>
      val ret = Theory.empty(Sage._base, LocalName("Axioms"), Some(Sage.theory))
      controller.add(ret)
      controller add MRef(topdoc.path,ret.path)
      ret
  } //
  val structh = Theory.empty(Sage._base,LocalName("Structures"), Some(Sage.theory))
  add(structh)
  add(MRef(topdoc.path,structh.path))
  val docs : mutable.HashMap[DPath,Document] = mutable.HashMap((Sage.docpath,topdoc))

  private def add(se : StructuralElement) : Unit = Try(controller.getO(se.path)) match {
    case scala.util.Success(Some(_)) =>
    case _ => controller add se
  }

  private def doMethod(m : SageMethod)(implicit th : DeclaredTheory, sec : Option[String]) = {
    val c = Constant(th.toTerm, LocalName(m.tp + "." + m.name), Nil, Some(doArity(m.arity)), None, None)
    if (sec.isDefined) c.setDocumentHome(LocalName(sec.get))
    controller add c
    addOpaque(m.doc, th, sec)
    //if (m.optional) addOpaque("(Optional)", th, sec)
  }

  private def doSageObject(obj : SageObject) : Unit = obj match {
    case pc : ParsedClass => doClass(pc)
    case pc : ParsedCategory => doCategory(pc)
  }

  private def doClass(clss : ParsedClass) =
    theories.getOrElse(clss.name, {
      implicit val th = Theory.empty(clss.path.parent, clss.path.name, Some(Sage.theory))
      controller.add(th)

      val doc = docs.getOrElse(th.parent, {
        val ndoc = new Document(DPath(th.parent.uri.setExtension("omdoc")), root = true)
        controller.add(ndoc)
        //controller add new DRef(topdoc.path,)
        docs(th.parent) = ndoc
        ndoc
      })
      controller.add(MRef(doc.path, th.path))

      controller.add(PlainInclude(axth.path, th.path))
      controller.add(PlainInclude(structh.path, th.path))
      theories(clss.name) = th

      clss.includes foreach (s => doSageObject(sobject(s)))
      clss.includes foreach (s => controller add PlainInclude(theories(s).path, th.path))

      implicit var sec: Option[String] = None


      if (clss.methods.nonEmpty) {
        sec = Some("Methods")
        controller add new Document(th.path.toDPath / sec.get, root = false)
        clss.methods foreach doMethod
      }
    }
    )

  private def doCategory(cat : ParsedCategory): Unit =
  theories.getOrElse(cat.name, {
    implicit val th = Theory.empty(cat.path.parent, cat.path.name, Some(Sage.theory))
    controller.add(th)

    val doc = docs.getOrElse(th.parent,{
      val ndoc = new Document(DPath(th.parent.uri.setExtension("omdoc")),root = true)
      controller.add(ndoc)
      //controller add new DRef(topdoc.path,)
      docs(th.parent) = ndoc
      ndoc
    })
    controller.add(MRef(doc.path,th.path))

    controller.add(PlainInclude(axth.path, th.path))
    controller.add(PlainInclude(structh.path, th.path))
    theories(cat.name) = th

    cat.includes foreach (s => doSageObject(sobject(s)))
    cat.includes foreach (s => controller add PlainInclude(theories(s).path,th.path))
    val importedaxioms = cat.includes.map(sobject).flatMap{case s : ParsedCategory => s.axioms}.distinct
    val importedstructs = (cat.includes ::: cat.structure.filterNot(_ == cat.name)).map(sobject).flatMap{case s :ParsedCategory => s.structure}.distinct
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

  private def addOpaque(text : String,th : DeclaredTheory, sec : Option[String] = None) =
    controller add new OpaqueText(if (sec.isDefined) th.path.toDPath / sec.get else th.path.toDPath,List(StringFragment(text)))

  private def doArity(ar : Int) : Term = if (ar == 0) OMS(Sage.obj) else Arrow(OMS(Sage.obj),doArity(ar-1))

  def apply(ls : List[SageObject]) = {
    // Do Axioms
    ls foreach {
      case pc @ ParsedCategory(name,_,axioms,_,_,_,_,_,_,_,_) =>
        allaxioms :::= (axioms map Axiom).reverse
        if (pc.isStructure) allstructures ::= Structure(pc.name)
        intcategories += ((name,pc))
      case pc @ ParsedClass(name,_,_,_) =>
        intclasses += ((name,pc))
    }
    allaxioms = allaxioms.distinct
    allstructures = allstructures.distinct
    // TODO weird hack around weird bug
    add(Constant(axth.toTerm,LocalName("Dummy"),Nil,None,None,None))

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
