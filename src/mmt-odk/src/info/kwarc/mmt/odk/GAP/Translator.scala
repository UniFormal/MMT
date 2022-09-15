package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.symbols._

import info.kwarc.mmt.lf._

import scala.collection.mutable


object Translator {
  def objtotype(o : GAPObject) : Term = o match {
    case p : GAPProperty => GAP.propfilt(p)
    case c : GAPCategory => GAP.catfilt(c)
    case f : GAPFilter => f.toTerm
    case p : GAPOperation => GAP.propfilt(p)
    case _ => throw new ParseError("Neither Filter, Category nor Property: " + o)
  }
}

class Translator(controller: Controller, bt: BuildTask, index: Document => Unit, log : GAPJSONImporter) {

  private val theories : mutable.HashMap[MPath, Theory] = mutable.HashMap.empty
  private val docs : mutable.HashMap[DPath, Document] = mutable.HashMap.empty /*((Path.parseD("http://www.gap-system.org/",NamespaceMap.empty),
  new Document(Path.parseD("http://www.gap-system.org/",NamespaceMap.empty),root = true))) */

  def apply(all : List[DeclaredObject]) : Unit = {

    log.toplog("Creating Theories...")
    all foreach doObject

    log.toplog("Checking...")
    log.toplogGroup {
      val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
        throw GeneralError(s"no mmt checker found")
      }
      // theories foreach (th => checker(th._2)(new CheckingEnvironment(controller.simplifier, new ErrorLogger(controller.report), RelationHandler.ignore, bt)))
    }
    docs.values foreach index
  }

  private def typeax(f : GAPObject, inputs : List[List[GAPObject]], retob : GAPObject) : Term = {
    val con = Context((1 to inputs.length).map(i => LocalName("x" + i)).map(n => VarDecl(n,GAP.obj)) :_*)
    val rettp = GAP.dotp(ApplySpine(f.toTerm,con.map(vd => OMV(vd.name)) :_*), retob)
    val realtm = inputs.indices.foldRight[Term](rettp)((i,t) => inputs(i).foldRight[Term](t)((o,tm) =>
      Arrow(GAP.dotp(OMV(LocalName("x" + {i+1})),o),tm)))
    Pi(con,realtm)
  }

  private var opcounter = 0

  private var dones : List[DeclaredObject] = List(IsBool,IsObject)

  import FilterRelations._

  private def doObject(obj : DeclaredObject) : Unit = {
    if (dones contains obj) return ()
    obj.dependencies.foreach(doObject)
    val (deps,cs) = obj match {
      case df : DefinedFilter =>
        var consts = List(Constant(OMMOD(df.path.module),df.name,Nil,Some(GAP.filter),df.defi,None))
        addDependencies(df.path.module,df.dependencies)
        (IsFilter(consts.last.path) :: doImpls(consts.head,df.dependencies),consts)

      case prop : DeclaredProperty =>
        val filters = prop.filters//(dones)
        var consts = List(Constant(OMMOD(prop.path.module),prop.name,Nil,Some(doArrow(1)),None,None),
          Constant(OMMOD(prop.path.module),LocalName(prop.name.toString + "_type"),Nil,Some(typeax(prop,List(filters),IsBool)),None,None))
        addDependencies(prop.path.module,obj.dependencies)
        (IsAttribute(consts.last.path) :: doImpls(consts.head,prop.dependencies),consts)
      case a : DeclaredAttribute =>
        val filters = a.filters
        var consts = List(Constant(OMMOD(a.path.module),a.name,Nil,Some(Arrow(GAP.obj,GAP.obj)),None,None))
        //addConstant(c)
        if (a.returntype.isDefined) {
          consts ::= Constant(OMMOD(a.path.module),LocalName(a.name.toString + "_type"),Nil,Some(
            typeax(a,List(filters),a.returntype.get)
          ),None,None)
        }
        addDependencies(a.path.module,obj.dependencies)
        (IsAttribute(consts.last.path) :: doImpls(consts.head,a.dependencies),consts)
      case a : Constructor =>
        val filters = a.filters
        var consts = List(Constant(OMMOD(a.path.module),a.name,Nil,Some(Arrow(GAP.obj,GAP.obj)),None,None))
        //addConstant(c)
        if (a.returntype.isDefined) {
          consts ::= Constant(OMMOD(a.path.module),LocalName(a.name.toString + "_type"),Nil,Some(
            typeax(a,List(filters),a.returntype.get)
          ),None,None)
        }
        addDependencies(a.path.module,obj.dependencies)
        (doImpls(consts.head,a.dependencies),consts)
      case op : DeclaredOperation =>
        opcounter = 0
        val filters = op.filters
        var consts = List(Constant(OMMOD(op.path.module),op.name,Nil,Some(
          if (op.arity.isDefined) doArrow(op.arity.get) else doArrow(1)),None,None))
        //addConstant(c)
        if (op.arity.isDefined) consts ::= Constant(OMMOD(op.path.module),LocalName(op.name.toString + "_type"),Nil,
          Some(???),None,None)
        // TODO TODO TODO
        /*
        if (op.arity.isDefined) op.methods.foreach (m => {
          ???
        })
        */
        val allfilters = filters.flatten.flatten// ::: op.methods.flatMap(_.filters(dones).flatten)
        addDependencies(op.path.module,obj.dependencies)
        (IsAttribute(consts.last.path) :: doImpls(consts.head,op.dependencies),consts)
      case cat : DeclaredCategory =>
        opcounter = 0
        val filters = cat.implied//(dones)
        if (cat == IsBool || cat == IsObject) return ()
        var consts = List(Constant(OMMOD(cat.path.module),cat.name,Nil,Some(GAP.cat),None,None))
        filters foreach (f =>
          {
            consts ::= Constant(OMMOD(cat.path.module),LocalName(cat.name.toString + "_st" + opcounter),Nil,
              Some(Pi(LocalName("x"),GAP.obj,Arrow(
                GAP.dotp(OMV("x"),cat),
                GAP.dotp(OMV("x"),f)))),None,None
            )
            opcounter+=1
          })
        addDependencies(cat.path.module,obj.dependencies)
        (IsFilter(consts.last.path) :: doImpls(consts.head,cat.dependencies),consts)
      case filt : DeclaredFilter =>
        opcounter = 0
        val filters = filt.implied//(dones)
        if (filters.flatMap(_.getInner).exists(o => !dones.contains(o))) filters.flatMap(_.getInner) foreach doObject
        var consts = List(Constant(OMMOD(filt.path.module),filt.name,Nil,Some(GAP.filter),None,None))
        filters foreach (f =>
        {
          consts ::= Constant(OMMOD(filt.path.module),LocalName(filt.name.toString + "_st" + opcounter),Nil,
            Some(Pi(LocalName("x"),GAP.obj,Arrow(
              GAP.dotp(OMV("x"),filt),
              GAP.dotp(OMV("x"),f)))),None,None
          )
          opcounter+=1
        })
        addDependencies(filt.path.module,obj.dependencies)
        (IsFilter(consts.last.path) :: doImpls(consts.head,filt.dependencies),consts)
      case rep : GAPRepresentation =>
        opcounter = 0
        val filters = rep.implied//(dones)
        if (filters.flatMap(_.getInner).exists(o => !dones.contains(o))) filters.flatMap(_.getInner) foreach doObject
        var consts = List(Constant(OMMOD(rep.path.module),rep.name,Nil,Some(GAP.filter),None,None))
        filters foreach (f =>
        {
          consts ::= Constant(OMMOD(rep.path.module),LocalName(rep.name.toString + "_st" + opcounter),Nil,
            Some(Pi(LocalName("x"),GAP.obj,Arrow(
              GAP.dotp(OMV("x"),rep),
              GAP.dotp(OMV("x"),f)))),None,None
          )
          opcounter+=1
        })
        addDependencies(rep.path.module,obj.dependencies)
        (IsFilter(consts.last.path) :: doImpls(consts.head,rep.dependencies),consts)
    }
    dones ::= obj
    if (cs.nonEmpty) {
      addConstants(cs.reverse)
      gaprels += ((cs.last,deps))
    }
  }

  private def doArrow(arity : Int) : Term = if (arity == 0) GAP.obj else Arrow(GAP.obj,doArrow(arity - 1))

  private def addDependencies(thp : MPath, objs : List[DeclaredObject]): Unit = objs foreach (obj => {
    addTheory(thp)
    val th = theories.get(thp).get
    if (!(th.getIncludes.contains(obj.path.module) || obj.path.module == th.path)) controller add PlainInclude(obj.path.module,thp)
  })
  private def doImpls(c : Constant, objs : List[DeclaredObject]) = objs.map(obj => Implies(c.path,obj.path))

  private def addConstants(c : List[FinalConstant]) = if (c.nonEmpty) {
    addTheory(c.head.parent)
    c foreach {d => controller.add(d)}
  }

  private def addTheory(mp : MPath) =  if (theories.get(mp).isEmpty) {
    val th = Theory.empty(mp.parent,mp.name,Some(GAP.theory))
    theories += ((th.path,th))
    controller add th
    addtoDoc(mp)
  }

  private def addtoDoc(mp : MPath) = {
    val doc = docs.getOrElse(mp.doc,{
      val ret = new Document(DPath(mp.doc.uri.setExtension("omdoc")), FileLevel)
      controller add ret
      docs(mp.doc) = ret
      ret
    })
    controller add MRef(doc.path,mp)
  }

}
