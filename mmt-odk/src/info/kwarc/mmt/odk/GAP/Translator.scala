package info.kwarc.mmt.odk.GAP

import info.kwarc.mmt.api.{LocalName, _}
import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, RelationHandler}
import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant, PlainInclude}
import info.kwarc.mmt.lf.{ApplySpine, Arrow, Pi}

import scala.collection.mutable

class Translator(controller: Controller, bt: BuildTask, index: Document => Unit, log : JSONImporter) {
  var dones : List[GAPObject] = Nil

  private var gap : GAPReader = null

  private var theories : mutable.HashMap[MPath, DeclaredTheory] = mutable.HashMap.empty

  def apply(ngap:GAPReader) = {
    gap = ngap
    log.toplog("sorting...")
    var i = 0
    dones = gap.all.sortBy(o => o.depweight(gap.all))
    log.toplog("imported " + dones.length + " GAP Objects. Head: " + dones.head)

    log.toplog("Creating Theories...")
    // dones foreach doObject

    log.toplog("Checking...")
    log.toplogGroup {
      val checker = controller.extman.get(classOf[Checker], "mmt").getOrElse {
        throw GeneralError(s"no mmt checker found")
      }
      theories foreach (th => checker(th._2)(new CheckingEnvironment(new ErrorLogger(log.reporter), RelationHandler.ignore)))
    }
    val dnames = theories.map(t => t._2.parent).toList.distinct
    val docths = dnames.map(d => (d,theories.collect {case (_,th) if d <= th.path => th}))
    val docs = docths.map(p => {
      val doc = new Document(p._1)
      p._2.foreach(t => doc add MRef(p._1,t.path))
      doc
    })
    docs foreach index
  }

  private def objtotype(o : GAPObject) : Term = o match {
    case f : GAPFilter => f.toTerm
    case p : Property => GAP.propfilt(p.toTerm)
    case c : Category => GAP.catfilt(c.toTerm)
    case _ => throw new ParseError("Neither Filter, Category nor Property: " + o)
  }

  private def typeax(f : GAPObject, inputs : List[List[GAPObject]], retob : GAPObject) : Term = {
    val con = Context((1 to inputs.length).map(i => LocalName("x" + i)).map(n => VarDecl(n,Some(GAP.obj),None,None)) :_*)
    val rettp = GAP.dotp(ApplySpine(f.toTerm,con.map(vd => OMV(vd.name)) :_*), objtotype(retob))
    val realtm = inputs.indices.foldRight[Term](rettp)((i,t) => inputs(i).foldRight[Term](t)((o,tm) =>
      Arrow(GAP.dotp(OMV(LocalName("x" + {i+1})),objtotype(o)),tm)))
    Pi(con,realtm)
  }

  private var opcounter = 0

  private def doObject(obj : GAPObject) : Unit = {
    val cs : List[FinalConstant] = obj match {
      case m : GAPMethod =>
        Nil
      case a : GAPAttribute =>
        val filters = a.filters(dones)
        var consts = List(Constant(OMMOD(a.path.module),a.name,Nil,Some(Arrow(GAP.obj,GAP.obj)),None,None))
        //addConstant(c)
        if (a.returntype.isDefined) {
          consts ::= Constant(OMMOD(a.path.module),LocalName(a.name + "_type"),Nil,Some(
            typeax(a,List(filters),a.returntype.get)
          ),None,None)
        }
        addDependencies(a.path.module,filters.map(_.getInner).distinct)
        consts
      case op : GAPOperation =>
        opcounter = 0
        val filters = op.filters(dones)
        var consts = List(Constant(OMMOD(op.path.module),op.name,Nil,Some(
          if (op.arity.isDefined) doArrow(op.arity.get) else doArrow(1)),None,None))
        //addConstant(c)
        if (op.arity.isDefined) consts ::= Constant(OMMOD(op.path.module),LocalName(op.name + "_type"),Nil,
          Some(???),None,None)
        // TODO TODO TODO
        if (op.arity.isDefined) op.methods.foreach (m => {
          ???
        })
        val allfilters = filters.flatten.flatten ::: op.methods.flatMap(_.filters(dones).flatten)
        addDependencies(op.path.module,allfilters.map(_.getInner).distinct)
        consts
      case cat : GAPCategory =>
        opcounter = 0
        val filters = cat.filters(dones)
        if (cat == IsBool) return ()
        var consts = List(Constant(OMMOD(cat.path.module),cat.name,Nil,Some(GAP.cat),None,None))
        filters foreach (f =>
          {
            consts ::= Constant(OMMOD(cat.path.module),LocalName(cat.name + "_st" + opcounter),Nil,
              Some(Pi(LocalName("x"),GAP.obj,Arrow(
                GAP.dotp(OMV("x"),GAP.catfilt(cat.toTerm)),
                GAP.dotp(OMV("x"),objtotype(f))))),None,None
            )
            opcounter+=1
          })
        addDependencies(cat.path.module,filters.map(_.getInner).distinct)
        consts
      case filt : GAPFilter =>
        opcounter = 0
        val filters = filt.filters(dones)
        var consts = List(Constant(OMMOD(filt.path.module),filt.name,Nil,Some(GAP.filter),None,None))
        filters foreach (f =>
        {
          consts ::= Constant(OMMOD(filt.path.module),LocalName(filt.name + "_st" + opcounter),Nil,
            Some(Pi(LocalName("x"),GAP.obj,Arrow(
              GAP.dotp(OMV("x"),filt.toTerm),
              GAP.dotp(OMV("x"),objtotype(f))))),None,None
          )
          opcounter+=1
        })
        addDependencies(filt.path.module,filters.map(_.getInner).distinct)
        consts
      case prop : GAPProperty =>
        val filters = prop.filters(dones)
        var consts = List(Constant(OMMOD(prop.path.module),prop.name,Nil,Some(doArrow(1)),None,None),
          Constant(OMMOD(prop.path.module),LocalName(prop.name + "_type"),Nil,Some(typeax(prop,List(filters),IsBool)),None,None))
        addDependencies(prop.path.module,filters.map(_.getInner).distinct)
        consts
      case rep : GAPRepresentation =>
        opcounter = 0
        val filters = rep.filters(dones)
        var consts = List(Constant(OMMOD(rep.path.module),rep.name,Nil,Some(GAP.filter),None,None))
        filters foreach (f =>
        {
          consts ::= Constant(OMMOD(rep.path.module),LocalName(rep.name + "_st" + opcounter),Nil,
            Some(Pi(LocalName("x"),GAP.obj,Arrow(
              GAP.dotp(OMV("x"),rep.toTerm),
              GAP.dotp(OMV("x"),objtotype(f))))),None,None
          )
          opcounter+=1
        })
        addDependencies(rep.path.module,filters.map(_.getInner).distinct)
        consts
    }
    addConstants(cs.reverse)
  }

  private def doArrow(arity : Int) : Term = if (arity == 0) GAP.obj else Arrow(GAP.obj,doArrow(arity - 1))

  private def addDependencies(thp : MPath, objs : List[Haspath]): Unit = objs foreach (obj => {
    if (theories.get(thp).isEmpty) {
      val th = new DeclaredTheory(thp.parent,thp.name,Some(GAP.theory))
      theories += ((th.path,th))
      controller add th
    }
    val th = theories.get(thp).get
    if (!th.getIncludes.contains(obj.path.module)) controller add PlainInclude(obj.path.module,thp)
  })

  private def addConstants(c : List[FinalConstant]) = {
    if (theories.get(c.head.parent).isEmpty) {
      val th = new DeclaredTheory(c.head.parent.parent,c.head.parent.name,Some(GAP.theory))
      theories += ((th.path,th))
      controller add th
    }
    c foreach controller.add
  }
}
