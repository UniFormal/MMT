package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Closer
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.symbols.{PlainViewInclude, FinalConstant}
import info.kwarc.mmt.api.{LocalName, ComplexStep, MPath, GlobalName}
import info.kwarc.mmt.api.modules.{DeclaredView, DeclaredTheory}

/** Convenience class; wraps around a list of assignments, represented as pairs of GlobalNames. Can contain
  * other Viewsets representing morphism inclusions, CAN be named (by setting var path) but doesn't have to be.
  * @param ass The list of assignments
  * @param sub included morphisms
  */

sealed trait Viewset {
  // all assignments (recursively)
  def assignments : List[(GlobalName,GlobalName)] = localassignments:::includes.flatMap(_.assignments)
  // all assignments locally declared (nonrecursively)
  var localassignments : List[(GlobalName,GlobalName)]
  var from : Option[DeclaredTheory] = None
  var to : Option[DeclaredTheory] = None
  // this can be given a path for the toView method
  private var path : Option[MPath] = None
  def setPath(p:MPath) {path = Some(p)}
  def getPath = path
  // direct view inclusions, can be computed by modularize
  var includes : List[Viewset]
  // two ways to modularize: ismodular respects view inclusions wrt theory inclusions in the domain (corresponds to
  // a well-formed view). isdistributed additionally distributes assignments to theory inclusions in the codomain and
  // does not represent a well-formed view, but useful for e.g. intersections.
  var ismodular : Boolean = false
  var isdistributed : Boolean = false
  def setfromto(source:DeclaredTheory,target:DeclaredTheory) = {from = Some(source); to = Some(target)}
  // all view inclusions recursively
  def allincludes : List[Viewset] = includes:::includes.flatMap(_.allincludes)

  // evaluates this view
  private def getdomcod(implicit controller:Controller) : (List[FinalConstant],List[FinalConstant]) = {
    require(from.isDefined && to.isDefined)

    val closer = new Closer(controller)
    val dominc = closer.getIncludes(from.get, true)
    val codinc = closer.getIncludes(to.get, true)

    val domc = ((dominc diff codinc).flatMap(t => t.getConstants) collect { case c: FinalConstant => c }).toList
    val codc = ((codinc diff dominc).flatMap(t => t.getConstants) collect { case c: FinalConstant => c }).toList
    (domc,codc)
  }
  def evaluateOnDom(implicit controller: Controller): Double = {
    require(from.isDefined && to.isDefined)
    val (domc, codc) = getdomcod
    assignments.count(p => domc.exists(q => (ComplexStep(q.path.module.toMPath) / q.name) == p._1.name)).toDouble / domc.length
  }

  def evaluateOnCod(implicit controller: Controller): Double = {
    require(from.isDefined && to.isDefined)
    val (domc, codc) = getdomcod
    assignments.count(p => domc.exists(q => (ComplexStep(q.path.module.toMPath) / q.name) == p._1.name)).toDouble / codc.length
  }
  def evaluate(implicit controller: Controller): Double = {
    require(from.isDefined && to.isDefined)
    val (domc, codc) = getdomcod

    assignments.count(p => domc.exists(q => (ComplexStep(q.path.module.toMPath) / q.name) == p._1.name)).toDouble /
      (if (domc.length < codc.length) domc.length else codc.length).toDouble
  }

  // creates an instance of DeclaredView from this
  // returns a pair consisting of this view and a list of all (recursively included) views generated in the process

  def toView(implicit controller:Controller) : (DeclaredView,List[DeclaredView]) = {
    require(from.isDefined && to.isDefined)
    val (doc,name) = if (path.isDefined) (path.get.^^,path.get.name) else (from.get.parent,LocalName(this.hashCode().toString))
    val v = new DeclaredView(doc,name,from.get.toTerm,to.get.toTerm,false)
    Moduleadder(v,localassignments,controller)
    val includeviews = includes.map(_.toView)
    includeviews.foreach(x => v add PlainViewInclude(v.toTerm,x._1.from.toMPath,x._1.toTerm.toMPath))
    (v,v::includeviews.flatMap(_._2))
  }

  private def canclaim(thp:MPath, dec: GlobalName)(implicit controller : Controller) : Boolean = {
    val th = controller.get(thp) match {
      case t:DeclaredTheory => t
      case _ => return false
    }
    (dec.module.toMPath == thp && th.declares(dec.name)) || th.getIncludes.exists(p => canclaim(p,dec))
  }

  // tries to find the lowest theory in the inclusion tree of thp that declares all elements in decs
  private def getbottomth(thp:MPath, decs: List[GlobalName])(implicit controller : Controller) : Option[MPath] = {
    if (!decs.forall(x => canclaim(thp,x))) None else {
      val th = controller.get(thp) match {
        case t:DeclaredTheory => t
        case _ => return None // Should be impossible
      }
      val candidate = th.getIncludes.find(p => decs.forall(x => canclaim(p,x)))
      if (candidate.isDefined) getbottomth(candidate.get,decs) else Some(thp)
    }
  }

  // tries to make this modular, i.e. transform assignments into view inclusions whenever possible

  def modularize(implicit controller : Controller) {
    require(from.isDefined && to.isDefined)
    // checks whether theory thp or any included theory declares dec
    if (ismodular) return

    val ass = assignments
    includes = Nil
    localassignments = ass.filter(p => p._1.module.toMPath == from.get.path && from.get.declares(p._1.name))
    var nonlocal = ass.filter(p => !localassignments.contains(p))
    val incs = from.get.getIncludes
    val newto = getbottomth(to.get.path,ass.map(_._2))
    if (newto.isDefined) to = controller.get(newto.get) match {
      case t:DeclaredTheory => Some(t)
      case _ => to // Should be impossible
    }
    if (nonlocal.nonEmpty) incs.foreach(pth => controller.get(pth) match {
      case t:DeclaredTheory =>
        val claims = nonlocal.filter(pair => canclaim(pth,pair._1))
        if (claims.nonEmpty) {
          val vset = Viewset.getNew(claims.toSet)
          vset.setfromto(t, to.get)
          vset.modularize
          includes ::= vset
        }
      case _ =>
    })
    nonlocal = nonlocal.filter(p => !includes.exists(q => q.assignments.contains(p)))
    localassignments:::=nonlocal
    ismodular = true
    isdistributed = false
  }

  def distribute(implicit controller : Controller) {
    require(from.isDefined && to.isDefined)
    // checks whether theory thp or any included theory declares dec
    if (isdistributed) return
    val ass = assignments
    includes = Nil
    localassignments = ass.filter(p => p._1.module.toMPath == from.get.path && from.get.declares(p._1.name) &&
      p._2.module.toMPath == to.get.path && to.get.declares(p._2.name))
    var nonlocal = ass.filter(p => !localassignments.contains(p))
    val incsleft = from.get.getIncludes
    val incsright = to.get.getIncludes
    val newto = getbottomth(to.get.path,ass.map(_._2))
    if (newto.isDefined) to = controller.get(newto.get) match {
      case t:DeclaredTheory => Some(t)
      case _ => to // Should be impossible
    }
    if (nonlocal.nonEmpty) incsright.foreach(pth => controller.get(pth) match{
      case t:DeclaredTheory =>
        val claims = nonlocal.filter(pair => canclaim(pth,pair._2))
        if (claims.nonEmpty) {
          val vset = Viewset.getNew(claims.toSet)
          val newfrom = getbottomth(from.get.path,claims.map(_._1)) map controller.get match {
            case Some(th:DeclaredTheory) => Some(th)
            case _ => None
          }
          vset.setfromto(newfrom.getOrElse(from.get),t)
          vset.distribute
          includes ::= vset
        }
      case _ =>
    })
    nonlocal = nonlocal.filter(p => !includes.exists(q => q.assignments.contains(p)))
    if (nonlocal.nonEmpty) incsleft.foreach(pth => controller.get(pth) match {
      case t:DeclaredTheory =>
        val claims = nonlocal.filter(pair => canclaim(pth,pair._1))
        if (claims.nonEmpty) {
          val vset = Viewset.getNew(claims.toSet)
          vset.setfromto(t, to.get)
          vset.distribute
          includes ::= vset
        }
      case _ =>
    })
    nonlocal = nonlocal.filter(p => !includes.exists(q => q.assignments.contains(p)))
    localassignments:::=nonlocal
    ismodular = false
    isdistributed = true
  }

  private def toStringIndent(ind:String) : String =
    ind+(if (path.isDefined) path.get.name.toString else "NoPath")+" : "+
      (if (from.isDefined) from.get.name.toString else "NoDomain")+" ---> "+
      (if (to.isDefined) to.get.name.toString else "NoCodomain")+
      includes.map(x => "\n"+x.toStringIndent(ind+"  ")).mkString("\n")+
      localassignments.map(p => "\n  "+ind+p._1.name+" -> "+p._2.name).mkString("")

  override def toString = toStringIndent("")

}

// Factory for Viewsets with a couple of convenience methods
object Viewset {

  class VS(val ass:Set[(GlobalName,GlobalName)],val sub:Set[Viewset]) extends Viewset {
    var localassignments = ass.toList
    var includes = sub.toList
  }

  private var existing : Set[VS] = Set()

  private def getConstants(v: DeclaredView, from: Option[DeclaredTheory], to: Option[DeclaredTheory])(implicit controller: Controller) = {
    val closer = new Closer(controller)
    val dom = if (from.isDefined) from.get
    else controller.get(v.from.toMPath) match {
      case t: DeclaredTheory => t
      case _ => throw new Exception("expected declared theory")
    }

    val cod = if (to.isDefined) to.get
    else controller.get(v.to.toMPath) match {
      case t: DeclaredTheory => t
      case _ => throw new Exception("expected declared theory")
    }

    val dominc = closer.getIncludes(dom, true)
    val codinc = closer.getIncludes(cod, true)

    val domc = ((dominc diff codinc).flatMap(t => t.getDeclarations) collect { case c: FinalConstant => c }).toList
    val codc = ((codinc diff dominc).flatMap(t => t.getDeclarations) collect { case c: FinalConstant => c }).toList

    (domc, codc)
  }

  private def flatDomain(v:DeclaredView)(implicit controller: Controller) : List[LocalName]
  = v.domain.toList:::v.getIncludes.flatMap(p => controller.get(p) match {
    case w:DeclaredView => flatDomain(w)
    case _ => List()
  })

  def evaluateOnDom(v: DeclaredView, from: Option[DeclaredTheory], to: Option[DeclaredTheory])(implicit controller: Controller): Double = {
    val (domc, codc) = getConstants(v, from, to)
    flatDomain(v).count(p => domc.exists(q => (ComplexStep(q.path.module.toMPath) / q.name) == p)).toDouble / domc.length
  }

  def evaluateOnCod(v: DeclaredView, from: Option[DeclaredTheory], to: Option[DeclaredTheory])(implicit controller: Controller): Double = {
    val (domc, codc) = getConstants(v, from, to)
    flatDomain(v).count(p => domc.exists(q => (ComplexStep(q.path.module.toMPath) / q.name) == p)).toDouble / codc.length
  }

  def evaluateView(v: DeclaredView, from: Option[DeclaredTheory], to: Option[DeclaredTheory])(implicit controller: Controller): Double = {
    val (domc, codc) = getConstants(v, from, to)
    flatDomain(v).count(p => domc.exists(q => (ComplexStep(q.path.module.toMPath) / q.name) == p)).toDouble / (if (domc.length < codc.length) domc.length else codc.length).toDouble
  }

  def apply(v:DeclaredView)(implicit controller : Controller) : Viewset = {
    val (dom,cod) = (controller.get(v.from.toMPath), controller.get(v.to.toMPath)) match {
      case p:(DeclaredTheory,DeclaredTheory) => p
      case _ => return Viewset(Set(),Set())
    }
    val ass : List[(GlobalName,GlobalName)] = v.getDeclarations.map{
      case d:FinalConstant => d.df match {
        case Some(OMS(p:GlobalName)) => controller.get(v.from.toMPath?d.name) match {
          case c:FinalConstant => Some((c.path,p))
          case _ => None
        }
        case _ => None
      }
    }.filter(_.isDefined).map(_.get)
    val includes = v.getIncludes map controller.get collect {
      case w:DeclaredView => w
    }
    val vs = Viewset(ass.toSet,(includes map apply).toSet,Some(v.path))
    vs.setfromto(dom,cod)
    vs
  }

  def apply(ass:Set[(GlobalName,GlobalName)],sub:Set[Viewset] = Set(), path:Option[MPath] = None) : Viewset = {
    require(ass.nonEmpty || sub.nonEmpty)
    existing.find(vs => (vs.getPath.isDefined && path.isDefined && path.get==vs.getPath.get) || (vs.ass==ass && vs.sub==sub)).getOrElse({
      // println("Creating new Viewset for "+ass+" and "+sub+ " with path "+path)
      val vs = new VS(ass,sub)
      if (path.isDefined) vs.setPath(path.get)
      existing+=vs
      vs
    })
  }

  def getNew(ass:Set[(GlobalName,GlobalName)],sub:Set[Viewset] = Set(), path:Option[MPath] = None) : Viewset = {
    val i = existing.find(vs => (vs.getPath.isDefined && path.isDefined && path.get==vs.getPath.get) || (vs.ass==ass && vs.sub==sub))
    if (i.isDefined) existing-=i.get
    apply(ass,sub,path)
  }
}
