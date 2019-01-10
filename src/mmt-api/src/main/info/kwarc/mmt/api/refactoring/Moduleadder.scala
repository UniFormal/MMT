package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

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
  var from : Option[Theory] = None
  var to : Option[Theory] = None
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
  def setfromto(source:Theory,target:Theory) = {from = Some(source); to = Some(target)}
  // all view inclusions recursively
  def allincludes : List[Viewset] = includes:::includes.flatMap(_.allincludes)

  // evaluates this view
  private def getdomcod(implicit controller:Controller) : (List[FinalConstant],List[FinalConstant]) = {
    require(from.isDefined && to.isDefined)

    val dominc = from.get.getIncludes.map(p => controller.getTheory(p))
    val codinc = to.get.getIncludes.map(p => controller.getTheory(p))

    val domc = ((dominc diff codinc).flatMap(t => t.getConstants) collect { case c: FinalConstant => c }).toList
    val codc = ((codinc diff dominc).flatMap(t => t.getConstants) collect { case c: FinalConstant => c }).toList
    (domc,codc)
  }
  def evaluateOnDom(implicit controller: Controller): Double = {
    require(from.isDefined && to.isDefined)
    val (domc, codc) = getdomcod
    assignments.count(p => domc.exists(q => (ComplexStep(q.path.module) / q.name) == p._1.name)).toDouble / domc.length
  }

  def evaluateOnCod(implicit controller: Controller): Double = {
    require(from.isDefined && to.isDefined)
    val (domc, codc) = getdomcod
    assignments.count(p => domc.exists(q => (ComplexStep(q.path.module) / q.name) == p._1.name)).toDouble / codc.length
  }
  def evaluate(implicit controller: Controller): Double = {
    require(from.isDefined && to.isDefined)
    val (domc, codc) = getdomcod

    assignments.count(p => domc.exists(q => (ComplexStep(q.path.module) / q.name) == p._1.name)).toDouble /
      (if (domc.length < codc.length) domc.length else codc.length).toDouble
  }

  // creates an instance of View from this
  // returns a pair consisting of this view and a list of all (recursively included) views generated in the process

  def toView(implicit controller:Controller) : (View,List[View]) = {
    require(from.isDefined && to.isDefined)
    val (doc,name) = if (path.isDefined) (path.get.^^,path.get.name) else (from.get.parent,LocalName(this.hashCode().toString))
    val v = new View(doc,name,from.get.toTerm,to.get.toTerm,false)
    Moduleadder(v,localassignments,controller)
    val includeviews = includes.map(_.toView)
    includeviews.foreach(x => v add PlainViewInclude(v.toTerm,x._1.from.toMPath,x._1.toTerm.toMPath))
    (v,v::includeviews.flatMap(_._2))
  }

  private def canclaim(thp:MPath, dec: GlobalName)(implicit controller : Controller) : Boolean = {
    val th = controller.get(thp) match {
      case t:Theory => t
      case _ => return false
    }
    (dec.module == thp && th.declares(dec.name)) || th.getIncludes.exists(p => canclaim(p,dec))
  }

  // tries to find the lowest theory in the inclusion tree of thp that declares all elements in decs
  private def getbottomth(thp:MPath, decs: List[GlobalName])(implicit controller : Controller) : Option[MPath] = {
    if (!decs.forall(x => canclaim(thp,x))) None else {
      val th = controller.get(thp) match {
        case t:Theory => t
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
    localassignments = ass.filter(p => p._1.module == from.get.path && from.get.declares(p._1.name))
    var nonlocal = ass.filter(p => !localassignments.contains(p))
    val incs = from.get.getIncludes
    val newto = getbottomth(to.get.path,ass.map(_._2))
    if (newto.isDefined) to = controller.get(newto.get) match {
      case t:Theory => Some(t)
      case _ => to // Should be impossible
    }
    if (nonlocal.nonEmpty) incs.foreach(pth => controller.get(pth) match {
      case t:Theory =>
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
    localassignments = ass.filter(p => p._1.module == from.get.path && from.get.declares(p._1.name) &&
      p._2.module == to.get.path && to.get.declares(p._2.name))
    var nonlocal = ass.filter(p => !localassignments.contains(p))
    val incsleft = from.get.getIncludes
    val incsright = to.get.getIncludes
    val newto = getbottomth(to.get.path,ass.map(_._2))
    if (newto.isDefined) to = controller.get(newto.get) match {
      case t:Theory => Some(t)
      case _ => to // Should be impossible
    }
    if (nonlocal.nonEmpty) incsright.foreach(pth => controller.get(pth) match{
      case t:Theory =>
        val claims = nonlocal.filter(pair => canclaim(pth,pair._2))
        if (claims.nonEmpty) {
          val vset = Viewset.getNew(claims.toSet)
          val newfrom = getbottomth(from.get.path,claims.map(_._1)) map controller.get match {
            case Some(th:Theory) => Some(th)
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
      case t:Theory =>
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

  private def getConstants(v: View, from: Option[Theory], to: Option[Theory])(implicit controller: Controller) = {
    val dom = if (from.isDefined) from.get
    else controller.get(v.from.toMPath) match {
      case t: Theory => t
      case _ => throw new Exception("expected declared theory")
    }

    val cod = if (to.isDefined) to.get
    else controller.get(v.to.toMPath) match {
      case t: Theory => t
      case _ => throw new Exception("expected declared theory")
    }

    val dominc = dom.getIncludes.map(p => controller.getTheory(p))
    val codinc = cod.getIncludes.map(p => controller.getTheory(p))

    val domc = ((dominc diff codinc).flatMap(t => t.getDeclarations) collect { case c: FinalConstant => c }).toList
    val codc = ((codinc diff dominc).flatMap(t => t.getDeclarations) collect { case c: FinalConstant => c }).toList

    (domc, codc)
  }

  private def flatDomain(v:View)(implicit controller: Controller) : List[LocalName]
  = v.domain.toList:::v.getIncludes.flatMap(p => controller.get(p) match {
    case w:View => flatDomain(w)
    case _ => List()
  })

  def evaluateOnDom(v: View, from: Option[Theory], to: Option[Theory])(implicit controller: Controller): Double = {
    val (domc, codc) = getConstants(v, from, to)
    flatDomain(v).count(p => domc.exists(q => (ComplexStep(q.path.module) / q.name) == p)).toDouble / domc.length
  }

  def evaluateOnCod(v: View, from: Option[Theory], to: Option[Theory])(implicit controller: Controller): Double = {
    val (domc, codc) = getConstants(v, from, to)
    flatDomain(v).count(p => domc.exists(q => (ComplexStep(q.path.module) / q.name) == p)).toDouble / codc.length
  }

  def evaluateView(v: View, from: Option[Theory], to: Option[Theory])(implicit controller: Controller): Double = {
    val (domc, codc) = getConstants(v, from, to)
    flatDomain(v).count(p => domc.exists(q => (ComplexStep(q.path.module) / q.name) == p)).toDouble / (if (domc.length < codc.length) domc.length else codc.length).toDouble
  }

  def apply(v:View)(implicit controller : Controller) : Viewset = {
    val (dom,cod) = (controller.get(v.from.toMPath), controller.get(v.to.toMPath)) match {
      case p:(Theory,Theory) => p
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
      case w:View => w
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


/** Helper class for intersecter; basically wraps around FinalConstants, includes and structures */

sealed trait TheorySet {
  // all constants (recursively)
  val orig : List[FinalConstant]
  def allconsts: Set[(FinalConstant,List[GlobalName])] = localconsts ++ includes.flatMap(_.allconsts)

  var structures : List[Structure]
  // all constants locally declared (nonrecursively)
  var localconsts : Set[(FinalConstant,List[GlobalName])]
  // this can be given a path for the toView method
  private var path: Option[MPath] = None
  def setPath(p:MPath) {path = Some(p)}
  def getPath = path
  // direct theory inclusions
  var includes: List[TheorySet]
  private var metath : Option[MPath] = None
  def setMeta(p:MPath) {metath = Some(p); includes.foreach(_.setMeta(p))}
  def meta = metath
  // all theory inclusions recursively
  def allincludes: List[TheorySet] = (includes ::: includes.flatMap(_.allincludes)).distinct

  private def makesub : Set[(GlobalName,GlobalName)] = {
    require(path.isDefined)
    localconsts.flatMap(p => p._2.tail.map(n => (path.get ? p._2.head.name,n)))++includes.flatMap(_.makesub)
  }

  def toTheory(implicit controller: Controller) : List[Theory] = toTheory()._1

  private def toTheory(sub:Set[(GlobalName,GlobalName)] = Set())(implicit controller:Controller) : (List[Theory],Set[(GlobalName,GlobalName)]) = {
    require(path.isDefined)
    var subst = if (sub.isEmpty) makesub else sub

    val consts = localconsts map(c => if(c._1.name == c._2.head.name) c._1 else {
      val d = controller.get(c._2.head) match {
        case e: FinalConstant => e
        case _ => throw new Exception("Constant "+c._2.head+" doesn't exist!")
      }
      d
    })

    val th = new Theory(path.get.doc,path.get.name,metath)
    structures.foreach(s => th.add(s))

    val losts = orig.filter(c => !localconsts.exists(p => p._1==c))

    val newincs = includes.foldLeft((List().asInstanceOf[List[Theory]],subst)) ((nsub,inc) =>{
      var (incth,newsubst) = inc.toTheory(nsub._2)
      val theselosts = losts.map(c => (c,inc.allconsts.find(d => d._2.contains(c.path)))).collect{
        case (c,Some((d,l))) =>
          val sourceth = (inc::inc.allincludes).find(ts => ts.localconsts.contains((d,l))).get.getPath.get
          (c,incth.collectFirst{case t if t.declares(l.head.name) => t.get(l.head.name) match {
            case e:FinalConstant => e
            case _ => throw new Exception("Constant expected")
          }}.getOrElse(throw new Exception("Constant went missing: "+l.head.name+" in "+incth)))
      }

      def getName(name:LocalName, i:Int = 0) : LocalName = {
        val tryname = if (i==0) name else LocalName(name.toString+"_"+i)
        if (th.declares(tryname)) getName(name,i+1) else tryname
      }

      if (!theselosts.exists(p => p._1.name!=p._2.name || p._1.df.isDefined)) th add PlainInclude(incth.head.path,th.path) else {
        val s = Structure(th.toTerm,getName(incth.head.name),incth.head.toTerm,false)
        theselosts foreach (p => if(p._1.name!=p._2.name || p._1.df.isDefined) {
          val c = Constant(s.toTerm,ComplexStep(p._2.home.toMPath)/p._2.name,if(p._1.name!=p._2.name) List(p._1.name) else Nil,
            p._2.tp,p._1.df,None)
          s add c
          newsubst = newsubst.map(q => if(q._1==p._1.path) (c.path,q._2) else q)+((c.path,p._1.path))
        })
        th add s
      }
      (incth,newsubst)
    })
    Moduleadder(th,consts.toList,newincs._2.toList)
    controller.library.delete(th.path)
    controller.library.add(th)
    (th::newincs._1,newincs._2)
  }

  private def toStringIndent(ind:String) : String =
    ind+(if (path.isDefined) path.get.name.toString else "NoPath")+" : "+
      (if (metath.isDefined) metath.get.name.toString else "NoMeta")+
      includes.map(x => "\n"+x.toStringIndent(ind+"  ")).mkString("\n")+
      localconsts.map(p => "\n  "+ind+p._1.name+" : "+p._1.tp+" = "+p._1.df).mkString("")

  override def toString = toStringIndent("")
}

// Factory for TheorySets
object TheorySet {

  class TS(val consts:List[FinalConstant], val sub:List[TheorySet], val structs:List[Structure]) extends TheorySet {
    var localconsts = consts.map(c => (c,List(c.path))).toSet
    var includes = sub.distinct
    var structures = structs.distinct
    val orig = consts
  }
  var existing : Set[TS] = Set()

  def reset = existing = Set()

  def apply(th:Theory,metaasincludes : Boolean)(implicit controller: Controller) : TheorySet = {
    val consts = th.getConstants collect {case c: FinalConstant => c}
    val subs = if(metaasincludes) th.getIncludes map controller.get collect {case t: Theory => t} map(apply(_,metaasincludes))
    else th.getIncludesWithoutMeta map controller.get collect {case t: Theory => t} map(apply(_,metaasincludes))
    val ts = TheorySet(consts,subs,th.getNamedStructures,Some(th.path))
    th.meta foreach ts.setMeta
    ts
  }


  def apply(consts:List[FinalConstant],sub:List[TheorySet] = Nil, structs : List[Structure] = Nil, path:Option[MPath] = None)
  : TheorySet = {
    require(consts.nonEmpty || sub.nonEmpty || structs.nonEmpty)
    existing.find(ts => (path.isDefined && ts.getPath.isDefined && path.get==ts.getPath.get)||(ts.consts==consts && ts.sub==sub & ts.structs==structs)).getOrElse({
      val ts = new TS(consts,sub,structs)
      if (path.isDefined) ts.setPath(path.get)
      existing+=ts
      ts
    })
  }

}

/**
  * Takes a View/Theory T and a (Set of) (Pair(s) of) Constant(s) (either as FinalConstant of GlobalName) and
  * adds them to T consistent with the specifications (i.e. adapts the GlobalName accordingly and so on).
  * Also, if handed a theory and a Set of Constants, recursively substitutes occurences of the other members of the
  * set in types/definitions
  */
object Moduleadder {

  def apply(v:View,a:(GlobalName,GlobalName),ctrl:Controller):Boolean = {
    val pair = (ctrl.get(a._1), ctrl.get(a._2)) match {
      case (c1: FinalConstant, c2: FinalConstant) => Some(c1, c2)
      case _ => None
    }
    pair match {case None => false case Some(c) => apply(v, c)}
  }

  def apply(th: Theory,a:GlobalName,ctrl:Controller):Boolean = {
    val const = ctrl.get(a) match {
      case c: FinalConstant => Some(c)
      case _ => None
    }
    const match {case None => false case Some(c) => apply(th, c)}
  }

  def apply(v:View,a:(FinalConstant,FinalConstant)):Boolean = {
    v.add(new FinalConstant(
      OMID(v.path),
      ComplexStep(a._1.path.module) / a._1.name,
      Nil,
      TermContainer(a._1.tp),
      TermContainer(OMID(a._2.path)),
      None,
      NotationContainer(None)))
    true
  }

  def apply(th: Theory,a:FinalConstant):Boolean = {
    th.add(new FinalConstant(OMID(th.path), a.name, a.alias, TermContainer(a.tp), TermContainer(a.df), a.rl,
      NotationContainer(a.not)))
    true
  }

  def apply(v:View,list:List[(FinalConstant,FinalConstant)]):Boolean = {
    for (a <- orderp(list)) apply(v,a)
    true
  }

  def apply(th:Theory,set:List[FinalConstant],substs:List[(GlobalName,GlobalName)]):Boolean = {
    val list = set.toList
    val nsubsts = for {a <- list} yield (GlobalName(th.path,a.name),a.path)
    for (a <- order(list)) th.add(new FinalConstant(OMID(th.path), a.name, a.alias,
      TermContainer(substitute(a.tp,nsubsts:::substs)), TermContainer(substitute(a.df,nsubsts:::substs)), a.rl,
      NotationContainer(a.not)))
    true
  }

  def apply(th:Theory,list:List[FinalConstant]):Boolean = apply(th,list,List())

  def apply(v:View,list:List[(GlobalName,GlobalName)],ctrl:Controller):Boolean = {
    val pairs = for {a <- list} yield (ctrl.get(a._1), ctrl.get(a._2)) match {
      case (c1: FinalConstant, c2: FinalConstant) => Some(c1, c2)
      case _ => None
    }
    if (pairs.contains(None)) false
    else apply(v,pairs.map(_.get))
  }

  def apply(th:Theory,list:List[GlobalName],substs:List[(GlobalName,GlobalName)],ctrl:Controller):Boolean = {
    val consts = for {a <- list} yield ctrl.get(a) match {
      case c: FinalConstant => Some(c)
      case _ => None
    }
    if (consts.contains(None)) false
    else apply(th,consts.map(_.get),substs)
  }

  def apply(th:Theory,list:List[GlobalName],ctrl:Controller):Boolean = apply(th,list,List(),ctrl)

  /**
    * For every (t1,t2) in
    * @param substs , substitutes every occurence of t2 in
    * @param victim by t1.
    * @return the fully substituted Term.
    *         Needed for makeCodomain.
    */

  def substitute(victim: Term, substs: List[(GlobalName,GlobalName)]): Term = victim match {

    case c: OMV => c

    case c: OMID => substs collectFirst {case p if c.head.get == p._2 => p} match {
      case None => c
      case Some(x) => OMID(x._1)
    }

    case c: OMBINDC => OMBINDC(c.binder, c.context, for {y <- c.scopes} yield substitute(y,substs))

    case c: OMA => OMA(substs collectFirst {case p if c.head.get == p._2 => p} match {
      case None => c.fun
      case Some(x) => OMID(x._1)
    },for {y <- c.args} yield substitute(y,substs))

    case c: OMATTR => OMATTR(substitute(c.arg,substs),
      substs collectFirst {case p if c.head.get == p._2 => p} match {
        case None => c.key
        case Some(x) => OMID(x._1)
      },substitute(c.value,substs))

    case _ => victim
  }

  def substitute(victim: Option[Term],substs: List[(GlobalName,GlobalName)]): Option[Term] = victim match {
    case Some(t) => Some(substitute(t,substs))
    case None => None
  }

  def orderp(list:List[(FinalConstant,FinalConstant)]) : List[(FinalConstant,FinalConstant)] = {
    val newlist = orderh(list.map(b => Consthash(b._2,List(),None))).reverse
    newlist.map(p => list.collectFirst{case c if c._2.path==p.name => c}.get)
  }

  def order(list:List[FinalConstant]):List[FinalConstant] = {
    val newlist = orderh(list.map(b => Consthash(b,List(),None))).reverse
    newlist.map(p => list.collectFirst{case c if c.path==p.name => c}.get)
  }

  def orderh(list:List[Consthash]):List[Consthash] = {
    if(list.isEmpty || list.tail.isEmpty) list
    else if (!list.tail.exists(hash => SubtractDeclaration.occursIn(list.head.name,hash,list)))
      list.head::orderh(list.tail)
    else orderh(list.tail:::List(list.head))
  }

}