package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.{Logger, Controller}

abstract class Intersecter extends Logger {
  implicit val controller: Controller
  val logPrefix = "Viewfinder"
  lazy val report = controller.report

  def intersect(vs:Viewset): (TheorySet,TheorySet) = {
    require(vs.from.isDefined && vs.to.isDefined)
    vs.distribute
    val metaasincludes = vs.from.get.meta!=vs.to.get.meta
    val from = TheorySet(vs.from.get,metaasincludes)
    val to = TheorySet(vs.to.get,metaasincludes)
    intersectOne(vs,from,to)
    (from,to)
  }

  private def intersectOne(vs:Viewset,from:TheorySet,to:TheorySet) {
    // println("top: "+vs)
    // println("from: "+from)
    // println("to: "+to)
    vs.includes.foreach(w => {
      // println("include:"+w)
      val newfrom = (from::from.allincludes).find(ts => ts.getPath.isDefined && w.from.isDefined && ts.getPath.get==w.from.get.path)
      val newto = (to::to.allincludes).find(ts => ts.getPath.isDefined && w.to.isDefined && ts.getPath.get==w.to.get.path)
      if(newfrom.isEmpty || newto.isEmpty) throw new Exception("Viewset or Theoryset has no path!")
      intersectOne(w,newfrom.get,newto.get)
    })
    if(vs.localassignments.isEmpty) return Nil
    val consts = vs.localassignments.map(pair => {
      val (c,d) = (from.localconsts.find(fc => fc._2.contains(pair._1)),to.localconsts.find(fc => fc._2.contains(pair._2)))
      if (c.isDefined && d.isDefined) {
        from.localconsts-=c.get
        to.localconsts-=d.get
        (c.get._1,(pair._1::pair._2::c.get._2:::d.get._2).distinct)
      } else throw new Exception("Constant not available in TheorySet")
    })
    if(from.localconsts.isEmpty && from.includes.forall(to.includes.contains)) {
      from.localconsts=consts.toSet
      to.includes = to.includes.filter(!from.includes.contains(_))
      to.includes::=from
    } else if (to.localconsts.isEmpty && to.includes.forall(from.includes.contains)) {
      to.localconsts=consts.map(c => (c._1,c._2 match {case x::y::l => y::x::l case _ => c._2})).toSet
      from.includes = from.includes.filter(!to.includes.contains(_))
      from.includes::=to
    } else if (consts.nonEmpty) {
      val ts = TheorySet(consts.map(_._1))
      ts.localconsts = consts.toSet
      ts.includes = from.includes.filter(to.includes.contains)
      from.includes = ts::from.includes.filter(!ts.includes.contains(_))
      to.includes = ts::to.includes.filter(!ts.includes.contains(_))
    }
  }

  /*
  def intersect(vs:Viewset, takefromCodomain:Boolean = false, intname:String ="", refact : List[TheorySet] = Nil) : List[TheorySet] = {
    require(vs.from.isDefined && vs.to.isDefined)

    vs.distribute

    val refactored = vs.includes.foldLeft(refact)((r,x) => intersect(x,takefromCodomain):::r)
    if (vs.localassignments.isEmpty) return refactored

    val metaasincludes = vs.from.get.meta!=vs.to.get.meta

    val from = TheorySet(vs.from.get,metaasincludes)
    val to = refactored.find(s => s.getPath.get==vs.to.get.path).getOrElse(TheorySet(vs.to.get,metaasincludes))
    from.includes = from.includes map (x => refactored.find(w => w.getPath == x.getPath) getOrElse x)
    to.includes = to.includes map (x => refactored.find(w => w.getPath == x.getPath) getOrElse x)

    val incs1 : Set[MPath] = from.allincludes.map(x => x.getPath.get).toSet
    val incs2 = to.allincludes.map(x => x.getPath.get).toSet

    if ((from.localconsts forall (c => vs.localassignments.exists(p => p._1==c.path))) &&
      (incs1 subsetOf incs2)) {
      var subst = vs.localassignments.filter(p => to.localconsts.exists(c => c.path==p._2))
      to.localconsts = to.localconsts.filter(c => !vs.localassignments.exists(p => p._2==c.path))

      val s = DeclaredStructure(vs.to.get.toTerm,from.getPath.get.name,vs.from.get.toTerm,false)
      vs.localassignments foreach (pair =>{
        val constOpt = to.localconsts.find(c => pair._2.name==c.name)
        if (pair._1.name!=pair._2.name || constOpt.exists(_.df.nonEmpty)) {
          s add Constant(s.toTerm,ComplexStep(from.getPath.get) / pair._1.name,if (pair._1.name!=pair._2.name) Some(pair._2.name) else None,
            constOpt.map(_.tp).getOrElse(None),constOpt.map(_.df).getOrElse(None),None)
        }
      })
      if (s.getDeclarations.isEmpty) to.includes = from::to.includes
      else {
        to.structures = s::to.structures
        subst = subst.map(pair => {
          val c = s.getDeclarations.find(d => d.name==pair._1.name)
          if (c.isDefined) (pair._1,c.get.path) else pair
        })
      }

      to.subst = subst
      List(from,to)

    } else if ((to.localconsts forall (c => vs.localassignments.exists(p => p._2==c.path))) &&
      (incs2 subsetOf incs1)) {
      var subst = vs.localassignments.filter(p => to.localconsts.exists(c => c.path==p._2)).map(p => (p._2,p._1))
      from.localconsts = from.localconsts.filter(c => !vs.localassignments.exists(p => p._1==c.path))

      val s = DeclaredStructure(vs.from.get.toTerm,to.getPath.get.name,vs.to.get.toTerm,false)
      vs.localassignments foreach (pair =>{
        val constOpt = from.localconsts.find(c => pair._1.name==c.name)
        if (pair._1.name!=pair._2.name || constOpt.exists(_.df.nonEmpty)) {
          s add Constant(s.toTerm,ComplexStep(to.getPath.get) / pair._2.name,if (pair._1.name!=pair._2.name) Some(pair._1.name) else None,
            constOpt.map(_.tp).getOrElse(None),constOpt.map(_.df).getOrElse(None),None)
        }
      })
      if (s.getDeclarations.isEmpty) from.includes = to::from.includes
      else {
        from.structures = s::from.structures
        subst = subst.map(pair => {
          val c = s.getDeclarations.find(d => d.name==pair._1.name)
          if (c.isDefined) (pair._1,c.get.path) else pair
        })
      }

      to.subst = subst
      List(from,to)
    } else {
      var subst1 : List[(GlobalName,GlobalName)] = Nil
      var subst2 : List[(GlobalName,GlobalName)] = Nil
      val path : MPath = from.getPath.get.doc ? LocalName(if (intname=="") vs.hashCode.toString else intname)
      val consts = vs.localassignments.map(pair => {
        val const = if (takefromCodomain) to.localconsts.find(c => c.path==pair._2) else
          from.localconsts.find(c => c.path==pair._1)
        if (takefromCodomain) {
          subst1::=(path?pair._2.name,pair._1)
          subst2::=(path?pair._2.name,pair._2)
        } else {
          subst1::=(path?pair._1.name,pair._1)
          subst2::=(path?pair._1.name,pair._2)
        }
        const.getOrElse(throw new Exception("Erroneous assignment in Viewset during intersecting"))
      })
      from.localconsts = from.localconsts.filter(p => !vs.localassignments.exists(pair => p.path==pair._1))
      to.localconsts = to.localconsts.filter(p => !vs.localassignments.exists(pair => p.path==pair._2))
      val includes = from.includes.toSet intersect to.includes.toSet
      val int = TheorySet(consts,includes.toList)
      if (from.meta.isDefined && from.meta==to.meta) int.setMeta(from.meta.get)
      int.setPath(path)
      from.includes = int::from.includes.filter(i => !includes.contains(i))
      to.includes = int::to.includes.filter(i => !includes.contains(i))
      from.subst=subst1
      to.subst=subst2
      List(int,from,to)
    }
  }
  */
}
/*
/**
 * Intersects theories
 */
case class Intersecter(controller:Controller) {
  def apply(th1:DeclaredTheory,th2:DeclaredTheory,pairs:List[(FinalConstant,FinalConstant,LocalName,Option[String])],intname:LocalName):List[DeclaredTheory] = {
    val int = new DeclaredTheory(th1.path.^^,intname,if (th1.meta==th2.meta) th1.meta else None)

    val includes = (int.meta match {
      case Some(x) => th1.getIncludesWithoutMeta.toSet diff th2.getIncludesWithoutMeta.toSet
      case None => th1.getIncludes.toSet diff th2.getIncludes.toSet
    }).toList

    for (o <- includes) int.add(PlainInclude(o,int.path))

    val namelist = pairs.map(p => (GlobalName(OMID(int.path),p._3),p._1.path))
    val consts = pairs map (c => {
      val tp = Moduleadder.substitute(c._1.tp,namelist)
      val df = None //TODO ?
      val rl = c._1.rl
      val notC = NotationContainer(c._4 match {
        case Some(n) => Some(TextNotation.parse(n,NamespaceMap.empty))
        case None => if(c._1.not.toString==c._2.not.toString) c._1.not else None
      })
      Constant(OMID(int.path),c._3,None,tp,df,rl,notC)
    })
    Moduleadder(int,consts.toSet)

    var newth1 = new DeclaredTheory(th1.path.^^,th1.name,th1.meta)
    var newth2 = new DeclaredTheory(th2.path.^^,th2.name,th2.meta)
    val simple1 = if (pairs.forall(p => p._3==p._1.name && p._1.df.isEmpty)) true else false
    val simple2 = if (pairs.forall(p => p._3==p._2.name && p._2.df.isEmpty)) true else false
    var subst1 = if(simple1) pairs.map(p => (int.get(p._3).path,p._1.path)) else List()
    var subst2 = if(simple2) pairs.map(p => (int.get(p._3).path,p._2.path)) else List()
    val s1 = if(simple1) PlainInclude(int.path,newth1.path) else {
      val s = new DeclaredStructure(OMID(newth1.path), int.name, TermContainer(OMID(int.path)), false)
        subst1 = pairs.map(c => (s.path / ComplexStep(int.path) / c._3,c._1.path))
      for (o <- pairs if o._1.name!=o._3 || o._1.df.isDefined) {
        val df = Moduleadder.substitute(o._1.df,subst1)
        s.add(Constant(OMID(s.path), ComplexStep(int.path) / o._3,
          if (o._1.name!=o._3) Some(o._1.name) else None, None, df, o._1.rl, o._1.notC))
      }
      s
    }
    val s2 = if(simple2) PlainInclude(int.path,newth2.path) else {
      val s = new DeclaredStructure(OMID(newth2.path), int.name, TermContainer(OMID(int.path)), false)
      subst2 = pairs.map(c => (s.path / ComplexStep(int.path) / c._3,c._2.path))
      for (o <- pairs if o._2.name!=o._3 || o._2.df.isDefined) {
        val df = Moduleadder.substitute(o._2.df,subst2)
        s.add(Constant(OMID(s.path), ComplexStep(int.path) / o._3,
          if (o._2.name!=o._3) Some(o._2.name) else None, None, df, o._2.rl, o._2.notC))
      }
      s
    }
    newth1.add(s1)
    newth2.add(s2)

    val th1consts = th1.getConstants.filter(p => !pairs.exists(q => q._1==p)) collect {case c:FinalConstant => c}
    val th2consts = th2.getConstants.filter(p => !pairs.exists(q => q._2==p)) collect {case c:FinalConstant => c}
    val th1includes = th1.getIncludesWithoutMeta.filter(p => !includes.contains(p))
    val th2includes = th2.getIncludesWithoutMeta.filter(p => !includes.contains(p))
    // TODO flatten includes in s?

    Moduleadder(newth1,th1consts.toSet,subst1)
    Moduleadder(newth2,th2consts.toSet,subst2)

    List(int,newth1,newth2)
  }

  def apply(th1:DeclaredTheory,th2:DeclaredTheory,intname:LocalName):List[DeclaredTheory] = {
    val viewfinder = new Viewfinder(controller)
    val view = viewfinder.findBest(th1,th2).getOrElse(return List())._1
    val pairs = Intersecter.getPairs(view,th1,th2).map(p => (p._1,p._2,p._1.name,None))
    apply(th1,th2,pairs,intname)
  }

  def apply(th1:DeclaredTheory,th2:DeclaredTheory,v:DeclaredView,intname:Option[String],takecod:Boolean = false):List[DeclaredTheory] = {
    require(v.from==th1.toTerm && v.to==th2.toTerm)

    val pairs = Intersecter.getPairs(v,th1,th2).map(p => (p._1,p._2,if (takecod) p._2.name else p._1.name,None))
    apply(th1,th2,pairs,if (intname.isDefined && intname.get!="") LocalName(intname.get) else LocalName("INTERSECTION"))
  }
}

object Intersecter {

  /**
   * Splits a view up in Pairs of FinalConstants ; takes only those, that are directly elements
   * of (dom x cod)
   * @param v
   * @param dom
   * @param cod
   * @return
   */

  def getPairs(v:DeclaredView,dom:DeclaredTheory,cod:DeclaredTheory) : List[(FinalConstant,FinalConstant)]= {

    val domconsts = (for {o <- v.getDeclarations if o.name.head.toString=="["+dom.path+"]"} yield o) collect {
      case c: FinalConstant if c.df.isDefined => c
    }

    (for {o <- domconsts} yield (o.name.tail,o.df.get)).filter(p =>
      p._2.head.get.module.toMPath==cod.path
    ).map(p => (dom.get(p._1),cod.get(p._2.head.get.name))) collect {
      case (c1:FinalConstant,c2:FinalConstant) => (c1,c2)
    }
  }

  /**
   * As above but takes TWO views
   * @param v1 : dom -> cod
   * @param v2 : cod -> dom and returns the paired declarations sorted by type:
   *           (constant -> constant)
   *           (constant -> term) and
   *           (term <- constant)
   * @return paired declarations as a triple of lists
   */

  def getPairs(v1:DeclaredView,v2:Option[DeclaredView],dom:DeclaredTheory,cod:DeclaredTheory)
  : (List[(FinalConstant,FinalConstant)],List[(FinalConstant,Term)],List[(Term,FinalConstant)]) = {

    val consts1 = ((for {o <- v1.getDeclarations if o.name.head.toString=="["+dom.path+"]"} yield o) collect {
      case c: FinalConstant if c.df.isDefined => c
    }).map(c => Try({
      val c1 = dom.get(c.name.tail)
      val c2 = if (c.df.get.head.get.module.toMPath==cod.path) cod.get(c.df.get.head.get.name) match {
        case d:FinalConstant => d
        case _ => c.df.get
      } else c.df.get
      (c1,c2)
    })) collect {case Success((a,b)) => (a,b)}

    val consts2 = v2 match {
      case Some(v) => ((for {o <- v.getDeclarations if o.name.head.toString == "[" + cod.path + "]"} yield o) collect {
        case c: FinalConstant if c.df.isDefined => c
      }).map(c => Try({
        val c1 = cod.get(c.name.tail)
        val c2 = if (c.df.get.head.get.module.toMPath == dom.path) dom.get(c.df.get.head.get.name) match {
          case d: FinalConstant => d
          case _ => c.df.get
        } else c.df.get
        (c2, c1)
      })) collect {case Success((a,b)) => (a,b)}
      case None => List()
    }
    val all = (consts1:::consts2).distinct
    for(i <- 0 to all.length-2 ; j <- i+1 to all.length-1) if (all(i)._1==all(j)._1 || all(i)._2==all(j)._2) return (Nil,Nil,Nil)
    (all collect {case (c:FinalConstant,d:FinalConstant) => (c,d)},
      all collect {case (c:FinalConstant,t:Term) => (c,t)},
      all collect {case (t:Term, c:FinalConstant) => (t,c)})
  }
}

*/