package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.notations.{TextNotation, NotationContainer}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.{ComplexStep, NamespaceMap, LocalName, GlobalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.symbols._

import scala.util.{Success, Try}

/**
 * Intersects theories
 */
case class Intersecter(controller:Controller) {
  def apply(th1:DeclaredTheory,th2:DeclaredTheory,pairs:List[(FinalConstant,FinalConstant,LocalName,Option[String])],intname:LocalName):List[DeclaredModule] = {
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

    val newth1 = new DeclaredTheory(th1.path.^^,th1.name,th1.meta)
    val newth2 = new DeclaredTheory(th2.path.^^,th2.name,th2.meta)
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

  def apply(th1:DeclaredTheory,th2:DeclaredTheory,intname:LocalName):List[DeclaredModule] = {
    val viewfinder = new Viewfinder(controller)
    val view = viewfinder.findBest(th1,th2).getOrElse(return List())._1
    val pairs = Intersecter.getPairs(view,th1,th2).map(p => (p._1,p._2,p._1.name,None))
    apply(th1,th2,pairs,intname)
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