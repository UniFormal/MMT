package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.{MPath, LocalName, DPath, GlobalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.symbols._

/**
 * Intersects theories
 */
case class Intersecter(controller:Controller) {
  /**
   * Takes a view
   * @param v : th1 -> th2 and yields the corresponding theory intersection with path
   * @param intdoc (if given) and name
   * @param intname (if given; otherwise [v.parent]?INT_[v.name]) and refactored theories with names
   *                [th1.name][suffix], [th1.name][suffix] (if given; otherwise [thi.name]* )
   * @param suffix
   * @return a List of Modules; namely the intersection and the new theories th1*,th2*
   */
  def apply(v:DeclaredView, intdoc:Option[DPath],intname: Option[LocalName],
            suffix:Option[String]) : List[DeclaredModule] = {
    val dom = controller.get(v.from.toMPath) match {
      case t: DeclaredTheory => t
      case _ => throw new Exception("expected declared theory")
    }
    val cod = controller.get(v.to.toMPath) match {
      case t: DeclaredTheory => t
      case _ => throw new Exception("expected declared theory")
    }
    apply(v,dom,cod,intdoc,intname,suffix)
  }

  /**
   * Does the same thing as the above one, but takes as input two theories and intersects along the best view
   * between them.
   */
  def apply(th1:DeclaredTheory,th2:DeclaredTheory, intdoc:Option[DPath],
            intname:Option[LocalName],suffix:Option[String])
  : List[DeclaredModule] = {
    val viewOpt = new Viewfinder(controller).findBest(th1,th2)
    viewOpt match {
      case Some(view) => apply(view._1,th1,th2,intdoc,intname,suffix)
      case None => List()
    }
  }

  /**
   * Does the same thing as the above two, but already gets the view, domain and codomain as parameters
   * (all other apply methods forward to this one)
   */
  def apply(v:DeclaredView,dom:DeclaredTheory,cod:DeclaredTheory, intdoc:Option[DPath],
            intname:Option[LocalName],suffix:Option[String]) : List[DeclaredModule] = {
    val pairs = Intersecter.getPairs(v,dom,cod)
    val name = intname match {
      case Some(x) => x
      case None => LocalName("INT_"+v.name)
    }
    val doc = intdoc match {
      case Some(x) => x
      case None => v.parent
    }
    val newsuffix = suffix match {
      case Some(x) => x
      case None => "*"
    }
    val int = Intersecter.makeIntersection(pairs,doc,name,dom.meta)

    List(int,Intersecter.makeDomain(pairs,dom,newsuffix,int.path),Intersecter.makeCodomain(pairs,cod,newsuffix,int.path))
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
      case c: FinalConstant if (c.df match {case Some(t) => true case None => false}) => c
    }

    (for {o <- domconsts} yield (o.name.tail,o.df.get)).filter(p =>
      p._2.head.get.module.toMPath==cod.path
    ).map(p => (dom.get(p._1),cod.get(p._2.head.get.name))) collect {
      case (c1:FinalConstant,c2:FinalConstant) => (c1,c2)
    }

  }

  /**
   * Takes a list of pairs and creates the new domain with int as include
   * @param pairs
   * @param dom
   * @param suffix
   * @param int
   * @return
   */
  def makeDomain(pairs: List[(FinalConstant,FinalConstant)],dom:DeclaredTheory,suffix:String,int:MPath)
  :DeclaredTheory = {
    val newdom = new DeclaredTheory(dom.path.doc,LocalName(dom.name.toString()+suffix),dom.meta)
    newdom.add(PlainInclude(int,newdom.path))
    val consts = (dom.getConstants collect {case c:FinalConstant => c}).filter(p => !pairs.exists(q => q._1==p)).toSet
    val substs = for {o <- pairs} yield (GlobalName(OMID(int),o._1.name),o._1.path)
    Moduleadder(newdom,consts,substs)
    newdom
  }

  /**
   * Takes a list of pairs and creates the new codomain with int as include
   * @param pairs
   * @param cod
   * @param suffix
   * @param int
   * @return
   */
  def makeCodomain(pairs: List[(FinalConstant,FinalConstant)],cod:DeclaredTheory,suffix:String,int:MPath)
  :DeclaredTheory = {

    val newcod = new DeclaredTheory(cod.path.doc,LocalName(cod.name.toString()+suffix),cod.meta)
    newcod.add(PlainInclude(int,newcod.path))
    val consts = (cod.getConstants collect {case c:FinalConstant => c}).filter(p => !pairs.exists(q => q._2==p)).toSet
    val substs = for {o <- pairs} yield (GlobalName(OMID(int),o._1.name),o._2.path)
    Moduleadder(newcod,consts,substs)

    newcod
  }

  /**
   * Takes a list of Pairs (as generated above) and returns the intersection-theory
   * @param pairs
   * @param doc
   * @param name
   * @param meta
   * @return
   */

  def makeIntersection(pairs: List[(FinalConstant,FinalConstant)],doc:DPath,name:LocalName,meta:Option[MPath])
  : DeclaredTheory = {
    val int = new DeclaredTheory(doc,name,meta)
    val commons = pairs.map(_._1).toSet
    Moduleadder(int,commons)
    int
  }

}