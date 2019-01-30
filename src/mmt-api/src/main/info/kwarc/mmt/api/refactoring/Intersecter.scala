package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.symbols.FinalConstant

import scala.util.{Success, Try}

class Intersecter extends Extension {

  def apply(view : View) : (Theory, Theory, Theory) = {
    apply(controller.getTheory(view.from.toMPath), controller.getTheory(view.to.toMPath), view)
  }

  def apply(th1 : Theory, th2 : Theory, view : View) : (Theory, Theory, Theory) = {
    val th1p = Theory.empty(th1.parent, LocalName(th1.name.toString+"Mod"), th1.meta) // T1'
    val th2p = Theory.empty(th2.parent, LocalName(th2.name.toString+"Mod"), th2.meta) // T2'
    val sec = Theory.empty(th1.parent, LocalName(th1.name.toString+"Ctus"+th2.name.toString), th1.meta) // intersection between T1 and T2

    val renaming = ViewSplitter(view)(controller)

    val constSec = renaming.map(_._1)
    val structSec = view.getIncludes

    val constRem1 = th1.getConstants.filter(!renaming.map(_._1).contains(_)).map(_.asInstanceOf[FinalConstant])
    val structRem1 = th1.getIncludes ++ th1.getNamedStructures

    val constRem2 = th2.getConstants.filter(!renaming.map(_._2).contains(_)).map(_.asInstanceOf[FinalConstant])
    val structRem2 = th2.getIncludes ++ th2.getNamedStructures

    Moduleadder(sec, constSec)
    Moduleadder(th1p, constRem1)
    Moduleadder(th2p, constRem2)

    (sec, th1p, th2p)
  }
}

object ViewSplitter {

  def apply(v : View) ( implicit controller : Controller) : List[(FinalConstant,FinalConstant)] = {
    getPairs(v, controller.getTheory(v.from.toMPath), controller.getTheory(v.to.toMPath))
  }

  /**
    * Splits a view up in Pairs of FinalConstants ; takes only those, that are directly elements
    * of (dom x cod)
    * @param v
    * @param dom
    * @param cod
    * @return
    */

  def getPairs(v:View, dom:Theory, cod:Theory) : List[(FinalConstant,FinalConstant)]= {
    val domconsts = v.getDeclarations.filter(_.name.head.toString=="["+dom.name+"]") collect {
      case c: FinalConstant if c.df.isDefined => c
    }

    (for {o <- domconsts} yield (o.name.tail,o.df.get)).filter(p =>
      p._2.head.get.module==cod.path
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

  def getPairs(v1:View, v2:Option[View], dom:Theory, cod:Theory)
  : (List[(FinalConstant,FinalConstant)],List[(FinalConstant,Term)],List[(Term,FinalConstant)]) = {

    val consts1 = ((for {o <- v1.getDeclarations if o.name.head.toString=="["+dom.path+"]"} yield o) collect {
      case c: FinalConstant if c.df.isDefined => c
    }).map(c => Try({
      val c1 = dom.get(c.name.tail)
      val c2 = if (c.df.get.head.get.module==cod.path) cod.get(c.df.get.head.get.name) match {
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
        val c2 = if (c.df.get.head.get.module == dom.path) dom.get(c.df.get.head.get.name) match {
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