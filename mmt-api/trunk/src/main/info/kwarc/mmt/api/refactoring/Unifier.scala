package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.objects.OMID
import info.kwarc.mmt.api.{GlobalName, LocalName}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredView, DeclaredTheory}
import info.kwarc.mmt.api.ontology.IsView
import info.kwarc.mmt.api.symbols.{PlainInclude, FinalConstant}

import scala.util.{Success, Try}

/**
 * Takes two theories A,B and partial views A->C, B->C and yields (modulo renamings)
 * (AuB)nC=:C' and a new theory C*:=C\C' with C' as include
 */

object Unifier {
  /**
   * Takes all (ultimately) necessary parameters:
   * @param thA , thB and thC,
   * @param viewA a view thA->thC,
   * @param viewB a view thB->thC,
   * @param nameUnion an (optional) LocalName for the union C' (if not given: [thA.name]+[thB.name]) and
   * @param namenewC an (optional) LocalName for C*:=thC\C' (if not given: [thC.name]* )
   * @return C' and C*
   */
  def apply(thA:DeclaredTheory,thB:DeclaredTheory,thC:DeclaredTheory,viewA:DeclaredView,viewB:DeclaredView,nameUnion:Option[LocalName],
             namenewC:Option[LocalName])
    : List[DeclaredModule] = {

    val unionname = nameUnion match {
      case Some(x) => x
      case None => LocalName(thA.name.toString()+"+"+thB.name)
    }

    val newthCname = namenewC match {
      case Some(x) => x
      case None => LocalName(thC.name.toString()+"*")
    }

    val union = new DeclaredTheory(thC.path.doc,unionname,thC.meta)
    val newthC = new DeclaredTheory(thC.path.doc,newthCname,thC.meta)

    newthC.add(PlainInclude(union.path,newthC.path))
    val constA = Intersecter.getPairs(viewA,thA,thC)
    val constB = Intersecter.getPairs(viewB,thB,thC)

    val cod = (for {a <- constA:::constB} yield a._2).toSet
    Moduleadder(union,cod)
    val allnewconsts = (thC.getConstants collect {case t:FinalConstant => t}).toSet
    val newconsts = allnewconsts--cod
    val substs = (for {o <- cod} yield (GlobalName(OMID(union.path),o.name),o.path)).toList
    Moduleadder(newthC,newconsts,substs)
    for (o <- thC.getIncludesWithoutMeta) newthC.add(PlainInclude(o,newthC.path))

    union::newthC::List()
  }

  /**
   * Does the same as above, but only takes the theories
   * @param thA , thB and thC. Uses
   * @param ctrl to find the "most useful" views by either using the controller memory or, if
   * @param usedepstore is set to true, its depstore.
   */

  def apply(thA:DeclaredTheory,thB:DeclaredTheory,thC:DeclaredTheory,ctrl:Controller,usedepstore:Boolean,
            nameUnion:Option[LocalName], namenewC:Option[LocalName]): List[DeclaredModule] = {

    val allviews = if (usedepstore)
      ctrl.depstore.getInds(IsView).map(tp => Try(ctrl.get(tp))) collect { case Success(t : DeclaredView) => t }
    else ctrl.memory.content.getModules collect { case t : DeclaredView => t }

    val viewsA = allviews.filter(p => p.from.toMPath==thA.path && p.to.toMPath==thC.path)
    val viewsB = allviews.filter(p => p.from.toMPath==thB.path && p.to.toMPath==thC.path)
    val paired = (for {a <- viewsA ; b <- viewsB}
      yield (a,b,Viewfinder.evaluateView(a,Some(thA),Some(thB))(ctrl)*Viewfinder.evaluateView(b,Some(thB),Some(thA))(ctrl))).toList

    if (paired.isEmpty) List()
    else {
      val bestview = if (paired.tail.isEmpty) paired.head else
        paired.tail.foldLeft(paired.tail.head)((x,y) => if (x._3>y._3) x else y)
      if (bestview._3==1) List(thC,bestview._1,bestview._2)
      else apply(thA,thB,thC,paired.head._1,paired.head._2,nameUnion,namenewC)
    }

  }

  /**
   * Does the same as above, but only takes the theories
   * @param thA and thB. Tries to use
   * @param ctrl to find the best suited theory thC and the corresponding best views.
   */

  def apply(thA:DeclaredTheory,thB:DeclaredTheory,ctrl:Controller,usedepstore:Boolean, name:Option[LocalName],prefix:Option[String])
    : List[DeclaredModule] = {

    val allviews = if (usedepstore)
      ctrl.depstore.getInds(IsView).map(tp => Try(ctrl.get(tp))) collect { case Success(t : DeclaredView) => t }
    else ctrl.memory.content.getModules collect { case t : DeclaredView => t }

    val viewsA = allviews.filter(p => p.from.toMPath==thA.path)
    val viewsB = allviews.filter(p => p.from.toMPath==thB.path)
    val paired = (for {a <- viewsA ; b <- viewsB if a.to==b.to}
      yield (a,b,Viewfinder.evaluateView(a,Some(thA),None)(ctrl)*Viewfinder.evaluateView(b,Some(thB),None)(ctrl))).toList

    if (paired.isEmpty) List()
    else {
      val bestview = if (paired.tail.isEmpty) paired.head else
        paired.tail.foldLeft(paired.tail.head)((x,y) => if (x._3>y._3) x else y)
      val thC = ctrl.get(bestview._1.to.toMPath) match {
        case t: DeclaredTheory => t
        case _ => throw new Exception("DeclaredTheory expected!")
      }
      if (bestview._3==1) List(thC,bestview._1,bestview._2)
      else apply(thA,thB,thC,paired.head._1,paired.head._2,name,prefix match {
        case None => None
        case Some(s:String) => Some(LocalName(thC.name+s))
      })
    }

  }

}
