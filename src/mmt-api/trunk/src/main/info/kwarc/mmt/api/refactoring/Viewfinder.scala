package refactoring

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{GlobalName, LocalName}

/**
 * Finds views between theories and provides several methods related to that
 */

object Viewfinder {

  /**
   * Finds all Views (involving only constants occuring in axioms) between
   * @param th1 and
   * @param th2 .
   * @return a List of DeclaredView
   */

  def apply(th1:DeclaredTheory,th2:DeclaredTheory,ctrl:Controller) : List[DeclaredView]
    = settoViews(findByAxioms(th1,th2,ctrl,0,true,true),th1,th2,ctrl)

  /**
   * Tries to find the "best" (i.e. with largest evaluateView-Value) view between
   * @param th1 and
   * @param th2 .
   * @return a Pair consisting of the found view and its value
   */

  def findBest(ctrl:Controller,th1:DeclaredTheory,th2:DeclaredTheory) : (DeclaredView,Double) = {
    val allviews = for {o <- settoViews(findByAxioms(th1,th2,ctrl,0,true,true),th1,th2,ctrl)} yield (o,evaluateView(ctrl,o))
    if (allviews.isEmpty) throw new Exception("No new views found!")
    else if (allviews.tail.isEmpty) allviews.head
    else allviews.tail.foldLeft(allviews.head)((x,v) => if (v._2<x._2) x else v)
  }

  /**
   * Finds all possible Views (involving only constants occuring in specified axioms) between
   * @param th1 and
   * @param th2 using
   * @param cutoff an Int value such, that only axioms using at least cutoff many constants unique to
   *               th1 will be considered.
   * @param makeComp If true, will combine the found views such that they are maximally consistent
   * @param maximize If true, will add all other uniquely matchable declarations not occuring in the considered axiom
   * @return A set of views, each represented as a set of pairs of GlobalName
   */

  def findByAxioms(th1:DeclaredTheory,th2:DeclaredTheory,ctrl:Controller,cutoff:Int,makeComp:Boolean,maximize:Boolean)
   : Set[Set[(GlobalName,GlobalName)]] = {

    val allhashes = Consthash(th1,th2,ctrl)
    val allpairs = for {a <- allhashes._1; b <- allhashes._2 if a.hash==b.hash} yield (a,b)

    val pairedbyaxioms = allpairs.filter(p => p._1.isAxiom && p._1.pars.length>=cutoff).toIndexedSeq

    val allPaths = for {i <- 0 to pairedbyaxioms.length-1} yield findByAxiomsIterator(allpairs,pairedbyaxioms(i),List()).distinct
    val max = for {o <- allPaths} yield if(maximize) makeMaximal(o,allpairs).toList else o
    val comp = (for {o <- if (makeComp) makeCompatible(max,List()) else max} yield o.toSet).toSet

    comp.filter(p => !(comp-p).exists(q => p subsetOf q))
  }

  /**
   * Helper method, maximizing a view by adding all unique matches
   * @param view  the view as List of Pairs of GlobalName
   * @param pairs all possible pairings (based on hash equality)
   * @return a new View as Set of Pairs of GlobalName
   */

  def makeMaximal(view:List[(GlobalName,GlobalName)],pairs:Set[(Consthash,Consthash)])
    :Set[(GlobalName,GlobalName)] = {
      val newpairs = pairs.filter(p => !view.exists(q => p._1.name==q._1 || p._2.name==q._2))
      val matches1 = for{a <- newpairs if a._1.matches(view)(a._2)}yield a
      val imm_matches = matches1.filter(p => !(matches1-p).exists(q => q._1==p._1 || p._2==q._2))
      view.toSet++(for {a <- imm_matches} yield (a._1.name,a._2.name))
  }

  /**
   * Takes a Set of partial Views and finds all maximally consistent subsets of it
   * @param views An IndexedSeq of Views, represented as Lists of Pairs of GlobalName
   * @param currentView All pairs already matched in the current Iteration (Startingvalue: List() )
   * @return A List of new Views (as Lists of GlobalName)
   */
  def makeCompatible(views:IndexedSeq[List[(GlobalName,GlobalName)]],
                     currentView:List[(GlobalName,GlobalName)])
    :List[List[(GlobalName,GlobalName)]] = {
    if (views.isEmpty) List(currentView) else {
      val newviews = for {v <- views.tail if !v.exists(p => views.head.exists(q => p._1==q._1 || p._2==q._2))} yield v
      makeCompatible(views.tail,currentView):::makeCompatible(newviews,currentView:::views.head)
    }
  }

  /**
   * Helper Method for the Viewfinder
   * @param pairs All possible pairings based on hash equality
   * @param current the current pair of constants that have to/should be matched
   * @param currentPath all previously found matches in the current iteration
   * @return a consistent list of matches
   */

  def findByAxiomsIterator(pairs:Set[(Consthash,Consthash)],
                         current:(Consthash,Consthash),
                         currentPath:List[(GlobalName,GlobalName)])
  : List[(GlobalName,GlobalName)] = {
    if (current._1.pars.length!=current._2.pars.length) List()
    else if (current._1.matches(currentPath)(current._2)) (current._1.name,current._2.name)::currentPath
    else {
      val parsmatch = for {i <- 0 to current._1.pars.length-1} yield {
        pairs.collectFirst{case p if p._1.name==current._1.pars(i) && p._2.name==current._2.pars(i) => p} match {
          case None => List()
          case Some(x) => findByAxiomsIterator(pairs, x, (current._1.name, current._2.name) :: currentPath)
        }
      }
      if (parsmatch.contains(List())) List() else
        parsmatch.foldLeft(List().asInstanceOf[List[(GlobalName,GlobalName)]])((b,l) => b:::l)
    }
  }

  /**
   * Takes a Set of views (as Set of pairs of GlobalName) and converts them to DeclaredViews
   * @param allrenamings input views
   * @param thA Domain of views
   * @param thB Codomain of views
   * @param ctrl
   * @return a list of DeclaredView
   */

  def settoViews(allrenamings : Set[Set[(GlobalName,GlobalName)]],thA:DeclaredTheory,thB:DeclaredTheory,ctrl:Controller)
  :List[DeclaredView] = {
    val renamings = allrenamings.toList
    (for {i <- 0 to renamings.length-1} yield {
      val v = new DeclaredView(thA.parent, LocalName(thA.name + "_TO_" + thB.name + "_" + i), OMID(thA.path), OMID(thB.path), false)
      Moduleadder(v,renamings(i),ctrl)
      v
    }).toList
  }

  /**
   * Evaluates a view: A -> B by checking what percentage of min{A,B} is matched by it.
   * @param ctrl
   * @param v the input view
   * @return basically dom(view)/min{A,B}
   */

  def evaluateView(ctrl:Controller,v:DeclaredView): Double = {
    val domc = (ctrl.get(v.from.toMPath) match {
      case t: DeclaredTheory => t
      case _ => throw new Exception("expected declared theory")
    }).getConstants collect { case c: FinalConstant => c}

    val codc = (ctrl.get(v.to.toMPath) match {
      case t: DeclaredTheory => t
      case _ => throw new Exception("expected declared theory")
    }).getConstants collect { case c: FinalConstant => c}

    v.domain.filter(p => domc.exists(q => LocalName("["+q.path.module+"]/"+q.name)==p)).toList.length.toDouble / (if (domc.length<codc.length) domc.length else codc.length).toDouble

  }

}

