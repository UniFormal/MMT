package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.{Report, Logger, Controller}
import info.kwarc.mmt.api.libraries.Closer
import info.kwarc.mmt.api.modules.{DeclaredTheory, DeclaredView}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{ComplexStep, SimpleStep, GlobalName, LocalName}

/**
 * Finds views between theories and provides several methods related to that
 */

case class Viewfinder(ctrl: Controller) extends Logger {
  implicit val controller = ctrl
  var report = controller.report
  val logPrefix = "Viewfinder"
  /**
   * Finds all Views (involving only constants occuring in axioms) between
   * @param th1 and
   * @param th2 .
   * @return a List of DeclaredView
   */

  def apply(th1:DeclaredTheory,th2:DeclaredTheory) : List[DeclaredView]
    = Viewfinder.settoViews(findByAxioms(th1,th2,0,true,true),th1,th2)

  /**
   * Tries to find the "best" (i.e. with largest evaluateView-Value) view between
   * @param th1 and
   * @param th2 .
   * @return a Pair consisting of the found view and its value
   */

  def findBest(th1:DeclaredTheory,th2:DeclaredTheory) : Option[(DeclaredView,Double)] = {
    log("Looking for best morphism from "+th1.path+" to "+th2.path)
    val allviews = for {o <- Viewfinder.settoViews(findByAxioms(th1,th2,0,true,true),th1,th2)} yield (o,Viewfinder.evaluateView(o,Some(th1),Some(th2))(controller))
    if (allviews.isEmpty) {log("No new views found!"); None}
      else if (allviews.tail.isEmpty) Some(allviews.head)
      else Some(allviews.tail.foldLeft(allviews.head)((x, v) => if (v._2 < x._2) x else v))
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

  def findByAxioms(th1:DeclaredTheory,th2:DeclaredTheory,cutoff:Int,makeComp:Boolean,maximize:Boolean)
   : Set[Set[(GlobalName,GlobalName)]] = {
    log("Finding by axioms from "+th1.name+" to "+th2.name)
    logGroup {
      log("Hashing...")
      val (allhashes,(judg1,judg2)) = Consthash(th1, th2, controller)
      log("Pairing...")
      val allpairs = for {a <- allhashes._1; b <- allhashes._2 if a.hash == b.hash} yield (a, b)
      if (judg1.isEmpty) log("No judgment declaration found! Continue with all (this can be slow)...")
      val pairedbyaxioms = if (judg1.isDefined) allpairs.filter(p => p._1.isAxiom && p._1.pars.length >= cutoff).toList
        else allpairs.filter(p => p._1.pars.length >= cutoff).toList
      //TODO just for testing purposes

      log("Finding Paths...")
      val allPaths = pairedbyaxioms.indices.map(i => findByAxiomsIterator(allpairs, pairedbyaxioms(i), List()).getOrElse(List()).distinct)

      val max = if (maximize) {
        log("Maximizing found sets...")
        allPaths.map(o => makeMaximal(o, allpairs, Some(pairedbyaxioms))).distinct
      }
      else allPaths
      if (makeComp) log("Finding maximally consistent unions...")

      val comp = (for {o <- if (makeComp) {
        makeCompatible(max.toList, List())
      } else max} yield o.toSet).toList.sortBy(_.size)
      log("Filtering out redundant subviews...")
      val res = comp.foldRight(comp)((b, s) =>
        if (!s.contains(b)) s
        else {
          val i = s.indexOf(b)
          s.take(i).filter(p => !p.subsetOf(b)) ::: s.drop(i)
        }
      ).toSet-Set()
      log("Done. "+res.size+" morphisms found!")
      res
    }
  }

  /**
   * Helper method, maximizing a view by adding all unique matches
   * @param view  the view as List of Pairs of GlobalName
   * @param pairs all possible pairings (based on hash equality)
   * @return a new View as Set of Pairs of GlobalName
   */
  def makeMaximal(view:List[(GlobalName,GlobalName)],pairs:Set[(Consthash,Consthash)],axpairs:Option[List[(Consthash,Consthash)]])
  :List[(GlobalName,GlobalName)] = makeMaximalIter(view,
    axpairs match {
      case None => pairs.filter(p => !p._1.isAxiom && !view.exists(q => p._1.name==q._1 || p._2.name==q._2))
      case Some(x) => (pairs diff x.toSet).filter(p => !view.exists(q => p._1.name==q._1 || p._2.name==q._2))
    })

  def makeMaximalIter(view:List[(GlobalName,GlobalName)],pairs:Set[(Consthash,Consthash)])
    :List[(GlobalName,GlobalName)] = {
      val matches1 = for{a <- pairs if a._1.matches(view)(a._2)}yield a
      val imm_matches = matches1.filter(p => !(matches1-p).exists(q => q._1==p._1 || p._2==q._2))
    if (imm_matches.isEmpty) view
    else {
      val newview = view:::imm_matches.map(p => (p._1.name,p._2.name)).toList
      makeMaximalIter(newview,pairs.filter(p => !imm_matches.exists(q => p._1.name==q._1.name || p._2.name==q._2.name)))
    }
  }

  /**
   * Takes a Set of partial Views and finds all maximally consistent subsets of it
   * @param views An IndexedSeq of Views, represented as Lists of Pairs of GlobalName
   * @param currentView All pairs already matched in the current Iteration (Startingvalue: List() )
   * @return A List of new Views (as Lists of GlobalName)
   */
  def makeCompatible(views:List[List[(GlobalName,GlobalName)]],
                     currentView:List[(GlobalName,GlobalName)])
    :List[List[(GlobalName,GlobalName)]] = {
    if (views.isEmpty) List(currentView) else if (views.tail.isEmpty) List(currentView:::views.head)
    else {
      /*
      val newtail = views.tail.map(v => v diff views.head)
      val newviews = for {v <- newtail
                          if !v.exists(p => views.head.exists(q => p._1==q._1 || p._2==q._2))} yield v */
      val newviews = views.tail.foldRight(Nil.asInstanceOf[List[List[(GlobalName,GlobalName)]]])((v,p) => {
        val newv = v diff views.head
        if (!newv.exists(x => views.head.exists(y => y._1==x._1 || y._2==x._2)) && newv.nonEmpty) newv::p
        else p
      })

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
  : Option[List[(GlobalName,GlobalName)]] = {
    if (current._1.pars.length!=current._2.pars.length) None
    else if (current._1.pars==current._2.pars) Some((current._1.name,current._2.name)::currentPath)
    else {
      current._1.pars.indices.foldLeft(
        Some((current._1.name,current._2.name)::currentPath).asInstanceOf[Option[List[(GlobalName,GlobalName)]]]
      )((pt,i) =>
        pt match {
          case None => None
          case Some(path) => if (path contains ((current._1.pars(i),current._2.pars(i)))) Some(path)
            else pairs.collectFirst{case p if p._1.name==current._1.pars(i) && p._2.name==current._2.pars(i) => p} match {
              case None => None
              case Some(x) => findByAxiomsIterator(pairs, x, path)
          }
        }
      )
    }
  }



}

object Viewfinder {

  /**
   * Takes a Set of views (as Set of pairs of GlobalName) and converts them to DeclaredViews
   * @param allrenamings input views
   * @param thA Domain of views
   * @param thB Codomain of views
   * @return a list of DeclaredView
   */

  def settoViews(allrenamings : Set[Set[(GlobalName,GlobalName)]],thA:DeclaredTheory,thB:DeclaredTheory)(implicit controller:Controller)
  :List[DeclaredView] = {
    val renamings = allrenamings.toList
    renamings.indices.map( i => {
      // TODO add metamorph?
      val v = new DeclaredView(thA.parent, LocalName(thA.name + "_TO_" + thB.name + "_" + i), OMID(thA.path), OMID(thB.path), false)
      Moduleadder(v, renamings(i), controller)
      v
    }).toList
  }

  /**
   * Evaluates a view: A -> B by checking what percentage of min{A,B} is matched by it.
   * @param v the input view
   * @return basically dom(view)/min{A,B}
   */

  def evaluateView(v:DeclaredView,from:Option[DeclaredTheory],to:Option[DeclaredTheory])(implicit controller: Controller): Double = {
    val closer = new Closer(controller)
    val dom = from match {
      case None => controller.get(v.from.toMPath) match {
        case t: DeclaredTheory => t
        case _ => throw new Exception("expected declared theory")
      }
      case Some(t) => t
    }

    val cod = to match {
      case None => controller.get (v.to.toMPath) match {
        case t: DeclaredTheory => t
        case _ => throw new Exception("expected declared theory")
      }
      case Some(t) => t
    }

    val dominc = closer.getIncludes(dom,true)
    val codinc = closer.getIncludes(cod,true)

    val domc = ((dominc diff codinc).flatMap(t => t.getDeclarations) collect {case c:FinalConstant => c}).toList
    val codc = ((codinc diff dominc).flatMap(t => t.getDeclarations) collect {case c:FinalConstant => c}).toList

    v.domain.filter(p => domc.exists(q => (ComplexStep(q.path.module.toMPath) / q.name)==p)).toList.length.toDouble / (if (domc.length<codc.length) domc.length else codc.length).toDouble
  }

  def evaluateViewset(from:DeclaredTheory,to:DeclaredTheory,viewset:Set[(GlobalName,GlobalName)])(controller:Controller): Double = {
    val closer = new Closer(controller)
    val dominc = closer.getIncludes(from,true)
    val codinc = closer.getIncludes(to,true)

    val domc = ((dominc diff codinc).flatMap(t => t.getConstants) collect {case c:FinalConstant => c}).toList
    val codc = ((codinc diff dominc).flatMap(t => t.getConstants) collect {case c:FinalConstant => c}).toList

    viewset.filter(p => domc.exists(q => q.path==p._1)).toList.length.toDouble / (if (domc.length<codc.length) domc.length else codc.length).toDouble

  }

  def viewtoviewset(v:DeclaredView)(implicit controller:Controller) : List[(FinalConstant,FinalConstant)] = {
    val from = controller.get(v.from.toMPath) match {
      case t:DeclaredTheory => t
      case _ => throw new Exception("DeclaredTheory expected!")
    }
    val to = controller.get(v.to.toMPath) match {
      case t:DeclaredTheory => t
      case _ => throw new Exception("DeclaredTheory expected!")
    }
    Intersecter.getPairs(v,from,to)
  }
}