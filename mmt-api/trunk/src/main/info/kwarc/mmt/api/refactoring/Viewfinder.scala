package info.kwarc.mmt.api.refactoring
/*
import info.kwarc.mmt.api.frontend.{Logger, Controller}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.DeclaredTheory


abstract class Viewfinder extends Logger {
  implicit val controller : Controller
  lazy val logPrefix = "Viewfinder"
  lazy val report = controller.report

  protected var max = false
  protected var comp = false
  protected var length = 0
  protected var mod = false
  protected var nosubs = false

  def setParameters(maximalize:Boolean,compatiblize:Boolean,modularize:Boolean,onlymaximals:Boolean, minimalparamlenght:Int) = {
    max = maximalize
    comp = compatiblize
    length = minimalparamlenght
    mod = modularize
    nosubs = onlymaximals
  }
  /**
    * Main Method for the Viewfinder
    * @param allpairs All compatible pairs (based on hash equality)
    * @param startingpoints A list of pairs to use as starting points (e.g. axioms)
    * @return a list of Viewsets
    */

  protected def find(allpairs:List[(Consthash,Consthash)],startingpoints:List[(Consthash,Consthash)])
  : List[Viewset] = {

    def iterate(pairs:List[(Consthash,Consthash)],
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
              case Some(x) => iterate(pairs, x, path)
            }
          }
        )
      }
    }
    log("Finding morphisms...")
    val ls = startingpoints.indices.map(i => iterate(allpairs, startingpoints(i), List()).getOrElse(List()).toSet).toList
    ls.filter(i => i.nonEmpty).map(Viewset(_))
  }

  /**
    * Helper method, maximizing a partial morphism by adding all unique matches under an input morphism
    * @param viewset  the morphism as list of pairs of GlobalName
    * @param allpairs all pairings to consider (e.g. startingpoints can be left out, if makeCompatible is called later)
    * @return a new View as list of pairs of GlobalName
    */

  protected def makeMaximal(viewset:Viewset,allpairs:List[(Consthash,Consthash)])
  :Viewset = {
    def iterate(view: List[(GlobalName, GlobalName)], pairs: List[(Consthash, Consthash)]) : List[(GlobalName,GlobalName)]= {
      val matches1 = pairs.filter(a => a._1.matches(view)(a._2)) //for{a <- pairs if a._1.matches(view)(a._2)}yield a
      val imm_matches = matches1.filter(p => !matches1.exists(q => q != p && (q._1 == p._1 || p._2 == q._2)))
      if (imm_matches.isEmpty) view
      else {
        val newview = view ::: imm_matches.map(p => (p._1.name, p._2.name))
        iterate(newview, pairs.filter(p => !imm_matches.exists(q => p._1.name == q._1.name || p._2.name == q._2.name)))
      }
    }
    log("Maximizing morphisms...")
    Viewset(iterate(viewset.assignments,allpairs).toSet)
  }

  /**
    * Takes a list of partial morphisms and finds all maximally compatible unions of them
    * @param allviews A list of morphisms, represented as Viewsets
    * @return A List of new morphisms (as Viewsets)
    */
  protected def makeCompatible(allviews:List[Viewset])
  :List[Viewset] = {
    def Iterate(views:List[List[(GlobalName,GlobalName)]],currentView:List[(GlobalName,GlobalName)] = List())
    : List[List[(GlobalName,GlobalName)]] = {
      if (views.isEmpty) List(currentView)
      else if (views.tail.isEmpty) List(currentView ::: views.head)
      else {
        val newviews = views.tail.foldRight(Nil.asInstanceOf[List[List[(GlobalName, GlobalName)]]])((v, p) => {
          val newv = v diff views.head
          if (!newv.exists(x => views.head.exists(y => y._1 == x._1 || y._2 == x._2)) && newv.nonEmpty) newv :: p
          else p
        })
        Iterate(views.tail, currentView) ::: Iterate(newviews, currentView ::: views.head)
      }
    }
    log("Looking for maximally compatible unions of morphisms...")
    Iterate(allviews.map(_.assignments)).filter(x => x.nonEmpty).map(x => Viewset(x.toSet))
  }

  // Steps through all the methods depending on the parameters set
  protected def run(th1:DeclaredTheory,th2:DeclaredTheory,allpairs:Set[(Consthash,Consthash)],startingpoints:Set[(Consthash,Consthash)]) = {
    var paths = find(allpairs.toList,startingpoints.toList).distinct
    if (max) paths = paths.map(makeMaximal(_,(allpairs--startingpoints).toList)).distinct
    if (comp) paths = makeCompatible(paths).distinct
    paths foreach {p => p.setfromto(th1,th2)}
    if (nosubs) {
      log("Filtering out redundant morphisms...")
      paths = paths.indices.foldLeft(Nil.asInstanceOf[List[Viewset]])((l, i) =>
        if (paths.drop(i + 1).exists(q => paths(i).assignments.toSet subsetOf q.assignments.toSet)) l
        else paths(i) :: l)
    }
    if (mod) {
      log("Modularizing...")
      paths foreach (_.modularize)
    }
    paths = paths.filter(vs => vs.assignments.nonEmpty)
    log("Done. "+paths.length+" morphisms found.")
    paths
  }

  def apply(th1:DeclaredTheory, th2:DeclaredTheory) : List[Viewset]

  // sorts theories such that "larger" theories are in front (since finding morphisms starting from those subsumes
  // morphisms starting from smaller theories
  private def sortbyInclude(ths:List[DeclaredTheory]) :List[DeclaredTheory] = {
    if (ths.length<=1) return ths
    if (ths.tail.exists(t => t.getIncludes.contains(ths.head.path))) sortbyInclude(ths.tail:::List(ths.head))
    else ths.head::sortbyInclude(ths.tail)
  }

  def apply(ths:List[DeclaredTheory]) : List[Viewset] = {
    log("Sorting theories")
    val theories = sortbyInclude(ths)
    var thpairs = theories.indices.flatMap(i => theories.indices.drop(i+1).map(j => (i,j))).map(p => (theories(p._1),theories(p._2)))
    var results : List[Viewset] = Nil
    while (thpairs.nonEmpty) {
      log(" Current Pair: "+thpairs.head._1.path+" -> "+thpairs.head._2.path)
      val ret = apply(thpairs.head._1,thpairs.head._2).flatMap(vs => vs::vs.allincludes)
      results:::=ret
      thpairs = thpairs.tail
      thpairs = thpairs.filter(p => !ret.exists(vs => (p._1==vs.from.get && p._2 == vs.to.get) ||
        (p._2==vs.from.get && p._1==vs.to.get)))
    }
    results.distinct
  }

  def findBest(th1: DeclaredTheory,th2:DeclaredTheory) : Option[Viewset] = {
    log("Finding best morphism...")
    val vsl = apply(th1,th2)
    if (vsl.nonEmpty) Some(vsl.max(Ordering.by[Viewset,Double](p => p.evaluate))) else None
  }
}

/**
  * Axiomatic Viewfinder
  *
  * uses axioms as starting points (as determined by Consthash constructor).
  */

case class AxiomaticViewfinder(ctrl:Controller) extends Viewfinder {
  implicit val controller = ctrl

  def apply(th1:DeclaredTheory, th2:DeclaredTheory) : List[Viewset] = {
    log("Finding via axioms...")
    val ((hashes1,hashes2),_) = Consthash(th1,th2,controller)
    val pairs = for {h1 <- hashes1 ; h2 <- hashes2 if h1.hash==h2.hash} yield (h1,h2)
    val axpairs = pairs.filter(p => p._1.isAxiom && p._1.pars.length>=length)
    run(th1,th2,pairs,axpairs)
  }
}

case class FullViewfinder(ctrl:Controller) extends Viewfinder {
  implicit val controller = ctrl

  def apply(th1:DeclaredTheory, th2:DeclaredTheory) : List[Viewset] = {
    log("Finding all views...")
    val ((hashes1,hashes2),_) = Consthash(th1,th2,controller)
    val pairs = for {h1 <- hashes1 ; h2 <- hashes2 if h1.hash==h2.hash} yield (h1,h2)
    val axpairs = pairs.filter(p => p._1.pars.length>=length)
    run(th1,th2,pairs,axpairs)
  }
}

/*
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
*/
*/