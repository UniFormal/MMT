package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils.FilePath
import info.kwarc.mmt.api.{GlobalName, Path, frontend}

import scala.util.{Success, Try}

class AlignmentFinder extends frontend.Extension {

  var use_judgment = false
  var minimal_parameter_length = 0
  var judg1 : Option[GlobalName] = None
  var judg2 : Option[GlobalName] = None
  var allpairs : List[(Consthash,Consthash)] = Nil

  private def flattened(th : DeclaredTheory) : List[DeclaredTheory] = {
    Try(controller.simplifier(th))
    th :: th.getIncludes.map(p => Try(controller.get(p).asInstanceOf[DeclaredTheory])).collect{
      case Success(t) => t
    }.flatMap(flattened)
  }

  private def getJudgment(ths : List[DeclaredTheory]) = {
    var i = 0
    var judg : Option[GlobalName] = None
      while (judg.isEmpty && i < ths.length) {
        judg = AxiomHandler.findJudgment(controller, ths(i), None)
        i += 1
      }
    judg
  }

  def apply(a1 : FullArchive, a2 : FullArchive/*, fixing : List[FormalAlignment]*/) = {
    var fromTheories = a1.theories.map(controller.get).collect {
      case t : DeclaredTheory => t
    }
    var toTheories = a1.theories.map(controller.get).collect {
      case t : DeclaredTheory => t
    }

    if (use_judgment) {
      log("finding Judgments...")
      judg1 = getJudgment(fromTheories)
      judg2 = getJudgment(toTheories)
      log("Judgments are " + judg1 + " and " + judg2)
    }

    log("identify common theories...")
    val commonths = fromTheories.filter(toTheories.contains)
    fromTheories = fromTheories.filter(!commonths.contains(_))
    toTheories = toTheories.filter(!commonths.contains(_))
    //val alignments = fixing ::: commonths.flatMap(_.getConstants.map(c => Identity(c.path)))

    //log(alignments.length + " common or aligned symbol(pair)s")

    log("Compute hashes")
    val hasher = new Consthasher(controller,fromTheories,toTheories,a1,a2,commonths.map(_.path),/*alignments,*/(judg1,judg2),doDefs = false)
    val hashes1 = fromTheories.map(hasher.getTheory)
    val hashes2 = toTheories.map(hasher.getTheory)

    log("Selecting Theories to use...")
    val tops1 = select(hashes1)
    val tops2 = select(hashes2)

    log(tops1.length + " and " + tops2.length + " maximal elements")

    val alls = tops1.flatMap(t1 => tops2.flatMap(t2 => {
      log("Looking for: " + t1.path + " -> " + t2.path)
      val ret = logGroup{findViews(t1,t2)}
      log(ret.size + " Results.")
      ret
    }))
    log("Postprocessing starts...")
    val processed = logGroup {
      postProcess(alls.toSet)
    }
    log("Evaluate...")
    processed.flatten.map(p => (p,100 * processed.count(_.contains(p)).toDouble / processed.size.toDouble)).toList.sortBy(_._2)

  }

  private def select(theories : List[Theoryhash]) = theories.sortBy(_.getAllIncludes.length).indices.collect{
      case i if !theories.drop(i + 1).exists(p => p.getAllIncludes.contains(theories(i))) => theories(i)
    }

  // the method used for matching two constants. Defaults to hash equality with all parameters pairwise matched up
  private def matches(c : Consthash, d : Consthash)(l : List[(GlobalName,GlobalName)]) : Boolean = {
    if (c !<> d) false
    else (c.pars zip d.pars).forall {case (p,q) => p==q ||  l.contains((p,q))}
  }

  // finds views between two theories; returns a list of results as List of (GlobalName,GlobalName) pairs
  protected def findViews(th1 : Theoryhash, th2 : Theoryhash) : Set[List[(GlobalName,GlobalName)]] = {
    def iterate(pairs:List[(Consthash,Consthash)],
                current:(Consthash,Consthash),
                currentPath:List[(GlobalName,GlobalName)])
    : Option[List[(GlobalName,GlobalName)]] = {
      if (current._1 !<> current._2) None
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

    potentialMatches(th1.getAll,th2.getAll)
    if (allpairs.isEmpty) {
      log("No potential matches")
      return Set()
    }
    log("Potential matches: " + allpairs.length)

    /*
    println(allpairs.mkString("\n"))
    controller.handleLine("server on 8080")
    readLine()
    */

    val startingpoints = getStartingPoints
    if (startingpoints.isEmpty) {
      log("No starting points")
      return Set()
    }
    log("Starting Points: " + startingpoints.length)
    startingpoints.indices.map(i => iterate(allpairs, startingpoints(i), List()).getOrElse(Nil)).toSet - Nil

  }

  //selects potential matches to work with; defaults to hash-equality
  protected def potentialMatches(t1 : List[Consthash], t2 : List[Consthash]) : Unit
  = allpairs = t1.flatMap(c => t2 collect {case d if c <> d => (c,d)})

  // gets starting points for viewfinding. Defaults to Axioms and minimal_parameter_length
  protected def getStartingPoints : List[(Consthash,Consthash)]
  = (if(judg1.isDefined || judg2.isDefined) allpairs.filter(p =>  p._1.isProp || p._2.isProp)
    else allpairs).filter(p => p._1.pars.length >= minimal_parameter_length)

  // does something with the resulting lists of pairs
  protected def postProcess(views : Set[List[(GlobalName,GlobalName)]]) = {
    if (views.nonEmpty) {
      var ret = views
      logGroup {
        log("Maximizing " + ret.size + " morphisms...")
        ret = ret.map(makeMaximal)
      }
      /*
      logGroup {
        ret = makeCompatible(ret)
      }
      */
      log("Done - " + ret.size + " found")
      ret
    } else views
  }

  /**
    * Helper method, maximizing a partial morphism by adding all unique matches under an input morphism
    *
    * @param viewset  the morphism as list of pairs of GlobalName
    * @return a new View as list of pairs of GlobalName
    */

  protected def makeMaximal(viewset : List[(GlobalName,GlobalName)])
  : List[(GlobalName,GlobalName)] = {
    def iterate(view: List[(GlobalName, GlobalName)], pairs: List[(Consthash, Consthash)]) : List[(GlobalName,GlobalName)]= {
      val matches1 = pairs.filter(a => matches(a._1,a._2)(view)) //for{a <- pairs if a._1.matches(view)(a._2)}yield a
      val imm_matches = matches1.filter(p => !matches1.exists(q => q != p && (q._1 == p._1 || p._2 == q._2)))
      if (imm_matches.isEmpty) view
      else {
        val newview = view ++ imm_matches.map(p => (p._1.name, p._2.name))
        iterate(newview, pairs.filter(p => !imm_matches.exists(q => p._1.name == q._1.name || p._2.name == q._2.name)))
      }
    }
    iterate(viewset,allpairs)
  }

  /**
    * Takes a list of partial morphisms and finds all maximally compatible unions of them
    *
    * @param allviews A list of morphisms, represented as Viewsets
    * @return A List of new morphisms (as Viewsets)
    */
  protected def makeCompatible(allviews:Set[List[(GlobalName,GlobalName)]])
  :Set[List[(GlobalName,GlobalName)]] = {
    def Iterate(views:List[List[(GlobalName,GlobalName)]],currentView:List[(GlobalName,GlobalName)] = Nil)
    : List[List[(GlobalName,GlobalName)]] = {
      if (views.isEmpty) List(currentView)
      else if (views.tail.isEmpty) List(currentView ++ views.head)
      else {
        val newviews = views.tail.foldRight(Nil.asInstanceOf[List[List[(GlobalName, GlobalName)]]])((v, p) => {
          val newv = v diff views.head
          if (!newv.exists(x => views.head.exists(y => y._1 == x._1 || y._2 == x._2)) && newv.nonEmpty) newv :: p
          else p
        })
        Iterate(views.tail, currentView) ::: Iterate(newviews, currentView ::: views.head)
      }
    }
    log("Looking for maximally compatible unions in " + allviews.size + " morphisms...")
    Iterate(allviews.toList).filter(x => x.nonEmpty).toSet
  }

  /*
  var identify_commons = true
  var use_judgment = true
  var minimal_parameter_length = 0
  var identified : List[(GlobalName,GlobalName)] = Nil

  var judg1 : Option[GlobalName] = None
  var judg2 : Option[GlobalName] = None

  private var commons : List[GlobalName] = Nil
  private var commonths : List[DeclaredTheory] = Nil

  private var allpairs : List[(Consthash,Consthash)] = Nil

  // used to efficiently compute dependency closure and sorting them, by storing the results in var all
  object Closer {
    private var all : List[(DeclaredTheory,Set[DeclaredTheory])] = Nil

    def  apply(th:DeclaredTheory) : Set[DeclaredTheory] = all.collectFirst({
      case p if p._1==th => p._2
    }).getOrElse({
      run(th)
    })

    private def run(th: DeclaredTheory) : Set[DeclaredTheory] = {
      val res = th.getIncludes.flatMap(mp => {
        controller.get(mp) match {
          case t:DeclaredTheory => apply(t)+t
          case _ => throw new Exception("Not a DeclaredTheory: " + mp)
        }
      }).toSet
      all::=(th, res)
      res
    }

    def blowup(ths : List[DeclaredTheory]) : List[DeclaredTheory] =
      (ths:::ths.flatMap(apply)).distinct.sortBy(apply(_).size)

  }

  // forwards to applyths by controller.getting the theories
  def applypaths(lib1: List[MPath],lib2: List[MPath]): A = {
    val ths1 = lib1.map(p => Try(controller.get(p).asInstanceOf[DeclaredTheory])).collect{
      case Success(t:DeclaredTheory) => t}
    val ths2 = lib2.map(p => Try(controller.get(p).asInstanceOf[DeclaredTheory])).collect{
      case Success(t:DeclaredTheory) => t}
    applyths(ths1,ths2)
  }

  // Collects dependency closures, sorts all theories by inclusion, applies the consthasher and passes on the theoryhashes.
  // Optionally tries to find the judgment declarations in both sets and adds declarations in common theories
  // to the list of fixed symbols during hashing
  def applyths(lib1: List[DeclaredTheory], lib2 : List[DeclaredTheory]): A = {
    log("Collecting dependencies...")
    var ths1 = Closer.blowup(lib1)
    var ths2 = Closer.blowup(lib2)

    log("Theorylist 1: " + ths1.map(_.name).mkString(","))
    log("Theorylist 2: " + ths2.map(_.name).mkString(","))

    if(use_judgment) {

      var i = 0
      while (judg1.isEmpty && i < ths1.length) {
        judg1 = AxiomHandler.findJudgment(controller, ths1(i), None)
        i += 1
      }
      i = 0
      while (judg2.isEmpty && i < ths2.length) {
        judg2 = AxiomHandler.findJudgment(controller, ths2(i), None)
        i += 1
      }

      log(if (judg1.isDefined) "Judgement for theory list 1: " + judg1.get else "No judgements for theory list 1 found.")
      log(if (judg2.isDefined) "Judgement for theory list 2: " + judg2.get else "No judgements for theory list 2 found.")
    }
    // filter out theories in common and collect constants
    if (identify_commons) {
      log("identify common theories...")
      commonths = ths1.filter(ths2.contains)
      ths1 = ths1.filter(!commonths.contains(_))
      ths2 = ths2.filter(!commonths.contains(_))
      commons = commonths.flatMap(_.getConstants.map(_.path))
      log("Common theories: " + commonths.map(_.name).mkString(", "))
    }

    if (identified.nonEmpty) log("Assuming:\n   " + identified.map(p => p._1 + " = " + p._2).mkString("\n   "))

    selectTheories(
      ths1.map(t => hasher(t,(commons ::: identified.map(_._1),commonths.map(_.path)),judg1)),
      ths2.map(t => hasher(t,(commons ::: identified.map(_._2),commonths.map(_.path)),judg2))
    )
  }

  // Selects the theories to use for viewfinding, passes them on as pairs and returns the result to postprocess.
  // This default method picks the maximal ones wrt inclusion
  protected def selectTheories(lib1:List[Theoryhash],lib2:List[Theoryhash]): A = {
    log("Selecting Theories to use...")
    var tops1 : List[Theoryhash] = Nil
    var tops2 : List[Theoryhash] = Nil

    lib1.indices.foreach(i => if (!lib1.drop(i+1).exists(p => p.getAllIncludes.contains(lib1(i)))) tops1::=lib1(i))
    lib2.indices.foreach(i => if (!lib2.drop(i+1).exists(p => p.getAllIncludes.contains(lib2(i)))) tops2::=lib2(i))

    log("Maximal theories:\n   " + tops1.map(_.path.name) + "\n   " + tops2.map(_.path.name))

    postProcess(tops1.flatMap(t1 => tops2.flatMap(findViews(t1,_))).toSet)
  }

  // the method used for matching two constants. Defaults to hash equality with all parameters pairswise matched up
  def matches(c : Consthash, d : Consthash)(l : Iterable[(GlobalName,GlobalName)]) : Boolean = {
    if (c.hash!=d.hash) false
    else if (c.pars.length!=d.pars.length) false
    else (c.pars zip d.pars).forall {case (p,q) => p==q ||  l.exists(pair => pair._1.name == p && pair._2.name == q)}
  }

  // finds views between two theories; returns a list of results as List of (GlobalName,GlobalName) pairs
  protected def findViews(th1 : Theoryhash, th2 : Theoryhash) : Set[List[(GlobalName,GlobalName)]] = {
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

    log("Looking for matches: " + th1.path.name + " -> " + th2.path.name)

    potentialMatches(th1.getAll,th2.getAll)
    val startingpoints = getStartingPoints
    startingpoints.indices.map(i => iterate(allpairs, startingpoints(i), List()).getOrElse(Nil)).toSet - Nil

    /*
    log("Finding morphisms...")
    val ls = startingpoints.indices.map(i => iterate(allpairs, startingpoints(i), List()).getOrElse(List()).toSet).toList
    ls.filter(i => i.nonEmpty).map(Viewset(_))
    */
  }

  //selects potential matches to work with; defaults to hash-equality
  protected def potentialMatches(t1 : List[Consthash], t2 : List[Consthash]) : Unit
  = allpairs = t1.flatMap(c => t2 collect {case d if c.hash == d.hash => (c,d)})

  // gets starting points for viewfinding. Defaults to Axioms and minimal_parameter_length
  protected def getStartingPoints : List[(Consthash,Consthash)]
  = (if(judg1.isDefined) allpairs.filter(p =>  p._1.isProp)
  else if(judg2.isDefined) allpairs.filter(p =>  p._2.isProp) else
    allpairs).filter(p => p._1.pars.length >= minimal_parameter_length)

  // does something with the resulting lists of pairs
  protected def postProcess(views : Set[List[(GlobalName,GlobalName)]]) : A = {
    log("Results : " + views.mkString("\n   "))
    log("Postprocessing...")
    val ret = makeCompatible(views.map(makeMaximal))
    log("Done - " + ret.size + " found")
    finish(ret)
  }

  /**
    * Helper method, maximizing a partial morphism by adding all unique matches under an input morphism
    *
    * @param viewset  the morphism as list of pairs of GlobalName
    * @return a new View as list of pairs of GlobalName
    */

  protected def makeMaximal(viewset : List[(GlobalName,GlobalName)])
  : List[(GlobalName,GlobalName)] = {
    def iterate(view: List[(GlobalName, GlobalName)], pairs: List[(Consthash, Consthash)]) : List[(GlobalName,GlobalName)]= {
      val matches1 = pairs.filter(a => matches(a._1,a._2)(view)) //for{a <- pairs if a._1.matches(view)(a._2)}yield a
      val imm_matches = matches1.filter(p => !matches1.exists(q => q != p && (q._1 == p._1 || p._2 == q._2)))
      if (imm_matches.isEmpty) view
      else {
        val newview = view ++ imm_matches.map(p => (p._1.name, p._2.name))
        iterate(newview, pairs.filter(p => !imm_matches.exists(q => p._1.name == q._1.name || p._2.name == q._2.name)))
      }
    }
    log("Maximizing morphisms in\n   " + viewset.map(p => (p._1.name,p._2.name)))
    iterate(viewset,allpairs)
  }

  /**
    * Takes a list of partial morphisms and finds all maximally compatible unions of them
    *
    * @param allviews A list of morphisms, represented as Viewsets
    * @return A List of new morphisms (as Viewsets)
    */
  protected def makeCompatible(allviews:Set[List[(GlobalName,GlobalName)]])
  :Set[List[(GlobalName,GlobalName)]] = {
    def Iterate(views:List[List[(GlobalName,GlobalName)]],currentView:List[(GlobalName,GlobalName)] = Nil)
    : List[List[(GlobalName,GlobalName)]] = {
      if (views.isEmpty) List(currentView)
      else if (views.tail.isEmpty) List(currentView ++ views.head)
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
    Iterate(allviews.toList).filter(x => x.nonEmpty).toSet
  }

  def convert(l:List[(GlobalName,GlobalName)]) : DeclaredView = {

    ???
  }

  */

}