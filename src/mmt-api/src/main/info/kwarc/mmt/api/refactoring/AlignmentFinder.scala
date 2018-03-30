package info.kwarc.mmt.api.refactoring

import info.kwarc.mmt.api.frontend.{Logger, Report}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.FormalAlignment

import scala.collection.mutable
import scala.concurrent.Future

class FinderConfig(finder : AlignmentFinder, protected val report : Report) extends Logger {
  override def logPrefix: String = "viewfinder"

  private var fromTheories_var : List[DeclaredTheory] = Nil
  private var toTheories_var : List[DeclaredTheory] = Nil
  private var commonTheories_var : List[DeclaredTheory] = Nil
  private var fixing_var : List[AlignmentTranslation] = Nil
  private var judg1_var: Option[GlobalName] = None
  private var judg2_var: Option[GlobalName] = None
  private var doDefs_var : Boolean = false
  private var minimal_parameter_length_var = 0

  def fromTheories : List[DeclaredTheory] = fromTheories_var
  def toTheories : List[DeclaredTheory] = toTheories_var
  def commonTheories : List[DeclaredTheory] = commonTheories_var
  def fixing : List[AlignmentTranslation] = fixing_var
  def judg1: Option[GlobalName] = judg1_var
  def judg2: Option[GlobalName] = judg2_var
  def doDefs : Boolean = doDefs_var
  def minimal_parameter_length = minimal_parameter_length_var

  def addFrom(th : List[DeclaredTheory]) = {
    fromTheories_var :::= th
    val cs = fromTheories_var.filter(t => toTheories_var.contains(t) || commonTheories_var.contains(t))
    commonTheories_var :::= cs
    fromTheories_var = fromTheories_var.filterNot(commonTheories_var.contains).distinct
    toTheories_var = toTheories_var.filterNot(commonTheories_var.contains).distinct
    commonTheories_var = commonTheories_var.distinct
  }
  def addTo(th : List[DeclaredTheory]) = {
    toTheories_var :::= th
    val cs = fromTheories_var.filter(t => toTheories_var.contains(t) || commonTheories_var.contains(t))
    commonTheories_var :::= cs
    fromTheories_var = fromTheories_var.filterNot(commonTheories_var.contains).distinct
    toTheories_var = toTheories_var.filterNot(commonTheories_var.contains).distinct
    commonTheories_var = commonTheories_var.distinct
  }
  private def addAlignments(al : List[FormalAlignment]) = {
    fixing_var :::= al.map(finder.makeAlignment)
  }

  // val translations = fixing.map(AlignmentTranslation.apply(_)(controller))

  def addJudgmentFrom(gn : GlobalName) = judg1_var = Some(gn)
  def addJudgmentTo(gn : GlobalName) = judg2_var = Some(gn)

  def setDoDefs(b : Boolean) = doDefs_var = b
  def setMinParLength(i : Int) = minimal_parameter_length_var = i

  def findJudgments = {
    log("finding Judgments...")
    val judg1 = if (judg1_var.isDefined) judg1_var else finder.getJudgment(fromTheories ::: commonTheories)
    val judg2 = if (judg2_var.isDefined) judg2_var else finder.getJudgment(toTheories ::: commonTheories)
    log("Judgments are " + judg1 + " and " + judg2)
    judg1.foreach(addJudgmentFrom)
    judg2.foreach(addJudgmentTo)
  }
}

class AlignmentFinder extends frontend.Extension {
  override def logPrefix: String = "viewfinder"

  private[refactoring] def getJudgment(ths: List[DeclaredTheory]): Option[GlobalName] = {
    var i = 0
    var judg: Option[GlobalName] = None
    while (judg.isEmpty && i < ths.length) {
      judg = findJudgment(ths(i))
      i += 1
    }
    judg
  }

  private[refactoring] def makeAlignment(al : FormalAlignment) =
    AlignmentTranslation(al)(controller)

  private def findJudgment(th:DeclaredTheory):Option[GlobalName] = {

    def findJudgmentIt(th:DeclaredTheory):Option[GlobalName] = {
      val list = for {o <- th.getConstants.filter(p => p.rl match {
        case t: Some[String] => true
        case _ => false
      }) if o.rl.get == "Judgment"} yield o match {
        case t: FinalConstant => t.path
        case _ => throw new Exception("FinalConstant Expected!")
      }
      list.headOption
    }

    controller.simplifier.apply(th)
    val ths = th.getIncludes.map(controller.get(_).asInstanceOf[DeclaredTheory])//closer.getIncludes(th,true)
    (ths map findJudgmentIt) collectFirst {case Some(x) => x}
  }

  def getConfig : FinderConfig = new FinderConfig(this,report)

  def getConfig(a1: Archive, a2: Archive) : FinderConfig = {
    val cfg = getConfig
    var collect : List[DeclaredTheory] = Nil

    // guarantees that the dependency closure is ordered
    def flatten(th : DeclaredTheory) : Unit = {
      collect ::= th
      th.getIncludes.map(controller.get).foreach{
        case t : DeclaredTheory => flatten(t)
        case _ =>
      }
    }
    a1.allContent.map(controller.get).foreach {
      case th : DeclaredTheory => flatten(th)
      case _ =>
    }
    cfg.addFrom(collect)

    collect = Nil
    a2.allContent.map(controller.get).foreach {
      case th : DeclaredTheory => flatten(th)
      case _ =>
    }
    cfg.addTo(collect)

    cfg
  }

  def run(cfg : FinderConfig) = {
    val proc = new FindingProcess(this.report,cfg)
    proc.hash
    proc.run
  }

  // TODO alignments

}

class FindingProcess(val report : Report, cfg : FinderConfig) extends MMTTask with Logger {
  override def logPrefix: String = "viewfinder"

  import cfg._

  private object Hashes {
    private var theories : scala.collection.mutable.HashMap[MPath,Theoryhash] = mutable.HashMap[MPath,Theoryhash]()

    def getTheory(th : DeclaredTheory) : Theoryhash = theories.getOrElse(th.path,get(th))

    private def getTheory(p : MPath) : Theoryhash = theories.getOrElse(p, {
      getTheory((commonTheories ::: fromTheories ::: toTheories).find(_.path == p).getOrElse(???)) // TODO shouldn't happen though
    })

    private def get(th : DeclaredTheory) : Theoryhash = {
      val h = new Theoryhash(th.path)
      if (!(commonTheories contains th)) {
        th.getConstants.collect({ case c : FinalConstant => c }) foreach (c =>
          /* if (!alignments.exists(a => a.from.mmturi == c.path || a.to.mmturi == c.path)) */ h.addConstant(doConstant(c))
          )
        th.getIncludes.foreach(t => h.addInclude(getTheory(t)))
      }
      h.init
      theories += ((th.path,h))
      h
    }

    private lazy val numbers = {
      var inner : List[(GlobalName,Int)] = Nil
      commonTheories.foreach { th =>
        th.getConstants.foreach(c => inner ::= (c.path, inner.length + 1))
      }
      fixing.foreach { a =>
        a.alignment.to.mmturi match {
          case gn : GlobalName => (gn,inner.length + 1)
        }
      }
      inner
    }

    private def doConstant(c:FinalConstant) : Consthash = {
      var isAxiom = false
      var pars : List[GlobalName] = Nil

      def traverse(t: Term)(implicit vars : List[LocalName]) : List[Int] = {
        // assumption: at most one alignment applicable
        val al = fixing.find(_.applicable(t))
        val tm = al.map(_.apply(t)).getOrElse(t)
        tm match {
          case OMV(name) =>
            List(0, 2 * vars.indexOf(name))
          case OMS(path) =>
            if (judg1.contains(path) || judg2.contains(path)) isAxiom = true
            val nopt = numbers.find(_._1 == path)
            val (i,j) = nopt match {
              case Some((_,n)) => (0,n)
              case _ => (1,if (pars.contains(path)) pars.length - (pars.indexOf(path)+1) else {
                pars ::= path
                pars.length-1
              })
            }
            List(1,i,j)
          case OMA(f, args) =>
            2 :: args.length :: traverse(f) ::: args.flatMap(traverse)
          case OMBINDC(f, con, bds) =>
            val (cont, newvars) = con.foldLeft((List(3, con.length), vars))((p, v) =>
              (p._1 ::: v.tp.map(traverse(_)(p._2)).getOrElse(List(-1)), v.name :: p._2))
            cont ::: List(bds.length) ::: bds.flatMap(traverse(_)(newvars))
          case OMLIT(value, rt) =>
            4 :: value.hashCode :: traverse(rt.synType)
          case UnknownOMLIT(s, tp) =>
            5 :: s.hashCode :: traverse(tp)
          case OML(name, tp, df, _,_) =>
            6 :: tp.map(traverse).getOrElse(List(-1)) ::: df.map(traverse).getOrElse(List(-1))
          case tm =>
            println("Missing: " + tm.getClass)
            ???
        }
      }
      val hash = c.tp.map(traverse(_)(Nil)).getOrElse(Nil).asInstanceOf[List[Any]]
      val tppars = pars
      val optDef = if(doDefs) {
        pars = Nil
        c.df.map(traverse(_)(Nil))
      } else None
      Consthash(c.path,hash,tppars,isAxiom,optDef.map(d => (d.hashCode(),pars)))
    }
  }

  // starts the hashing process and collects the theories on either "side" to use
  def hash: Unit = {
    log("Compute hashes")
    (commonTheories ::: fromTheories ::: toTheories).foreach(Hashes.getTheory)
  }

  def run = {
    log("Selecting Theories to use...")
    val tops1 = select(fromTheories.map(Hashes.getTheory))
    val tops2 = select(toTheories.map(Hashes.getTheory))

    log(tops1.length + " and " + tops2.length + " selected elements")

    // One thread for each pair, should speed things up
    var fs : List[Future[Set[FinderResult]]] = Nil

    tops1.foreach(t1 => tops2.foreach(t2 => {
      val f = Future {
        log("Looking for: " + t1.path + " -> " + t2.path)
        val ret = findViews(t1, t2)
        log(t1.path.name + " -> " + t2.path.name + ": " + ret.size + " Results.")
        ret
      }(scala.concurrent.ExecutionContext.global)
      fs ::= f
    }))
    while (!fs.forall(_.isCompleted)) {
      Thread.sleep(100)
    }
    val alls = fs.flatMap(_.value).collect {
      case scala.util.Success(v) => v
    }.flatten

    log("Postprocessing starts...")
    val processed = logGroup {
      postProcess(alls.toSet)
    }
    log("Evaluate...")
    processed.foreach(_.evaluate((p1,p2) => 100 * processed.count(_.contains(p1,p2)).toDouble / processed.size.toDouble))
    processed.toList.sortBy(_.value)
  }

  // selects which theories to use. By default only maximal theories
  private def select(theories: List[Theoryhash]) : List[Theoryhash] = theories.indices.collect {
    case i if !theories.drop(i + 1).exists(p => p.getAllIncludes.contains(theories(i))) => theories(i)
  }.toList

  // the method used for matching two constants. Defaults to hash equality with all parameters pairwise matched up
  protected def matches(c: Consthash, d: Consthash)(l: List[(GlobalName, GlobalName)]): Boolean = {
    if (c !<> d) false
    else (c.pars zip d.pars).forall { case (p, q) => p == q || l.contains((p, q)) }
  }

  //selects potential matches to work with; defaults to hash-equality
  protected def potentialMatches(t1: List[Consthash], t2: List[Consthash])
  = t1.flatMap(c => t2 collect { case d if c <> d => (c, d) })

  // gets starting points for viewfinding. Defaults to Axioms and minimal_parameter_length
  protected def getStartingPoints(implicit allpairs : List[(Consthash, Consthash)]): List[(Consthash, Consthash)]
  = (if (judg1.isDefined || judg2.isDefined) allpairs.filter(p => p._1.isProp || p._2.isProp)
  else allpairs).filter(p => p._1.pars.length >= minimal_parameter_length)

  // does something with the resulting lists of pairs. By default maximizes them
  protected def postProcess(views: Set[FinderResult]) = {
    if (views.nonEmpty) {
      var ret = views
      /*
      logGroup {
        log("Maximizing " + ret.size + " morphisms...")
      }
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
    * @param viewset the morphism as list of pairs of GlobalName
    * @return a new View as list of pairs of GlobalName
    */

  private def makeMaximal(viewset: List[(GlobalName, GlobalName)])(implicit allpairs : List[(Consthash, Consthash)])
  : List[(GlobalName, GlobalName)] = {
    def iterate(view: List[(GlobalName, GlobalName)], pairs: List[(Consthash, Consthash)]): List[(GlobalName, GlobalName)] = {
      val matches1 = pairs.filter(a => matches(a._1, a._2)(view)) //for{a <- pairs if a._1.matches(view)(a._2)}yield a
      val imm_matches = matches1.filter(p => !matches1.exists(q => q != p && (q._1 == p._1 || p._2 == q._2)))
      if (imm_matches.isEmpty) view
      else {
        val newview = view ++ imm_matches.map(p => (p._1.name, p._2.name))
        iterate(newview, pairs.filter(p => !imm_matches.exists(q => p._1.name == q._1.name || p._2.name == q._2.name)))
      }
    }

    iterate(viewset, allpairs)
  }

  // finds views between two theories; returns a list of results as List of (GlobalName,GlobalName) pairs
  protected def findViews(th1: Theoryhash, th2: Theoryhash): Set[FinderResult] = {
    def iterate(pairs: List[(Consthash, Consthash)],
                current: (Consthash, Consthash),
                currentPath: List[(GlobalName, GlobalName)])
    : Option[List[(GlobalName, GlobalName)]] = {
      if (current._1 !<> current._2) None
      else if (current._1.pars == current._2.pars) Some((current._1.name, current._2.name) :: currentPath)
      else {
        current._1.pars.indices.foldLeft(
          Some((current._1.name, current._2.name) :: currentPath).asInstanceOf[Option[List[(GlobalName, GlobalName)]]]
        )((pt, i) =>
          pt match {
            case None => None
            case Some(path) => if (path contains ((current._1.pars(i), current._2.pars(i)))) Some(path)
            else pairs.collectFirst { case p if p._1.name == current._1.pars(i) && p._2.name == current._2.pars(i) => p } match {
              case None => None
              case Some(x) => iterate(pairs, x, path)
            }
          }
        )
      }
    }

    implicit val allpairs : List[(Consthash,Consthash)] = potentialMatches(th1.getAll, th2.getAll)
    if (allpairs.isEmpty) {
      log("No potential matches")
      return Set()
    }
    log("Potential matches: " + allpairs.length)

    val startingpoints = getStartingPoints
    if (startingpoints.isEmpty) {
      log("No starting points")
      return Set()
    }
    log("Starting Points: " + startingpoints.length)
    val ret = startingpoints.indices.map(i => iterate(allpairs, startingpoints(i), List()).getOrElse(Nil)).toSet - Nil
    ret.map(ls => FinderResult(makeMaximal(ls),th1.path,th2.path))
  }

  /*
    /**
    * Takes a list of partial morphisms and finds all maximally compatible unions of them
    *
    * @param allviews A list of morphisms, represented as Viewsets
    * @return A List of new morphisms (as Viewsets)
    */
  private def makeCompatible(allviews: Set[List[(GlobalName, GlobalName)]])
  : Set[List[(GlobalName, GlobalName)]] = {
    def Iterate(views: List[List[(GlobalName, GlobalName)]], currentView: List[(GlobalName, GlobalName)] = Nil)
    : List[List[(GlobalName, GlobalName)]] = {
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
   */
}

class FinderResult(from : MPath, to : MPath) {
  private var entries : List[(GlobalName,GlobalName,Double)] = Nil

  def value : Double = entries.map(_._3).sum / entries.length.toDouble

  def evaluate(eval : (GlobalName,GlobalName) => Double) : Unit =
    entries = entries.map(tr => (tr._1,tr._2,eval(tr._1,tr._2)))

  def contains(p1 : GlobalName, p2 : GlobalName) : Boolean =
    entries.exists(tr => tr._1 == p1 && tr._2 == p2)

  override def toString() = "Result: " + from + " --> " + to + " (" + value + ")" + "\n" +
    entries.map(tr => "  " + tr._1.module.name + "?" + tr._1.name + " -> " + tr._2.module.name + "?" + tr._2.name).mkString("\n")
}

object FinderResult {
  def apply(viewset: List[(GlobalName, GlobalName)],from : MPath, to : MPath) : FinderResult = {
    val ret = new FinderResult(from,to)
    ret.entries = viewset.map(p => (p._1,p._2,0.0))
    ret
  }
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
