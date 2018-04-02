package info.kwarc.mmt.api.refactoring

import java.util.concurrent.Executors

import info.kwarc.mmt.api.frontend.{Logger, Report}
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology.FormalAlignment
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.utils.time.Time

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

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
  private var multithreaded : Boolean = false

  def fromTheories : List[DeclaredTheory] = fromTheories_var
  def toTheories : List[DeclaredTheory] = toTheories_var
  def commonTheories : List[DeclaredTheory] = commonTheories_var
  def fixing : List[AlignmentTranslation] = fixing_var
  def judg1: Option[GlobalName] = judg1_var
  def judg2: Option[GlobalName] = judg2_var
  def doDefs : Boolean = doDefs_var
  def minimal_parameter_length = minimal_parameter_length_var
  def isMultithreaded = multithreaded

  def setMultithreaded(b : Boolean) = multithreaded = b

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
  implicit private val ec = ExecutionContext.global //.fromExecutor(Executors.newFixedThreadPool(10000))

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

    // controller.simplifier.apply(th)
    // val ths = th.getIncludes.map(controller.get(_).asInstanceOf[DeclaredTheory])//closer.getIncludes(th,true)
    findJudgmentIt(th)
  }

  def getConfig : FinderConfig = new FinderConfig(this,report)

  def getConfig(a1 : Archive, a2 : Archive) : FinderConfig = {
    val cfg = getConfig
    var dones : mutable.HashMap[MPath,List[DeclaredTheory]] = mutable.HashMap.empty

    // guarantees that the dependency closure is ordered
    def flatten(mp : MPath) : List[DeclaredTheory] = {
      dones.getOrElse(mp, {
        log("Doing: " + mp.toString)
        // println(mp)
        val ret = controller.getO(mp) match {
          case Some(th : DeclaredTheory) =>
            (th.getIncludes.flatMap(flatten) ::: List(th)).distinct
          case _ => Nil
        }
        dones += ((mp,ret))
        ret
      })
    }

    var lf = a1.root / "viewfinder_order"
    if (lf.exists) {
      log("Reading file " + lf)
      val ls = File.read(lf).split("\n").toList.distinct
      val mps = ls.map(s => Path.parseM(s,NamespaceMap.empty))
      log("Adding " + mps.length + " theories...")
      cfg.addFrom(mps.indices.map{i =>
        print("\r  " + (i + 1) + " of " + mps.indices.length)
        controller.getAs(classOf[DeclaredTheory],mps(i))
      }.toList)
      println(" Done.")
    } else {
      log("Collecting theories in " + a1.id)
      val ths = a1.allContent.flatMap(flatten)
      File.write(lf,ths.map(_.path.toString).distinct.mkString("\n"))
      cfg.addFrom(ths)
      dones.clear()
    }

    lf = a2.root / "viewfinder_order"
    if (lf.exists) {
      log("Reading file " + lf)
      val ls = File.read(lf).split("\n").toList.distinct
      val mps = ls.map(s => Path.parseM(s,NamespaceMap.empty))
      log("Adding " + mps.length + " theories...")
      cfg.addTo(mps.indices.map{i =>
        print("\r  " + (i + 1) + " of " + mps.indices.length)
        controller.getAs(classOf[DeclaredTheory],mps(i))
      }.toList)
      println(" Done.")
    } else {
      log("Collecting theories in " + a2.id)
      val ths = a2.allContent.flatMap(flatten)
      File.write(lf,ths.map(_.path.toString).distinct.mkString("\n"))
      cfg.addTo(ths)
      dones.clear()
    }

    cfg
  }

/*
  def getConfigMultithreaded(a1: Archive, a2: Archive) : (Long,FinderConfig) = Time.measure {
    val cfg = getConfig

    object Dones {
      private var dones : mutable.HashMap[MPath,Future[List[DeclaredTheory]]] = mutable.HashMap.empty
      private def flattenIn(mp : MPath) : Future[List[DeclaredTheory]] = dones.get(mp) match {
        case None =>
          val f = Future {
            log("Doing: " + mp.toString)
            controller.getO(mp) match {
              case Some(th: DeclaredTheory) =>
                val rec = Future.sequence(th.getIncludes.map(flattenIn))
                rec.map { ls =>
                  val r = (ls.flatten ::: List(th)).distinct
                  log("DONE: " + mp.toString)
                  r
                }(ec)
              // Await.result(rec,Duration.Inf).flatten ::: List(th).distinct
              case _ => Future(Nil)
            }
          }(ec).flatten
          synchronized(dones += ((mp, f)))
          f
        case Some(f) =>
          f
      }

      def flatten(mp : MPath) : Unit = {
        flattenIn(mp)
      }


      private def finished = dones.values.forall(_.isCompleted)

      def getAll : List[DeclaredTheory] = {
        while(!finished) {
          // log("Waiting...")
          Thread.sleep(1000)
        }
        dones.values.flatMap(_.value.get.get).toList
      }
      def reset = dones.clear()
    }

    log("Collecting theories in " + a1.id)
    a1.allContent.foreach(Dones.flatten)
    cfg.addFrom(Dones.getAll)

    Dones.reset
    log("Collecting theories in " + a2.id)
    a2.allContent.foreach(Dones.flatten)
    cfg.addTo(Dones.getAll)

    cfg
  }
  */

  def run(cfg : FinderConfig) = {
    val proc = new FindingProcess(this.report,cfg)
    log("Multithreaded: " + cfg.isMultithreaded)
    proc.hash.run
    proc.run
  }

}

class FindingProcess(val report : Report, cfg : FinderConfig) extends MMTTask with Logger {
  override def logPrefix: String = "viewfinder"
  implicit private val ec = ExecutionContext.global //.fromExecutor(Executors.newFixedThreadPool(10000))

  import cfg._

  trait Hasher {
    // starts the hashing process
    protected def irun: Unit
    def run {
      log("Compute hashes... ")
      val (t,_) = Time.measure(irun)
      log("Hashing done after " + t + "ms.")
    }

    def from : List[Theoryhash]
    def to : List[Theoryhash]
  }

  val hash : Hasher = if (cfg.isMultithreaded) HashesMultithreaded else HashesNormal

  private object HashesNormal extends Hasher {
    private val theories : scala.collection.mutable.HashMap[MPath,Theoryhash] = mutable.HashMap()
    lazy val alltheories = commonTheories ::: fromTheories ::: toTheories

    // def getTheory(th : DeclaredTheory) : Theoryhash = theories.getOrElse(th.path,get(th))

    private def getTheory(p : MPath) : Theoryhash = theories.getOrElse(p, {
      get(alltheories.find(_.path == p).getOrElse{
        println(p)
        ???
      }) // TODO shouldn't happen though
    })

    private def get(th : DeclaredTheory) : Theoryhash = {
      val h = new Theoryhash(th.path)
      // log("Hashing " + th.path)
      if (!(commonTheories contains th)) {
        th.getConstants.collect({
          case c : FinalConstant
            if !cfg.fixing.exists(a => a.alignment.from.mmturi == c.path || a.alignment.to.mmturi == c.path) => c
        }) foreach (c =>
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

    // starts the hashing process
    def irun: Unit = {
      alltheories.indices.foreach(i => {
        print("\r  " + (i + 1) + " of " + alltheories.length)
        get(alltheories(i))
      })
      println(" Done.")
    }

    lazy val from = fromTheories.map(t => getTheory(t.path))
    lazy val to = toTheories.map(t => getTheory(t.path))
  }

  private object HashesMultithreaded extends Hasher {
    private val theories: scala.collection.mutable.HashMap[MPath, Future[Theoryhash]] = mutable.HashMap()

    // def getTheory(th : DeclaredTheory) : Theoryhash = theories.getOrElse(th.path,get(th))

    private def getTheory(p: MPath): Future[Theoryhash] = theories.getOrElse(p, {
      get((commonTheories ::: fromTheories ::: toTheories).find(_.path == p).getOrElse {
        println(p)
        ???
      }) // TODO shouldn't happen though
    })

    private def get(th: DeclaredTheory): Future[Theoryhash] = {
      val f = Future {
        // log("Hashing " + th.path)
        val h = new Theoryhash(th.path)
        if (!(commonTheories contains th)) {
          th.getConstants.collect({
            case c: FinalConstant
              if !cfg.fixing.exists(a => a.alignment.from.mmturi == c.path || a.alignment.to.mmturi == c.path) => c
          }) foreach (c =>
            /* if (!alignments.exists(a => a.from.mmturi == c.path || a.to.mmturi == c.path)) */ h.addConstant(doConstant(c))
            )
          val rec = Future.sequence(th.getIncludes.map(getTheory))
          rec.foreach { ls =>
            ls.foreach(h.addInclude)
            h.init
          }
        }
        h
      }
      synchronized(theories += ((th.path, f)))
      f
    }

    private lazy val numbers = synchronized {
      var inner: List[(GlobalName, Int)] = Nil
      commonTheories.foreach { th =>
        th.getConstants.foreach(c => inner ::= (c.path, inner.length + 1))
      }
      fixing.foreach { a =>
        a.alignment.to.mmturi match {
          case gn: GlobalName => (gn, inner.length + 1)
        }
      }
      inner
    }

    private def doConstant(c: FinalConstant): Consthash = {
      var isAxiom = false
      var pars: List[GlobalName] = Nil

      def traverse(t: Term)(implicit vars: List[LocalName]): List[Int] = {
        // assumption: at most one alignment applicable
        val al = fixing.find(_.applicable(t))
        val tm = al.map(_.apply(t)).getOrElse(t)
        tm match {
          case OMV(name) =>
            List(0, 2 * vars.indexOf(name))
          case OMS(path) =>
            if (judg1.contains(path) || judg2.contains(path)) isAxiom = true
            val nopt = numbers.find(_._1 == path)
            val (i, j) = nopt match {
              case Some((_, n)) => (0, n)
              case _ => (1, if (pars.contains(path)) pars.length - (pars.indexOf(path) + 1) else {
                pars ::= path
                pars.length - 1
              })
            }
            List(1, i, j)
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
          case OML(name, tp, df, _, _) =>
            6 :: tp.map(traverse).getOrElse(List(-1)) ::: df.map(traverse).getOrElse(List(-1))
          case tm =>
            println("Missing: " + tm.getClass)
            ???
        }
      }

      val hash = c.tp.map(traverse(_)(Nil)).getOrElse(Nil).asInstanceOf[List[Any]]
      val tppars = pars
      val optDef = if (doDefs) {
        pars = Nil
        c.df.map(traverse(_)(Nil))
      } else None
      Consthash(c.path, hash, tppars, isAxiom, optDef.map(d => (d.hashCode(), pars)))
    }

    // starts the hashing process
    def irun: Unit = {
      (commonTheories ::: fromTheories ::: toTheories).foreach(t => getTheory(t.path))
      while (!theories.values.forall(_.isCompleted)) {
        // log("Waiting...")
        Thread.sleep(500)
      }
    }

    lazy val from: List[Theoryhash] = fromTheories.map(t => getTheory(t.path).value.get.get)
    lazy val to: List[Theoryhash] = toTheories.map(t => getTheory(t.path).value.get.get)
  }


  def run = {
    log("Selecting Theories to use...")
    val (t1,tops1) = Time.measure(select(hash.from))
    val (t2,tops2) = Time.measure(select(hash.to))
    log("Done after " + (t1 + t2) + "ms")

    log(tops1.length + " and " + tops2.length + " selected elements")
    log("Finding Views...")
    val (t,alls : Set[FinderResult]) = Time.measure(if (cfg.isMultithreaded) {
      // One thread for each pair, should speed things up
      val ps = tops1.flatMap(t1 => tops2.map((t1,_)))
      val fs = Future.sequence(ps.map(p =>
        Future {
          // log("   Looking for: " + t1.path + " -> " + t2.path)
          val ret = findViews(p._1, p._2)
          // log("   " + t1.path.name + " -> " + t2.path.name + ": " + ret.size + " Results.")
          ret
        }(ec)
      ))
      while (!fs.isCompleted) {
        Thread.sleep(500)
      }
      fs.value.get.get.flatten.toSet
    } else {
      tops1.flatMap(t1 => tops2.flatMap { t2 =>
        // log("   Looking for: " + t1.path + " -> " + t2.path)
        val ret = findViews(t1, t2)
        // log("   " + t1.path.name + " -> " + t2.path.name + ": " + ret.size + " Results.")
        ret
      })
    }.toSet)
    log("Done after " + t + "ms")

    log("Postprocessing...")
    val (tlast,res) = Time.measure(postProcess(alls))
    log("Done after " + tlast + "ms")
    res
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

  // does something with the resulting lists of pairs.
  protected def postProcess (views: Set[FinderResult]) = {
    log("  Collecting pairs...")
    val consts = views.flatMap(_.entries).toList
    log("  Evaluating...")
    val evals = consts.indices.map {i =>
      val tr = consts(i)
      print("\r  " + (i+1) + " of " + consts.length)
      (tr._1,tr._2,Math.round(100.0 * views.count(_.contains(tr._1, tr._2)).toDouble / views.size.toDouble))
    }
    println(" Done.")
    List(FinderResult(hash.from.last,hash.to.last,evals.toSet,Set.empty))
  }
  /* TODO does not scale
protected def postProcess(views: Set[FinderResult]) : List[FinderResult] = {
  val vs = views.toList
  log("Evaluating...")
  val ev = if (cfg.isMultithreaded)
    vs.par.map(_.evaluate((p1,p2) => Math.round(100.0 * vs.count(_.contains(p1, p2)).toDouble / vs.length.toDouble))).toList
    else vs.map(_.evaluate((p1,p2) => Math.round(100.0 * vs.count(_.contains(p1, p2)).toDouble / vs.length.toDouble)))
  log("Flattening...")
  // var ret = views.map(_.evaluate((p1, p2) => Math.round(100.0 * views.count(_.contains(p1, p2)).toDouble / views.size.toDouble))).toList
  val dist = (if (cfg.isMultithreaded) {
    ev.par.flatMap(_.distribute)
  } else vs.indices.flatMap{v =>
    print("  " + (v+1) + "/" + ev.length)
    ev(v).distribute
  }).toList
  println("")

    val ret = dist

    log("Done - " + ret.size + " found")
    ret.sortWith((a, b) => a < b)
  }
  */

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
      // log("No potential matches")
      return Set()
    }
    // log("Potential matches: " + allpairs.length)

    val startingpoints = getStartingPoints
    if (startingpoints.isEmpty) {
      // log("No starting points")
      return Set()
    }
    // log("Starting Points: " + startingpoints.length)
    val ret = startingpoints.indices.map(i => iterate(allpairs, startingpoints(i), List()).getOrElse(Nil)).toSet - Nil
    ret.map(ls => FinderResult(makeMaximal(ls),th1,th2))
  }

}

case class FinderResult(from : Theoryhash,to : Theoryhash, entries : Set[(GlobalName,GlobalName,Long)], includes : Set[FinderResult]) {

  lazy val allentries : Set[(GlobalName,GlobalName,Long)] = entries ++ includes.flatMap(_.allentries)

  private def le(that : FinderResult) : Boolean = (that.from.getAllIncludes contains this.from) ||
    ((that.from == this.from) && (this.entries.size < that.entries.size))

  def <(that : FinderResult) : Boolean =
    (this le that) || ((!that.le(this)) && this.value > that.value)

  def value : Long =
    Math.round(allentries.toList.map(_._3).sum / allentries.toList.length.toDouble)

  def evaluate(eval : (GlobalName,GlobalName) => Long) : FinderResult =
    FinderResult(from,to,entries.map(tr => (tr._1,tr._2,eval(tr._1,tr._2))),includes.map(_.evaluate(eval)))

  def contains(p1 : GlobalName, p2 : GlobalName) : Boolean =
    allentries.exists(tr => tr._1 == p1 && tr._2 == p2)

  private def refactor : Set[FinderResult] = {
    val validtoincludes = to.getAllIncludes.filter(i => entries.forall(tr => i.getAll.exists(_.name == tr._2)))
    val newto = validtoincludes.find(th => !validtoincludes.exists(_ < th)).getOrElse(to)
    val locals = entries.filter(tr => tr._1.module == from.path)
    val rest = entries diff locals
    val recurses = from.getincludes.flatMap{ th =>
      FinderResult(th,newto,rest.filter(r => th.getAll.exists(_.name == r._1)),Set.empty).refactor
    }.toSet
    if (locals.nonEmpty) {
      val newthis = FinderResult(from,newto,locals,recurses)
      Set(newthis)
    } else recurses
  }

  private def allIncludes : Set[FinderResult] = includes ++ includes.flatMap(_.allIncludes)

  def distribute : Set[FinderResult] = {
    val rs = refactor
    rs ++ rs.flatMap(_.allIncludes)
  }

  def asString(s : String = "") : String = s + this.hashCode().toString + ": " + from.path + " --> " + to.path + " (" + value + ") {\n" +
    includes.map(_.asString(s + "  ")).mkString("\n") + "\n" +
    entries.map(tr => s + "  " + tr._1.module.name + "?" + tr._1.name + " -> " + tr._2.module.name + "?" + tr._2.name +
      " (" + tr._3 + ")").mkString("\n") + "\n" + s + "}"

  override def toString() = this.hashCode().toString + ": " + from.path + " --> " + to.path + " (" + value + ")" + "\n" +
    includes.map(i => "  include " + i.hashCode().toString).mkString("\n") + "\n" +
    entries.toList.sortBy(_._3).reverse.map(tr => "  " + tr._1.module.name + "?" + tr._1.name + " -> " + tr._2.module.name + "?" + tr._2.name +
     " (" + tr._3 + ")").mkString("\n")

  private def pairs : Set[(GlobalName,GlobalName)] = entries.map(tr => (tr._1,tr._2))

  override def equals(obj: scala.Any): Boolean = obj match {
    case that : FinderResult => this.from == that.from && this.to == that.to &&
      this.pairs == that.pairs
    case _ => false
  }
}

object FinderResult {
  def apply(viewset: List[(GlobalName, GlobalName)],from : Theoryhash, to : Theoryhash) : FinderResult = {
    FinderResult(from,to,viewset.map(p => (p._1,p._2,0.toLong)).toSet,Set.empty)
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
